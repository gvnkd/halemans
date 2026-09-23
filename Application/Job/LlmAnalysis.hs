module Application.Job.LlmAnalysis where

import Application.Helper.Ingest (publishAlertUpdate)
import Application.Service.I18n (agentLanguageName)
import Application.Service.Llm
import qualified Application.Service.Llm.Budget as Budget
import Application.Service.Llm.DbConfig (currentLlmConfig)
import Application.Service.Llm.Output (ParsedOutput (..), parseCompletionOutput)
import Application.Service.Llm.Prompt (BuiltPrompt (..), buildPromptForAlert)
import Application.Service.Llm.Roles (resolveAgentRole, templateNameForRole, toolsForRole)
import Application.Service.Llm.Tools (executeToolCall, runWithToolLoop, toolDefinitions)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)

-- LLM enrichment job (design_docs/milestone_4.md §4/§6). One-shot per
-- llm_analyses row; soft-fails only (D8) — the analysis result never feeds
-- pipeline actions, notifications, or external calls.
--
-- Flow: queued -> gates (dedupe -> budget -> rate) -> prompt build (already
-- done at this point for hashing) -> provider call (with optional read-only
-- tool loop) -> store markdown + jsonb + usage -> done. Terminal states
-- publish kind "enriched" on halemans_events so an open card live-updates.
instance Job LlmAnalysisJob where
    perform job = do
        analysis <- fetch job.analysisId
        -- "running" is reachable here only via IHP stale-job recovery: the
        -- worker that set it died mid-analysis (locked_at older than
        -- staleJobTimeout), so no live process will finish the row. A
        -- "queued"-only guard would no-op the recovered job and orphan the
        -- analysis in "running" forever; re-running from the top is the
        -- correct continuation (worst case a duplicate provider call, the
        -- same risk class as any stale-recovered job).
        when (analysis.status `elem` ["queued", "running"]) do
            alert <- fetch analysis.alertId
            maybeConfig <- currentLlmConfig
            case maybeConfig of
                Nothing -> failAnalysis analysis alert "llm_not_configured" "llm_skipped"
                Just config -> do
                    result <- try @SomeException (runAnalysis job analysis alert config)
                    case result of
                        Right () -> pure ()
                        Left err -> do
                            void (try @SomeException (failAnalysis analysis alert ("internal error: " <> tshow err) "llm_failed"))
                            throwIO err

    maxAttempts = 3

    -- 10s poll (default 60s): analyses should land on open cards promptly,
    -- and backoff-requeued retries must not wait a full minute each.
    queuePollInterval = 10 * 1000000

runAnalysis :: (?modelContext :: ModelContext) => LlmAnalysisJob -> LlmAnalysis -> Alert -> LlmProviderConfig -> IO ()
runAnalysis job analysis alert config = do
    -- Agent role (milestone_8.md §7): explicit on the analysis row, else the
    -- is_default role, else legacy behaviour (no role).
    role <- resolveAgentRole analysis.agentRoleId
    tokenBudget <- Budget.promptTokenBudget
    languageName <- agentLanguageName analysis.language
    promptResult <- buildPromptForAlert languageName tokenBudget (templateNameForRole role) alert
    case promptResult of
        Nothing -> failAnalysis analysis alert "no active prompt template" "llm_failed"
        Just built -> do
            now <- getCurrentTime
            _ <-
                analysis
                    |> set #status "running"
                    |> set #provider config.providerName
                    |> set #model config.model
                    |> set #promptTemplateId (Just built.templateId)
                    |> set #promptVersion (Just built.templateVersion)
                    |> set #promptHash built.hash
                    |> set #agentRoleId (fmap (get #id) role)
                    |> set #updatedAt now
                    |> updateRecord
            dedupeWindow <- Budget.dedupeWindowSeconds
            prior <- findDedupeSource (get #id analysis) built.hash (addUTCTime (fromIntegral (-dedupeWindow)) now)
            case prior of
                Just priorAnalysis -> copyDeduped analysis alert priorAnalysis
                Nothing -> do
                    overBudget <- checkBudget config.providerName
                    if overBudget
                        then failAnalysis analysis alert "budget_exceeded" "llm_skipped"
                        else do
                            delayed <- checkRateLimit config.providerName now
                            case delayed of
                                Just delaySeconds -> requeue analysis job delaySeconds
                                Nothing -> callProvider job analysis alert config role built

findDedupeSource :: (?modelContext :: ModelContext) => Id LlmAnalysis -> Text -> UTCTime -> IO (Maybe LlmAnalysis)
findDedupeSource selfId hash cutoff =
    query @LlmAnalysis
        |> filterWhere (#promptHash, hash)
        |> filterWhereNot (#id, selfId)
        |> filterWhere (#status, "done" :: Text)
        |> filterWhereSql (#dedupedFrom, "IS NULL")
        |> filterWhereSql (#createdAt, ">= " <> sqlQuote cutoff)
        |> orderByDesc #createdAt
        |> limit 1
        |> fetchOneOrNothing

sqlQuote :: UTCTime -> Text
sqlQuote time = "'" <> tshow time <> "'"

-- §6: identical prompt within the dedupe window copies the prior result into
-- a fresh row — per-alert history stays self-contained and the card query
-- remains "latest by alert".
copyDeduped :: (?modelContext :: ModelContext) => LlmAnalysis -> Alert -> LlmAnalysis -> IO ()
copyDeduped analysis alert prior = do
    now <- getCurrentTime
    _ <-
        analysis
            |> set #status "done"
            |> set #resultMd prior.resultMd
            |> set #result prior.result
            |> set #tokensIn prior.tokensIn
            |> set #tokensOut prior.tokensOut
            |> set #dedupedFrom (Just (get #id prior))
            |> set #updatedAt now
            |> updateRecord
    publishAlertUpdate alert "enriched"

checkBudget :: (?modelContext :: ModelContext) => Text -> IO Bool
checkBudget provider = do
    cap <- Budget.dailyTokenBudget
    rows <-
        sqlQueryTyped
            [typedSql|
        SELECT tokens_in, tokens_out FROM llm_budget_counters
        WHERE scope = 'analysis' AND provider = ${provider} AND day = CURRENT_DATE
    |]
    pure case rows of
        [] -> False
        (row : _) -> Budget.budgetExceeded cap (fromIntegral (get #tokens_in row)) (fromIntegral (get #tokens_out row))

checkRateLimit :: (?modelContext :: ModelContext) => Text -> UTCTime -> IO (Maybe Int)
checkRateLimit provider now = do
    perMinute <- Budget.rateLimitPerMinute
    recent <-
        sqlQueryTyped
            [typedSql|
        SELECT updated_at FROM llm_analyses
        WHERE provider = ${provider} AND status = 'done' AND deduped_from IS NULL
            AND updated_at > NOW() - INTERVAL '60 seconds'
        ORDER BY updated_at DESC
    |]
    pure (Budget.rateLimitDelaySeconds perMinute now recent)

-- §6: over the rate limit the job re-queues itself with a delay instead of
-- failing; the analysis row drops back to queued for the next attempt.
requeue :: (?modelContext :: ModelContext) => LlmAnalysis -> LlmAnalysisJob -> Int -> IO ()
requeue analysis job delaySeconds = do
    now <- getCurrentTime
    void do
        analysis
            |> set #status "queued"
            |> set #updatedAt now
            |> updateRecord
    void do
        newRecord @LlmAnalysisJob
            |> set #analysisId job.analysisId
            |> set #runAt (addUTCTime (fromIntegral delaySeconds) now)
            |> createRecord

callProvider :: (?modelContext :: ModelContext) => LlmAnalysisJob -> LlmAnalysis -> Alert -> LlmProviderConfig -> Maybe LlmAgentRole -> BuiltPrompt -> IO ()
callProvider job analysis alert config role built = do
    source <- mapM fetch alert.sourceId
    let messages = [userMessage built.rendered]
        tools = if config.toolsEnabled then toolsForRole role else []
    outcome <- runWithToolLoop config source 3 messages tools []
    case outcome of
        Left (Retriable err) -> do
            backoffs <- Budget.backoffSeconds
            -- Fresh requeued job rows reset attempts_count, so the retry
            -- budget counts llm_analysis_jobs rows for this analysis instead
            -- (the original enqueue counts as the first attempt).
            attempts <-
                query @LlmAnalysisJob
                    |> filterWhere (#analysisId, get #id analysis)
                    |> fetch
            if length attempts <= length backoffs
                then requeue analysis job (backoffs !! (length attempts - 1))
                else failAnalysis analysis alert err "llm_failed"
        Left (Terminal err) -> failAnalysis analysis alert err "llm_failed"
        Right (completion, toolLog) -> do
            let parsed = parseCompletionOutput completion.content
            now <- getCurrentTime
            _ <-
                analysis
                    |> set #status "done"
                    |> set #resultMd (Just parsed.markdown)
                    |> set #result parsed.structured
                    |> set #tokensIn completion.tokensIn
                    |> set #tokensOut completion.tokensOut
                    |> set #toolCalls (if null toolLog then Nothing else Just (Aeson.toJSON toolLog))
                    |> set #updatedAt now
                    |> updateRecord
            recordUsage config.providerName completion
            publishAlertUpdate alert "enriched"

-- The tool loop lives in Application.Service.Llm.Tools (shared with the
-- related-tasks relevance filter, milestone 10).

recordUsage :: (?modelContext :: ModelContext) => Text -> Completion -> IO ()
recordUsage provider completion = do
    let tokensIn = fromIntegral (fromMaybe 0 completion.tokensIn) :: Int64
        tokensOut = fromIntegral (fromMaybe 0 completion.tokensOut) :: Int64
    void do
        sqlExecTyped
            [typedSql|
            INSERT INTO llm_budget_counters (scope, provider, day, tokens_in, tokens_out, requests)
            VALUES ('analysis', ${provider}, CURRENT_DATE, ${tokensIn}, ${tokensOut}, 1)
            ON CONFLICT (scope, provider, day) DO UPDATE SET
                tokens_in = llm_budget_counters.tokens_in + EXCLUDED.tokens_in,
                tokens_out = llm_budget_counters.tokens_out + EXCLUDED.tokens_out,
                requests = llm_budget_counters.requests + 1
        |]

failAnalysis :: (?modelContext :: ModelContext) => LlmAnalysis -> Alert -> Text -> Text -> IO ()
failAnalysis analysis alert err eventKind = do
    now <- getCurrentTime
    void do
        analysis
            |> set #status "failed"
            |> set #errorMessage (Just err)
            |> set #updatedAt now
            |> updateRecord
    void do
        newRecord @AlertEvent
            |> set #alertId (get #id alert)
            |> set #userId Nothing
            |> set #kind eventKind
            |> set #payload (object ["error" .= err])
            |> createRecord
    publishAlertUpdate alert "enriched"
