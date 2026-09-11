module Application.Service.Jira.Related
( relatedTasksForAlert
, relevancePrompt
, relevanceContract
, relatedRoleName
, relatedTemplateName
, parseRelevantKeys
, maxCandidates
) where

import IHP.Prelude
import IHP.ModelSupport (ModelContext, newRecord, createRecord, updateRecord, deleteRecord)
import IHP.HaskellSupport (set, get)
import IHP.QueryBuilder (query, filterWhere)
import IHP.Fetch (fetch, fetchOneOrNothing)
import Generated.Types
import qualified Application.Service.Jira as Jira
import Application.Service.Jira (JiraIssue (..))
import qualified Application.Service.Jira.DbConfig as JiraDb
import qualified Application.Service.Assets as Assets
import qualified Application.Service.Assets.Cache as AssetsCache
import Application.Service.Assets.Types (Ticket (..))
import Application.Service.Llm (Prompt (..), userMessage, Completion (..), LlmProviderConfig (..))
import Application.Service.Llm.DbConfig (currentLlmConfig)
import qualified Application.Service.Llm.AutoAnalyze as AutoAnalyze
import Application.Service.Llm.Output (parseCompletionOutput, ParsedOutput (..))
import Application.Service.Llm.Prompt (PromptInputs (..), emptyInputs, bindingsFor, renderTemplate)
import Application.Service.Llm.Roles (resolveAgentRoleByName, toolsForRole)
import Application.Service.Llm.Tools (runWithToolLoop)
import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)
import Data.Aeson ((.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Functor ((<&>))

-- Related Jira tasks (milestone 10): candidates are gathered from Jira issue
-- search across ALL configured projects AND from Jira Assets "linked tasks"
-- (objectconnectedtickets) of the alert's linked assets. The configured LLM
-- then keeps only tasks relevant to the alert; the survivors are stored as
-- jira_links rows with origin 'related' and shown in the Jira card's
-- "Related tasks" section. Re-runs converge: related rows the LLM no longer
-- selects are deleted.
--
-- The filter prompt is template-driven like alert enrichment: the agent role
-- named relatedRoleName points at an active llm_prompt_templates row
-- (default relatedTemplateName) and carries the tool whitelist (seeded with
-- jira_issue_details so the model can fetch full task cards on demand). No
-- role/template → the built-in relevancePrompt fallback.

maxCandidates :: Int
maxCandidates = 15

relatedRoleName :: Text
relatedRoleName = "jira-related-filter"

relatedTemplateName :: Text
relatedTemplateName = "jira_related_filter"

data RelatedCandidate = RelatedCandidate
    { candidateKey :: Text
    , candidateSummary :: Text
    , candidateStatus :: Text
    , candidateUrl :: Text
    }

relatedTasksForAlert :: (?modelContext :: ModelContext) => Maybe Source -> Alert -> IO (Either Text ())
relatedTasksForAlert maybeSource alert = do
    -- Non-actionable alerts (resolved/closed/stalled, per the same status
    -- gate as auto-analysis) skip the search + LLM filter entirely —
    -- related tasks are advisory and must not churn on dead alerts.
    rules <- AutoAnalyze.currentRules
    if alert.status `notElem` rules.aaStatuses
        then pure (Right ())
        else runRelated maybeSource alert

runRelated :: (?modelContext :: ModelContext) => Maybe Source -> Alert -> IO (Either Text ())
runRelated maybeSource alert = do
    configs <- case maybeSource of
        Just source -> JiraDb.jiraConfigsForSource source
        Nothing -> JiraDb.currentJiraConfigs
    jiraResults <- forM configs \config -> do
        -- Unlike auto-link there is no statusCategory filter: related tasks
        -- deliberately include historical (Done) tickets about the same
        -- subject.
        result <- Jira.searchIssues config (Jira.projectClause (Jira.projects config) <> "(" <> Jira.jqlSubjectTerms alert <> ")") 10
        pure (result <&> map (fromIssue config))
    let jiraCandidates = concat [issues | Right issues <- jiraResults]
    assetsCandidates <- assetsTicketCandidates configs alert
    let candidates = dedupeOn candidateKey (jiraCandidates ++ assetsCandidates)
    existing <- query @JiraLink
        |> filterWhere (#alertId, get #id alert)
        |> fetch
    let linkedKeys = [link.ticketKey | link <- existing, link.origin /= "related"]
        fresh = filter (\candidate -> candidate.candidateKey `notElem` linkedKeys) (take maxCandidates candidates)
    enriched <- enrichCandidates configs fresh
    llmVerdict <- filterWithLlm alert maybeSource enriched
    let kept = case llmVerdict of
            Just keys -> filter (\candidate -> candidate.candidateKey `elem` keys) enriched
            Nothing -> enriched -- no LLM configured or call failed: keep candidates unfiltered
    now <- getCurrentTime
    forM_ kept \candidate -> do
        existingLink <- query @JiraLink
            |> filterWhere (#alertId, get #id alert)
            |> filterWhere (#ticketKey, candidate.candidateKey)
            |> fetchOneOrNothing
        let applyFields record = record
                |> set #summary candidate.candidateSummary
                |> set #status candidate.candidateStatus
                |> set #url candidate.candidateUrl
                |> set #syncedAt now
        -- Skip the write when nothing changed — re-upserting identical rows
        -- every cycle is pure churn (syncedAt only moves on real changes).
        _ <- case existingLink of
            Just link
                | link.summary == candidate.candidateSummary
                , link.status == candidate.candidateStatus
                , link.url == candidate.candidateUrl -> pure link
                | otherwise -> updateRecord (applyFields link)
            Nothing -> createRecord (applyFields (newRecord @JiraLink
                |> set #alertId (get #id alert)
                |> set #ticketKey candidate.candidateKey
                |> set #origin "related"))
        pure ()
    let keepKeys = map (.candidateKey) kept
        stale = [link | link <- existing, link.origin == "related", link.ticketKey `notElem` keepKeys]
    forM_ stale deleteRecord
    -- Total search failure (configs exist but every one errored) is reported
    -- so the enrichment job can log an enrichment_failed event; partial
    -- failures and the LLM filter stay silent (soft-fail, advisory data).
    pure case [err | Left err <- jiraResults] of
        errs | not (null errs) && length errs == length jiraResults -> Left (fromMaybe "jira search failed" (head errs))
        _ -> Right ()

fromIssue :: Jira.JiraConfig -> JiraIssue -> RelatedCandidate
fromIssue config issue = RelatedCandidate
    { candidateKey = issue.issueKey
    , candidateSummary = issue.issueSummary
    , candidateStatus = issue.issueStatus
    , candidateUrl = Jira.issueUrl config issue.issueKey
    }

-- Assets "linked tasks" (milestone 10): tickets connected to each linked
-- assets object. The browse URL is derived from the first Jira config when
-- one exists, else from the assets connection's Jira origin.
assetsTicketCandidates :: (?modelContext :: ModelContext) => [Jira.JiraConfig] -> Alert -> IO [RelatedCandidate]
assetsTicketCandidates jiraConfigs alert = do
    linked <- AssetsCache.linkedAssetsForAlert alert
    concat <$> forM linked \(_, object) -> do
        config <- fetchOneOrNothing object.configId
        case config of
            Nothing -> pure []
            Just assetsConfig -> do
                clientResult <- Assets.clientFromConfig assetsConfig
                case clientResult of
                    Left _ -> pure []
                    Right client -> do
                        result <- Assets.connectedTickets client (fromIntegral object.objectId)
                        pure case result of
                            Left _ -> []
                            Right tickets -> map (fromTicket (browseBase jiraConfigs assetsConfig)) tickets
    where
        browseBase jiraConfigs assetsConfig = case jiraConfigs of
            (config:_) -> Text.dropWhileEnd (== '/') (Jira.baseUrl config)
            [] -> Text.dropWhileEnd (== '/')
                (fromMaybe assetsConfig.baseUrl (Text.stripSuffix "/rest/assets/latest" assetsConfig.baseUrl))
        fromTicket base ticket = RelatedCandidate
            { candidateKey = ticket.ticketKey
            , candidateSummary = ticket.ticketSummary
            , candidateStatus = ticket.ticketStatus
            , candidateUrl = base <> "/browse/" <> ticket.ticketKey
            }

-- Candidates from Assets connected tickets can arrive without a summary
-- (title) — fill those from the Jira issue endpoint so the LLM filter and
-- the card always have a title to work with.
enrichCandidates :: [Jira.JiraConfig] -> [RelatedCandidate] -> IO [RelatedCandidate]
enrichCandidates configs = mapM enrich
    where
        enrich candidate
            | not (Text.null candidate.candidateSummary) = pure candidate
            | otherwise = go configs
            where
                go [] = pure candidate
                go (config:rest) = do
                    result <- Jira.getIssue config candidate.candidateKey
                    case result of
                        Left _ -> go rest
                        Right issue -> pure candidate
                            { candidateSummary = issue.issueSummary
                            , candidateStatus = if Text.null candidate.candidateStatus then issue.issueStatus else candidate.candidateStatus
                            , candidateUrl = Jira.issueUrl config issue.issueKey
                            }

dedupeOn :: Eq b => (a -> b) -> [a] -> [a]
dedupeOn key = go []
    where
        go _ [] = []
        go seen (x:xs)
            | key x `elem` seen = go seen xs
            | otherwise = x : go (key x : seen) xs

-- LLM relevance filter: returns Just keys-to-keep when the configured LLM
-- answered with a parseable verdict (an empty list is a valid "none
-- relevant" verdict), Nothing when no LLM is configured or the call/parse
-- failed (caller keeps candidates unfiltered — soft-fail). The prompt comes
-- from the relatedRoleName agent role's active template when present
-- (admin-editable, milestone 10), else the built-in fallback; the role's
-- tool whitelist lets the model fetch full task details on demand.
filterWithLlm :: (?modelContext :: ModelContext) => Alert -> Maybe Source -> [RelatedCandidate] -> IO (Maybe [Text])
filterWithLlm _ _ [] = pure (Just [])
filterWithLlm alert maybeSource candidates = do
    maybeConfig <- currentLlmConfig
    case maybeConfig of
        Nothing -> pure Nothing
        Just config -> do
            role <- resolveAgentRoleByName relatedRoleName
            rendered <- renderFilterPrompt alert candidates role
            let tools = if config.toolsEnabled then toolsForRole role else []
            outcome <- runWithToolLoop config maybeSource 3 [userMessage rendered] tools []
            pure case outcome of
                Left _ -> Nothing
                Right (completion, _) -> parseRelevantKeys (map (.candidateKey) candidates) completion.content

renderFilterPrompt :: (?modelContext :: ModelContext) => Alert -> [RelatedCandidate] -> Maybe LlmAgentRole -> IO Text
renderFilterPrompt alert candidates role = do
    let templateName = maybe relatedTemplateName (.promptTemplateName) role
    template <- query @LlmPromptTemplate
        |> filterWhere (#name, templateName)
        |> filterWhere (#active, True)
        |> fetchOneOrNothing
    pure case template of
        Just row -> renderTemplate row.body (filterBindings alert candidates) <> relevanceContract
        Nothing -> relevancePrompt alert candidates

filterBindings :: Alert -> [RelatedCandidate] -> [(Text, Text)]
filterBindings alert candidates =
    bindingsFor emptyInputs
        { piTitle = alert.title
        , piSeverity = alert.severity
        , piEnv = fromMaybe "unknown" (effectiveFieldText FieldEnv alert)
        , piHost = fromMaybe "unknown" (effectiveFieldText FieldHost alert)
        , piService = fromMaybe "unknown" (effectiveFieldText FieldService alert)
        , piCheckName = fromMaybe "unknown" alert.checkName
        , piDescription = Text.take 500 alert.description
        }
    ++ [("candidates", Text.intercalate "\n" (map candidateLine candidates))]

candidateLine :: RelatedCandidate -> Text
candidateLine candidate = "- " <> candidate.candidateKey <> ": " <> candidate.candidateSummary <> " [" <> candidate.candidateStatus <> "]"

-- Appended after the rendered admin template (mirrors outputContract for
-- alert enrichment): the verdict format is a code-level contract, not
-- template content.
relevanceContract :: Text
relevanceContract = Text.intercalate "\n"
    [ ""
    , "Keep only tasks plausibly related to this alert (same host, service or failure mode)."
    , "Respond with exactly one ```json fenced block of the shape {\"relevant\": [\"KEY-1\", ...]}."
    , "Use only keys from the candidate list; an empty list means none are relevant."
    ]

relevancePrompt :: Alert -> [RelatedCandidate] -> Text
relevancePrompt alert candidates = Text.intercalate "\n"
    [ "You are triaging Jira tasks related to a monitoring alert."
    , ""
    , "Alert: " <> alert.title
    , "Host: " <> fromMaybe "-" alert.host <> "; Service: " <> fromMaybe "-" alert.service <> "; Check: " <> fromMaybe "-" alert.checkName
    , "Severity: " <> alert.severity
    , "Description: " <> Text.take 500 alert.description
    , ""
    , "## Candidate Jira tasks"
    , Text.intercalate "\n" (map candidateLine candidates)
    , "You may call jira_issue_details with a task key to inspect its description and comments before deciding."
    , relevanceContract
    ]

-- Extracts the {"relevant": [...]} verdict from the fenced json block,
-- intersected with the actual candidate keys so hallucinated keys drop out.
parseRelevantKeys :: [Text] -> Text -> Maybe [Text]
parseRelevantKeys candidateKeys raw = do
    structured <- (parseCompletionOutput raw).structured
    keys <- parseMaybe (Aeson.withObject "verdict" (.: "relevant")) structured
    pure (filter (`elem` candidateKeys) keys)
