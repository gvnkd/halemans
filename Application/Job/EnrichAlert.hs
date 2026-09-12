module Application.Job.EnrichAlert where

import Application.Helper.Ingest (publishAlertUpdate)
import qualified Application.Service.Assets.Cache as AssetsCache
import qualified Application.Service.Cmdb.DbConfig as Cmdb
import qualified Application.Service.Facets as Facets
import qualified Application.Service.Groups as Groups
import qualified Application.Service.Http as Http
import qualified Application.Service.Jira.DbConfig as Jira
import qualified Application.Service.Jira.Related as Related
import qualified Application.Service.Llm.AutoAnalyze as AutoAnalyze
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (object, (.=))
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

-- Enrichment pipeline step 8 (design_docs/milestone_3.md §3): on new alert
-- only. CMDB + Jira lookups soft-fail independently — Confluence down still
-- runs Jira and vice versa; failures land as AlertEvent(enrichment_failed)
-- with a subsystem tag and the job re-enqueues with a delay (max 3 runs).
instance Job EnrichAlertJob where
    perform job = do
        startedAt <- getCurrentTime
        alert <- fetch job.alertId
        source <- fetchMaybeSource alert
        failures <- case source of
            Nothing -> pure []
            Just source -> do
                cmdbOutcome <- try (Cmdb.lookupForAlert source alert)
                jiraOutcome <- try (Jira.autoLinkForAlert source alert)
                let cmdbFailure = case cmdbOutcome of
                        Left err -> Just ("cmdb", tshow (err :: SomeException))
                        Right (Left err) -> Just ("cmdb", err)
                        Right (Right _) -> Nothing
                    jiraFailure = case jiraOutcome of
                        Left err -> Just ("jira", tshow (err :: SomeException))
                        Right (Left err) -> Just ("jira", err)
                        Right (Right _) -> Nothing
                    failures :: [(Text, Text)]
                    failures = catMaybes [cmdbFailure, jiraFailure]
                pure failures
        -- Assets (milestone_8.md §4): third independent soft-fail subsystem,
        -- not gated on a source row (configs are global).
        assetsOutcome <- try (AssetsCache.lookupAssetsForAlert alert)
        let assetsFailure = case assetsOutcome of
                Left err -> Just ("assets", tshow (err :: SomeException))
                Right (Left err) -> Just ("assets", err)
                Right (Right _) -> Nothing
            allFailures = failures ++ maybeToList assetsFailure
        -- Related Jira tasks (milestone 10): runs AFTER assets so the
        -- connected tickets of freshly linked objects are visible. Logged as
        -- an enrichment_failed event on total search failure but does NOT
        -- re-enqueue the run (advisory data; the next alert's run retries).
        relatedOutcome <- try (Related.relatedTasksForAlert source alert)
        let relatedFailure = case relatedOutcome of
                Left err -> Just ("jira-related", tshow (err :: SomeException))
                Right (Left err) -> Just ("jira-related", err)
                Right (Right _) -> Nothing
        forM_ (allFailures ++ maybeToList relatedFailure) \(subsystem, err) -> void do
            newRecord @AlertEvent
                |> set #alertId (get #id alert)
                |> set #userId Nothing
                |> set #kind "enrichment_failed"
                |> set #payload (object ["subsystem" .= subsystem, "error" .= err])
                |> createRecord
        -- Retry policy: only actionable alerts (same status gate as
        -- auto-analysis — enrichment is advisory, resolved/closed alerts
        -- must not loop) and only failures worth retrying — 4xx responses
        -- are deterministic and would fail identically forever. The
        -- attempts counter carries forward into the fresh row so the budget
        -- actually exhausts (a fresh row's attempts_count starts at 0).
        let retryableFailures = filter (\(_, err) -> not (Http.isDeterministicClientError err)) allFailures
        retryAllowed <-
            if null retryableFailures
                then pure False
                else do
                    rules <- AutoAnalyze.currentRules
                    pure (alert.status `elem` rules.aaStatuses)
        when (retryAllowed && job.attemptsCount < 2) do
            now <- getCurrentTime
            void do
                newRecord @EnrichAlertJob
                    |> set #alertId job.alertId
                    |> set #attemptsCount (job.attemptsCount + 1)
                    |> set #runAt (addUTCTime 60 now)
                    |> createRecord
        -- Facet materialization (milestone_9.md §3): re-resolve with the
        -- freshly linked assets objects; publishes below carry the new row.
        alert <- Facets.materializeFacets alert
        -- Regroup replay (milestone_9.md §7): facet-referencing rules only
        -- see attr facets now that they are materialized.
        alert <- Groups.regroupAlert alert
        publishAlertUpdate alert "enriched"
        -- Panel-only refresh for the assets card (milestone_8.md §4): the
        -- websocket broadcaster re-renders context panels without touching
        -- the timeline on this kind.
        publishAlertUpdate alert "assets"
        maybeRetriggerAnalysis alert startedAt

    maxAttempts = 3

-- Enrichment-triggered re-analysis (design_docs/milestone_5.md §7): an alert
-- whose LLM analysis completed before this enrichment run gets exactly one
-- automatic re-analysis so the fresh CMDB/Jira context is included. The
-- marker (error_message = 'enrichment_retrigger') caps it at one per alert;
-- prompt-hash dedupe in LlmAnalysisJob legitimately suppresses the rerun
-- when the context did not actually change the prompt.
retriggerMarker :: Text
retriggerMarker = "enrichment_retrigger"

maybeRetriggerAnalysis :: (?modelContext :: ModelContext) => Alert -> UTCTime -> IO ()
maybeRetriggerAnalysis alert startedAt = do
    doneBefore <-
        query @LlmAnalysis
            |> filterWhere (#alertId, get #id alert)
            |> filterWhere (#status, "done" :: Text)
            |> filterWhereSql (#createdAt, "< " <> sqlQuote startedAt)
            |> limit 1
            |> fetchOneOrNothing
    alreadyRetriggered <-
        query @LlmAnalysis
            |> filterWhere (#alertId, get #id alert)
            |> filterWhere (#errorMessage, Just retriggerMarker)
            |> limit 1
            |> fetchOneOrNothing
    when (isJust doneBefore && isNothing alreadyRetriggered) do
        -- Auto-analysis gate (milestone 10 §5): the alert may have resolved
        -- or stalled between ingest and this enrichment run — re-analysis
        -- follows the same status/severity rules as the initial enqueue.
        autoAnalyze <- AutoAnalyze.autoAnalyzeAllowed alert
        when autoAnalyze do
            void do
                analysis <-
                    newRecord @LlmAnalysis
                        |> set #alertId (get #id alert)
                        |> set #errorMessage (Just retriggerMarker)
                        |> createRecord
                void do
                    newRecord @LlmAnalysisJob
                        |> set #analysisId (get #id analysis)
                        |> createRecord

sqlQuote :: UTCTime -> Text
sqlQuote time = "'" <> tshow time <> "'"

fetchMaybeSource :: (?modelContext :: ModelContext) => Alert -> IO (Maybe Source)
fetchMaybeSource alert = mapM fetch alert.sourceId
