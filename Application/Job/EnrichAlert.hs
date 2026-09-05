module Application.Job.EnrichAlert where

import IHP.Prelude
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Fetch (fetch)
import Generated.Types
import qualified Application.Service.Cmdb as Cmdb
import qualified Application.Service.Jira as Jira
import Application.Helper.Ingest (publishAlertUpdate)
import Data.Aeson (object, (.=))
import Control.Exception (try, SomeException)
import Control.Monad (void)
import IHP.QueryBuilder
import IHP.Fetch (fetchOneOrNothing)

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
        forM_ failures \(subsystem, err) -> void do
            newRecord @AlertEvent
                |> set #alertId (get #id alert)
                |> set #userId Nothing
                |> set #kind "enrichment_failed"
                |> set #payload (object ["subsystem" .= subsystem, "error" .= err])
                |> createRecord
        when (not (null failures) && job.attemptsCount < 2) do
            now <- getCurrentTime
            void do
                newRecord @EnrichAlertJob
                    |> set #alertId job.alertId
                    |> set #runAt (addUTCTime 60 now)
                    |> createRecord
        publishAlertUpdate alert "enriched"
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
    doneBefore <- query @LlmAnalysis
        |> filterWhere (#alertId, get #id alert)
        |> filterWhere (#status, "done" :: Text)
        |> filterWhereSql (#createdAt, "< " <> sqlQuote startedAt)
        |> limit 1
        |> fetchOneOrNothing
    alreadyRetriggered <- query @LlmAnalysis
        |> filterWhere (#alertId, get #id alert)
        |> filterWhere (#errorMessage, Just retriggerMarker)
        |> limit 1
        |> fetchOneOrNothing
    when (isJust doneBefore && isNothing alreadyRetriggered) do
        void do
            analysis <- newRecord @LlmAnalysis
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
