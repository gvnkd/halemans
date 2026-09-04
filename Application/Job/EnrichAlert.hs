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

-- Enrichment pipeline step 8 (design_docs/milestone_3.md §3): on new alert
-- only. CMDB + Jira lookups soft-fail independently — Confluence down still
-- runs Jira and vice versa; failures land as AlertEvent(enrichment_failed)
-- with a subsystem tag and the job re-enqueues with a delay (max 3 runs).
instance Job EnrichAlertJob where
    perform job = do
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

    maxAttempts = 3

fetchMaybeSource :: (?modelContext :: ModelContext) => Alert -> IO (Maybe Source)
fetchMaybeSource alert = mapM fetch alert.sourceId
