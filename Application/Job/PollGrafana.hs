module Application.Job.PollGrafana where

import IHP.Prelude
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Generated.Types
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest)
import qualified Application.Connector.Grafana as Grafana
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as Text
import Control.Monad (void)
import System.Environment (lookupEnv)

-- Reconcile poller for grafana sources (design_docs/milestone_2.md §8): the
-- webhook stays the low-latency path; this job re-fetches the embedded
-- alertmanager's alert listing so resolves/fires missed during downtime or a
-- dropped webhook still land. Shared fingerprints make dual-path delivery
-- dedupe onto one alert. Self-rescheduling like PollZabbixJob.
--
-- Grafana's embedded alertmanager drops resolved alerts from the listing
-- within seconds, so resolves are reconciled by ABSENCE: a firing alert
-- whose fingerprint no longer appears in the listing (with a grace window
-- against listing lag) is resolved locally. The 90s refire guard covers the
-- inverse race — the listing still showing an alert the webhook already
-- resolved.
instance Job PollGrafanaJob where
    perform _job = do
        sources <- query @Source
            |> filterWhere (#type_, "grafana" :: Text)
            |> filterWhere (#enabled, True)
            |> fetch
        forM_ sources pollSource

        now <- getCurrentTime
        next <- newRecord @PollGrafanaJob
            |> set #runAt (addUTCTime 5 now)
            |> createRecord
        let nextId = get #id next
        _ <- sqlExecTyped [typedSql|
            DELETE FROM poll_grafana_jobs
            WHERE status = 'job_status_not_started' AND id <> ${nextId}
        |]
        pure ()

    queuePollInterval = 3 * 1000000
    maxAttempts = 3

pollSource :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Source -> IO ()
pollSource source = do
    let tokenEnv :: Maybe Text
        tokenEnv = parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: "tokenEnv")) source.config
    token <- case tokenEnv of
        Just envVar -> fmap cs <$> lookupEnv (cs envVar)
        Nothing -> pure Nothing
    case token of
        Nothing -> pure () -- token not issued yet (seed not run); try next cycle
        Just token -> do
            now <- getCurrentTime
            result <- Grafana.alertsGet source.baseUrl token
            case result of
                Left _err -> pure () -- soft-fail; surfaced via logs in phase 5 (source health)
                Right alerts -> do
                    -- 5-min overlap window on the cursor (§8) so a restarted
                    -- poller re-sees the tail and dedupe handles the rest.
                    let cutoff = addUTCTime (-300) (fromMaybe now source.lastSyncCursor)
                        fresh = filter (\amAlert -> maybe True (>= cutoff) amAlert.amUpdatedAt) alerts
                    forM_ fresh \amAlert -> do
                        let event = Grafana.amAlertToNormalized now amAlert
                        skip <- refireGuard now event
                        unless skip (void (ingest source event))
                    reconcileAbsences source now alerts
                    _ <- source
                        |> set #lastSyncCursor (Just now)
                        |> updateRecord
                    pure ()

-- | The listing can lag a webhook resolve by a few seconds (alertmanager
-- keeps the alert listed until its endsAt passes); don't let the poller
-- refire an alert the fast path just resolved.
refireGuard :: (?modelContext :: ModelContext) => UTCTime -> NormalizedEvent -> IO Bool
refireGuard now event = case event.status of
    Resolved -> pure False
    Firing -> do
        existing <- query @Alert
            |> filterWhere (#fingerprint, event.fingerprint)
            |> fetchOneOrNothing
        pure case existing of
            Just alert
                | alert.status == "resolved"
                , Just resolvedAt <- alert.resolvedAt
                , resolvedAt > addUTCTime (-90) now -> True
            _ -> False

-- | Firing alerts owned by this source that vanished from the listing
-- resolved while we weren't listening. Grace window guards against listing
-- blips: only alerts not seen for a minute are resolved.
reconcileAbsences :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Source -> UTCTime -> [Grafana.GrafanaAmAlert] -> IO ()
reconcileAbsences source now alerts = do
    let listedFingerprints = map ("grafana:" <>) (map (.amFingerprint) alerts)
    firing <- query @Alert
        |> filterWhere (#sourceId, Just (get #id source))
        |> filterWhere (#status, "firing" :: Text)
        |> fetch
    let graceCutoff = addUTCTime (-60) now
        vanished = filter (\alert ->
            "grafana:" `Text.isPrefixOf` alert.fingerprint
                && alert.fingerprint `notElem` listedFingerprints
                && alert.lastSeenAt < graceCutoff) firing
    forM_ vanished \alert ->
        void (ingest source NormalizedEvent
            { fingerprint = alert.fingerprint
            , externalId = alert.externalId
            , status = Resolved
            , severity = alert.severity
            , title = alert.title
            , description = alert.description
            , env = alert.env
            , host = alert.host
            , service = alert.service
            , checkName = alert.checkName
            , labels = alert.labels
            , annotations = alert.annotations
            , startedAt = alert.startedAt
            , sourceUrl = alert.sourceUrl
            })
