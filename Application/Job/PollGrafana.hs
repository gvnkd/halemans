module Application.Job.PollGrafana where

import qualified Application.Connector.Alertmanager as Am
import qualified Application.Connector.Grafana as Grafana
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest)
import Application.Service.Log (logDebug, logInfo, logWarn)
import Application.Service.Reconcile (lastAckWasExternal, mirrorExternalAck, mirrorExternalUnack, shouldMirror)
import Application.Service.SourceHealth (pollDue, recordFailure, recordSuccess)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.FrameworkConfig (FrameworkConfig (..))
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
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
    perform job = do
        -- Single-loop guard: see PollZabbixJob — concurrent duplicate loops
        -- double-ingest and create duplicate alert rows. The older-created
        -- running job wins; this one stops without rescheduling.
        let createdAt = job.createdAt
        olderRunning <-
            sqlQueryTyped
                [typedSql|
            SELECT count(*) FROM poll_grafana_jobs
            WHERE status = 'job_status_running' AND created_at < ${createdAt}
        |]
        case olderRunning of
            (count_ : _)
                | count_ > 0 ->
                    logWarn "duplicate PollGrafanaJob loop detected (an older poll job is running); stopping this one"
            _ -> do
                now <- getCurrentTime
                sources <-
                    query @Source
                        |> filterWhere (#type_, "grafana" :: Text)
                        |> filterWhere (#enabled, True)
                        |> fetch
                forM_ (filter (pollDue now) sources) pollSource

                if null sources
                    then do
                        logInfo "no enabled grafana sources; poll loop stopped (re-arms on source create/enable)"
                        void $
                            sqlExecTyped
                                [typedSql|
                            DELETE FROM poll_grafana_jobs
                            WHERE status = 'job_status_not_started'
                        |]
                    else reschedule

    queuePollInterval = 3 * 1000000
    maxAttempts = 3

-- Guarded reschedule: insert only when nothing is pending, so a duplicate
-- loop's reschedule no-ops and the loop dies instead of fighting over (or
-- deleting) the surviving loop's successor row.
reschedule :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => IO ()
reschedule = do
    now <- getCurrentTime
    let runAt = addUTCTime 5 now
    inserted <-
        sqlQueryTyped
            [typedSql|
        INSERT INTO poll_grafana_jobs (run_at)
        SELECT ${runAt}
        WHERE NOT EXISTS (SELECT 1 FROM poll_grafana_jobs WHERE status = 'job_status_not_started')
        RETURNING id
    |]
    case inserted of
        (nextId : _) ->
            void $
                sqlExecTyped
                    [typedSql|
            DELETE FROM poll_grafana_jobs
            WHERE status = 'job_status_not_started' AND id <> ${nextId}
        |]
        [] -> logInfo "another PollGrafanaJob is already pending; stopping this loop"

pollSource :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Source -> IO ()
pollSource source = do
    let tokenEnv :: Maybe Text
        tokenEnv = parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: "tokenEnv")) source.config
    token <- case tokenEnv of
        Just envVar -> fmap cs <$> lookupEnv (cs envVar)
        Nothing -> pure Nothing
    case token of
        Nothing -> logDebug ("grafana source \"" <> source.name <> "\": token env var " <> fromMaybe "<none configured>" tokenEnv <> " not set; skipping poll cycle")
        Just token -> do
            now <- getCurrentTime
            -- One streaming pass (alertsFold): fresh alerts are ingested as
            -- they arrive while fingerprints accumulate for the absence
            -- reconciliation — the listing itself is never materialized in
            -- memory, so a big source no longer OOMs the worker.
            let cutoff = addUTCTime (-300) (fromMaybe now source.lastSyncCursor)
                stepAlert (count, fingerprints) amAlert = do
                    let fingerprints' = Set.insert ("grafana:" <> amAlert.amFingerprint) fingerprints
                    case amAlert.amUpdatedAt of
                        -- 5-min overlap window on the cursor (§8) so a
                        -- restarted poller re-sees the tail and dedupe
                        -- handles the rest.
                        Just updated | updated < cutoff -> pure (count + 1, fingerprints')
                        _ -> do
                            let event = Grafana.amAlertToNormalized now amAlert
                            skip <- refireGuard now event
                            unless skip (void (ingest source event))
                            pure (count + 1, fingerprints')
            outcome <- try (Grafana.alertsFold source.baseUrl token stepAlert (0, Set.empty))
            result <- pure case outcome of
                Left err -> Left (tshow (err :: SomeException))
                Right result -> result
            case result of
                Left err -> do
                    recordFailure source err
                    logWarn ("grafana source \"" <> source.name <> "\" poll failed: " <> err)
                Right (count, listedFingerprints) -> do
                    logDebug ("grafana source \"" <> source.name <> "\": alertmanager listing returned " <> tshow count <> " alerts")
                    recordSuccess source
                    reconcileAbsences source now listedFingerprints
                    reconcileSilences source token now
                    _ <-
                        source
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
        existing <-
            query @Alert
                |> filterWhere (#fingerprint, event.fingerprint)
                |> fetchOneOrNothing
        pure case existing of
            Just alert
                | alert.status == "resolved"
                , Just resolvedAt <- alert.resolvedAt
                , resolvedAt > addUTCTime (-90) now ->
                    True
            _ -> False

-- | Firing alerts owned by this source that vanished from the listing
-- resolved while we weren't listening. Grace window guards against listing
-- blips: only alerts not seen for a minute are resolved. Takes the
-- fingerprint Set accumulated during the streaming fold, not the alerts.
reconcileAbsences :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Source -> UTCTime -> Set.Set Text -> IO ()
reconcileAbsences source now listedFingerprints = do
    firing <-
        query @Alert
            |> filterWhere (#sourceId, Just (get #id source))
            |> filterWhereIn (#status, ["firing", "stalled"] :: [Text])
            |> fetch
    let graceCutoff = addUTCTime (-60) now
        vanished =
            filter
                ( \alert ->
                    "grafana:" `Text.isPrefixOf` alert.fingerprint
                        && not (Set.member alert.fingerprint listedFingerprints)
                        && alert.lastSeenAt < graceCutoff
                )
                firing
    forM_ vanished \alert ->
        void
            ( ingest
                source
                NormalizedEvent
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
                    }
            )

-- Silence-based ack reconciliation (milestone_3.md §6): an active silence
-- covering a firing alert mirrors in as an external ack; silence expiry
-- reverts only acks that came from this mirror (never a local user's ack).
reconcileSilences :: (?modelContext :: ModelContext) => Source -> Text -> UTCTime -> IO ()
reconcileSilences source token now = do
    alerts <-
        query @Alert
            |> filterWhere (#sourceId, Just (get #id source))
            |> filterWhereIn (#status, ["firing", "ack"] :: [Text])
            |> fetch
    result <- Am.silencesGet source.baseUrl (Just token) "/api/alertmanager/grafana/api/v2"
    case result of
        Left _err -> pure ()
        Right silences -> forM_ alerts \alert -> do
            let covering = filter (\silence -> Am.silenceCoversLabels silence alert.labels) silences
            case (alert.status, covering) of
                ("firing", (silence : _)) -> do
                    let sourceAt = fromMaybe now silence.silenceUpdatedAt
                    when (shouldMirror alert.acknowledgedAt sourceAt) do
                        void (mirrorExternalAck alert "grafana" silence.silenceCreatedBy sourceAt)
                ("ack", []) -> do
                    external <- lastAckWasExternal alert
                    when external do
                        void (mirrorExternalUnack alert "grafana" "silence expired" now)
                _ -> pure ()
