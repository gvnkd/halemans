module Application.Job.AutoClose where

import qualified Application.Connector.Zabbix as Zabbix
import Application.Helper.Ingest (SourceStatus (..), fetchActiveBlackouts, publishAlertUpdate, transitionAlert)
import Application.Job.PollZabbix (triggerIdOf)
import Application.Pipeline.Actions (autoCloseAlert, stallAlert, unackAlert)
import Application.Pipeline.Blackouts (alertSubject, blackoutApplies)
import Application.Service.Log (logDebug, logInfo, logWarn)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Generated.Types
import IHP.Fetch (fetch)
import IHP.FrameworkConfig (FrameworkConfig (..))
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlExecTyped, typedSql)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- Periodic maintenance (design_docs/milestone_1.md §9): auto-close resolved
-- alerts after TTL, unack expired acks, clear suppression once the covering
-- blackout expired, stall alerts that stopped receiving source updates,
-- refire stalled zabbix alerts whose trigger is still in problem state, and
-- auto-close stalled alerts after their own TTL. Self-rescheduling like
-- PollZabbixJob.
instance Job AutoCloseJob where
    perform _job = do
        autoCloseResolved
        unackExpiredAcks
        unsuppressExpired
        stallStaleAlerts
        reconcileStalledZabbix
        closeStalledAlerts

        now <- getCurrentTime
        next <-
            newRecord @AutoCloseJob
                |> set #runAt (addUTCTime 300 now)
                |> createRecord
        let nextId = get #id next
        _ <-
            sqlExecTyped
                [typedSql|
            DELETE FROM auto_close_jobs
            WHERE status = 'job_status_not_started' AND id <> ${nextId}
        |]
        pure ()

    -- Pick up reschedules promptly (default 60s is too coarse for dev).
    queuePollInterval = 5 * 1000000
    maxAttempts = 3

envSeconds :: Text -> Int -> IO Int
envSeconds name fallback = do
    override <- lookupEnv (cs name)
    pure (fromMaybe fallback (override >>= readMaybe))

autoCloseTtlSeconds :: IO Int
autoCloseTtlSeconds = envSeconds "HALEMANS_AUTO_CLOSE_SECONDS" 86400

-- | No source update within this window marks a firing/ack alert stalled.
stallTtlSeconds :: IO Int
stallTtlSeconds = envSeconds "HALEMANS_STALL_SECONDS" (6 * 3600)

-- | A stalled alert is auto-closed once it spent this long without updates.
stalledCloseTtlSeconds :: IO Int
stalledCloseTtlSeconds = envSeconds "HALEMANS_STALLED_CLOSE_SECONDS" (3 * 86400)

autoCloseResolved :: (?modelContext :: ModelContext) => IO ()
autoCloseResolved = do
    ttlSeconds <- autoCloseTtlSeconds
    cutoff <- addUTCTime (fromIntegral (-ttlSeconds)) <$> getCurrentTime
    stale <-
        query @Alert
            |> filterWhere (#status, "resolved" :: Text)
            |> fetch
    forM_ (filter (resolvedBefore cutoff) stale) \alert ->
        void (autoCloseAlert alert "auto-closed: resolved TTL expired")
  where
    resolvedBefore cutoff alert = case alert.resolvedAt of
        Just resolvedAt -> resolvedAt < cutoff
        Nothing -> False

-- | Stall detection: firing/ack alerts whose last_seen_at is older than the
-- stall TTL never got a refire/resolve from their source (deleted trigger,
-- removed grafana rule, dead webhook). Alerts of a source with consecutive
-- poll failures are SKIPPED — a dead source must not mass-stall its alerts;
-- the source-health alert already covers that outage.
stallStaleAlerts :: (?modelContext :: ModelContext) => IO ()
stallStaleAlerts = do
    ttlSeconds <- stallTtlSeconds
    now <- getCurrentTime
    let cutoff = addUTCTime (fromIntegral (-ttlSeconds)) now
    stale <-
        query @Alert
            |> filterWhereIn (#status, ["firing", "ack"] :: [Text])
            |> fetch
    sources <- query @Source |> fetch
    let failing = Set.fromList [get #id source | source <- sources, source.consecutiveFailures > 0]
        overdue alert =
            alert.lastSeenAt < cutoff
                && maybe True (\sourceId -> Set.notMember sourceId failing) alert.sourceId
    forM_ (filter overdue stale) \alert ->
        void (stallAlert alert "no update from source within stall TTL")

-- | Stalled zabbix alerts whose trigger is STILL in problem state are
-- refired back to firing. A standing problem produces no new events for
-- the cursor-based event.get, so stallStaleAlerts ages these alerts out
-- even though zabbix still reports the trigger in problem — without this
-- step they would sit stalled until the stalled-close TTL. The inverse
-- direction (stalled alert whose trigger LEFT problem state -> resolved)
-- already runs per poll cycle in PollZabbix.reconcileProblemStates.
-- Sources that are disabled or have consecutive poll failures are skipped
-- (mirror of stallStaleAlerts: a dead source must not drive state
-- changes). Runs every AutoClose tick (300s); a refired alert gets its
-- last_seen_at touched by the transition, so the cycle repeats at
-- stallTtl intervals for as long as the trigger stays in problem.
reconcileStalledZabbix :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => IO ()
reconcileStalledZabbix = do
    stalled <-
        query @Alert
            |> filterWhere (#status, "stalled" :: Text)
            |> fetch
    sources <- query @Source |> fetch
    let sourceById = Map.fromList [(get #id source, source) | source <- sources]
        candidates =
            [ (source, triggerId, alert)
            | alert <- stalled
            , Just sourceId <- [alert.sourceId]
            , Just source <- [Map.lookup sourceId sourceById]
            , source.type_ == "zabbix"
            , source.enabled
            , source.consecutiveFailures == 0
            , Just triggerId <- [triggerIdOf alert]
            ]
    forM_ (groupBySource candidates) \(source, tracked) -> do
        let tokenEnv :: Maybe Text
            tokenEnv = parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: "tokenEnv")) source.config
        token <- case tokenEnv of
            Just envVar -> fmap cs <$> lookupEnv (cs envVar)
            Nothing -> pure Nothing
        case token of
            Nothing -> logDebug ("zabbix stalled-alert reconcile for source \"" <> source.name <> "\": token env var " <> fromMaybe "<none configured>" tokenEnv <> " not set; skipping")
            Just token -> do
                result <- Zabbix.triggerStateGet source.baseUrl token (nub (map fst tracked))
                case result of
                    Left err -> logWarn ("zabbix stalled-alert reconcile for source \"" <> source.name <> "\" failed: " <> err)
                    Right states -> do
                        let stillProblem = Set.fromList [state.triggerStateId | state <- states, state.triggerStateValue == "1"]
                        forM_ tracked \(triggerId, alert) -> do
                            now <- getCurrentTime
                            blackouts <- fetchActiveBlackouts now
                            let suppressedNow = any (blackoutApplies now (alertSubject alert)) blackouts
                            when (Set.member triggerId stillProblem) do
                                -- Firing => Refire trigger: (Stalled, Refire) is a
                                -- legal transition back to firing.
                                updated <- transitionAlert now Firing alert.env alert.environmentId alert.hostId alert.serviceId suppressedNow alert
                                when (updated.status == "firing") do
                                    logInfo ("zabbix stalled-alert reconcile: refired alert " <> tshow (get #id alert) <> " (trigger " <> triggerId <> " still in problem state)")
  where
    groupBySource candidates =
        Map.elems (Map.fromListWith (\(source, a) (_, b) -> (source, a ++ b)) [(get #id source, (source, [(triggerId, alert)])) | (source, triggerId, alert) <- candidates])

closeStalledAlerts :: (?modelContext :: ModelContext) => IO ()
closeStalledAlerts = do
    ttlSeconds <- stalledCloseTtlSeconds
    now <- getCurrentTime
    let cutoff = addUTCTime (fromIntegral (-ttlSeconds)) now
    stalled <-
        query @Alert
            |> filterWhere (#status, "stalled" :: Text)
            |> fetch
    forM_ (filter (expired cutoff) stalled) \alert ->
        void (autoCloseAlert alert "auto-closed: stalled TTL expired")
  where
    expired cutoff alert = alert.updatedAt < cutoff && alert.lastSeenAt < cutoff

unackExpiredAcks :: (?modelContext :: ModelContext) => IO ()
unackExpiredAcks = do
    now <- getCurrentTime
    acked <-
        query @Alert
            |> filterWhere (#status, "ack" :: Text)
            |> fetch
    forM_ (filter (expired now) acked) \alert ->
        void (unackAlert Nothing alert "ack timeout expired")
  where
    expired now alert = case alert.ackExpiresAt of
        Just expiresAt -> expiresAt <= now
        Nothing -> False

unsuppressExpired :: (?modelContext :: ModelContext) => IO ()
unsuppressExpired = do
    now <- getCurrentTime
    suppressed <-
        query @Alert
            |> filterWhere (#suppressed, True)
            |> fetch
    activeBlackouts <-
        query @Blackout
            |> filterWhereSql (#startsAt, "<= NOW()")
            |> filterWhereSql (#endsAt, "> NOW()")
            |> fetch
    -- Source-owned muting (suppressed_by = 'source') is cleared only by the
    -- source's unsuppress action, never by blackout expiry. Legacy NULL
    -- counts as blackout-owned.
    let blackoutOwned alert = alert.suppressedBy /= Just "source"
    forM_ (filter blackoutOwned suppressed) \alert -> do
        let covered = any (blackoutApplies now (alertSubject alert)) activeBlackouts
        unless covered do
            updated <-
                alert
                    |> set #suppressed False
                    |> set #suppressedBy Nothing
                    |> set #updatedAt now
                    |> updateRecord
            _ <-
                newRecord @AlertEvent
                    |> set #alertId (get #id alert)
                    |> set #userId Nothing
                    |> set #kind "unsuppressed"
                    |> createRecord
            publishAlertUpdate updated "unsuppressed"
