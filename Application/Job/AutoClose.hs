module Application.Job.AutoClose where

import IHP.Prelude
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Generated.Types
import Application.Pipeline.Actions (unackAlert, autoCloseAlert, stallAlert)
import Application.Pipeline.Blackouts (blackoutApplies)
import Application.Helper.Ingest (publishAlertUpdate)
import Control.Monad (void)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import qualified Data.Set as Set

-- Periodic maintenance (design_docs/milestone_1.md §9): auto-close resolved
-- alerts after TTL, unack expired acks, clear suppression once the covering
-- blackout expired, stall alerts that stopped receiving source updates, and
-- auto-close stalled alerts after their own TTL. Self-rescheduling like
-- PollZabbixJob.
instance Job AutoCloseJob where
    perform _job = do
        autoCloseResolved
        unackExpiredAcks
        unsuppressExpired
        stallStaleAlerts
        closeStalledAlerts

        now <- getCurrentTime
        next <- newRecord @AutoCloseJob
            |> set #runAt (addUTCTime 300 now)
            |> createRecord
        let nextId = get #id next
        _ <- sqlExecTyped [typedSql|
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
    cutoff <- addUTCTime (fromIntegral (- ttlSeconds)) <$> getCurrentTime
    stale <- query @Alert
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
    let cutoff = addUTCTime (fromIntegral (- ttlSeconds)) now
    stale <- query @Alert
        |> filterWhereIn (#status, ["firing", "ack"] :: [Text])
        |> fetch
    sources <- query @Source |> fetch
    let failing = Set.fromList [get #id source | source <- sources, source.consecutiveFailures > 0]
        overdue alert = alert.lastSeenAt < cutoff
            && maybe True (\sourceId -> Set.notMember sourceId failing) alert.sourceId
    forM_ (filter overdue stale) \alert ->
        void (stallAlert alert "no update from source within stall TTL")

closeStalledAlerts :: (?modelContext :: ModelContext) => IO ()
closeStalledAlerts = do
    ttlSeconds <- stalledCloseTtlSeconds
    now <- getCurrentTime
    let cutoff = addUTCTime (fromIntegral (- ttlSeconds)) now
    stalled <- query @Alert
        |> filterWhere (#status, "stalled" :: Text)
        |> fetch
    forM_ (filter (expired cutoff) stalled) \alert ->
        void (autoCloseAlert alert "auto-closed: stalled TTL expired")
    where
        expired cutoff alert = alert.updatedAt < cutoff && alert.lastSeenAt < cutoff

unackExpiredAcks :: (?modelContext :: ModelContext) => IO ()
unackExpiredAcks = do
    now <- getCurrentTime
    acked <- query @Alert
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
    suppressed <- query @Alert
        |> filterWhere (#suppressed, True)
        |> fetch
    activeBlackouts <- query @Blackout
        |> filterWhereSql (#startsAt, "<= NOW()")
        |> filterWhereSql (#endsAt, "> NOW()")
        |> fetch
    forM_ suppressed \alert -> do
        let covered = any (blackoutApplies now alert.environmentId alert.hostId alert.serviceId) activeBlackouts
        unless covered do
            updated <- alert
                |> set #suppressed False
                |> set #updatedAt now
                |> updateRecord
            _ <- newRecord @AlertEvent
                |> set #alertId (get #id alert)
                |> set #userId Nothing
                |> set #kind "unsuppressed"
                |> createRecord
            publishAlertUpdate updated "unsuppressed"
