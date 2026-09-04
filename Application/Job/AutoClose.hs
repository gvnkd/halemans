module Application.Job.AutoClose where

import IHP.Prelude
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Generated.Types
import Application.Pipeline.Actions (unackAlert, closeAlert)
import Application.Pipeline.Blackouts (blackoutApplies)
import Application.Helper.Ingest (publishAlertUpdate)
import Control.Monad (void)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- Periodic maintenance (design_docs/milestone_1.md §9): auto-close resolved
-- alerts after TTL, unack expired acks, clear suppression once the covering
-- blackout expired. Self-rescheduling like PollZabbixJob.
instance Job AutoCloseJob where
    perform _job = do
        autoCloseResolved
        unackExpiredAcks
        unsuppressExpired

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

autoCloseTtlSeconds :: IO Int
autoCloseTtlSeconds = do
    override <- lookupEnv "HALEMANS_AUTO_CLOSE_SECONDS"
    pure (fromMaybe 86400 (override >>= readMaybe))

autoCloseResolved :: (?modelContext :: ModelContext) => IO ()
autoCloseResolved = do
    ttlSeconds <- autoCloseTtlSeconds
    cutoff <- addUTCTime (fromIntegral (- ttlSeconds)) <$> getCurrentTime
    stale <- query @Alert
        |> filterWhere (#status, "resolved" :: Text)
        |> fetch
    forM_ (filter (resolvedBefore cutoff) stale) \alert ->
        void (closeAlert Nothing alert (Just "auto-closed: resolved TTL expired"))
    where
        resolvedBefore cutoff alert = case alert.resolvedAt of
            Just resolvedAt -> resolvedAt < cutoff
            Nothing -> False

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
