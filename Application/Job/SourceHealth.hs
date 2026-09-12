module Application.Job.SourceHealth where

import Application.Service.SourceHealth (expectedIntervalSeconds, recordSilence)
import Control.Monad (void)
import Generated.Types
import IHP.Fetch (fetch)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)

-- Webhook silence detection (design_docs/milestone_5.md §4): a push source
-- with an expectedIntervalSeconds config that saw no raw event for 3x that
-- interval is treated as failed. Push source types only: raw_events rows are
-- written exclusively by the webhook controller, so a poll source (zabbix,
-- grafana) with the key set would fall back to sources.created_at and
-- false-fire forever. Lightweight periodic check, self-rescheduling like the
-- pollers. Poll-based backoff lives in the pollers themselves.
instance Job SourceHealthJob where
    perform _job = do
        checkSilence

        now <- getCurrentTime
        next <-
            newRecord @SourceHealthJob
                |> set #runAt (addUTCTime 30 now)
                |> createRecord
        let nextId = get #id next
        _ <-
            sqlExecTyped
                [typedSql|
            DELETE FROM source_health_jobs
            WHERE status = 'job_status_not_started' AND id <> ${nextId}
        |]
        pure ()

    queuePollInterval = 5 * 1000000
    maxAttempts = 3

-- | Source types that receive inbound pushes and therefore produce
-- raw_events rows; silence detection is meaningless for poll sources.
pushSourceTypes :: [Text]
pushSourceTypes = ["alertmanager", "webhook"]

checkSilence :: (?modelContext :: ModelContext) => IO ()
checkSilence = do
    sources <-
        query @Source
            |> filterWhere (#enabled, True)
            |> filterWhereIn (#type_, pushSourceTypes)
            |> fetch
    now <- getCurrentTime
    forM_ sources \source -> forM_ (expectedIntervalSeconds source) \expected -> do
        let sourceId = get #id source
        rows <-
            sqlQueryTyped
                [typedSql|
            SELECT max(received_at) AS last_received FROM raw_events WHERE source_id = ${sourceId}
        |]
        let baseline = case rows of
                (lastReceived : _) -> fromMaybe source.createdAt lastReceived
                [] -> source.createdAt
            silenceSeconds = nominalDiffTimeToSeconds (diffUTCTime now baseline)
        when (silenceSeconds > fromIntegral (3 * expected)) do
            recordSilence source ("no inbound events for " <> tshow (round silenceSeconds :: Int) <> "s (expected every " <> tshow expected <> "s)")
