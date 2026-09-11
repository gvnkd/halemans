module Application.Job.Retention where

import IHP.Prelude
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetchOneOrNothing)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Generated.Types
import Data.Int (Int64)
import Control.Monad (void)

-- Retention (design_docs/milestone_5.md §3): daily batched prune of
-- raw_events older than retention_configs.raw_events_days. Batches commit
-- independently so an interrupted run resumes where it stopped; the job is
-- self-rescheduling like JiraSyncJob. enabled=false logs and exits.
instance Job RetentionJob where
    perform _job = do
        pruneTerminalJobRows
        maybeConfig <- query @RetentionConfig
            |> orderByDesc #updatedAt
            |> fetchOneOrNothing
        case maybeConfig of
            Nothing -> putStrLn ("retention: no retention_configs row, skipping" :: Text)
            Just config
                | not config.enabled -> putStrLn ("retention: disabled in retention_configs, skipping" :: Text)
                | otherwise -> pruneRawEvents config

        now <- getCurrentTime
        next <- newRecord @RetentionJob
            |> set #runAt (addUTCTime 86400 now)
            |> createRecord
        let nextId = get #id next
        _ <- sqlExecTyped [typedSql|
            DELETE FROM retention_jobs
            WHERE status = 'job_status_not_started' AND id <> ${nextId}
        |]
        pure ()

    queuePollInterval = 30 * 1000000
    maxAttempts = 3

pruneRawEvents :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => RetentionConfig -> IO ()
pruneRawEvents config = do
    startedAt <- getCurrentTime
    let cutoff = addUTCTime (fromIntegral (-config.rawEventsDays) * 86400) startedAt
    (batches, rows) <- deleteBatches cutoff 0 0
    now <- getCurrentTime
    _ <- config
        |> set #lastRunAt (Just now)
        |> updateRecord
    putStrLn ("retention: pruned " <> tshow rows <> " raw_events in " <> tshow batches <> " batches (" <> tshow (diffUTCTime now startedAt) <> ")" :: Text)
    pure ()
  where
    deleteBatches cutoff batches rows = do
        deleted <- deleteBatch cutoff
        if deleted == 0
            then pure (batches :: Int, rows)
            else deleteBatches cutoff (batches + 1) (rows + deleted)

-- | One committed batch. The inner SELECT keeps the giant first-run prune
-- from holding one transaction (milestone_5.md §13).
deleteBatch :: (?modelContext :: ModelContext) => UTCTime -> IO Int64
deleteBatch cutoff = sqlExecTyped [typedSql|
    DELETE FROM raw_events
    WHERE id IN (SELECT id FROM raw_events WHERE received_at < ${cutoff} LIMIT 1000)
|]

-- One-shot event-job rows are never removed by the job runner (it marks
-- them succeeded/failed in place), so tables like enrich_alert_jobs grow
-- unbounded — prune terminal rows older than a day, unconditionally
-- (independent of the raw_events retention config).
pruneTerminalJobRows :: (?modelContext :: ModelContext) => IO ()
pruneTerminalJobRows = do
    cutoff <- addUTCTime (-86400) <$> getCurrentTime
    counts <- mapM (pruneTable cutoff)
        [ pruneEnrichAlertJobs, pruneWriteBackJobs
        , prunePushNotificationJobs, pruneLlmAnalysisJobs
        ]
    let total = sum counts
    when (total > 0) do
        putStrLn ("retention: pruned " <> tshow total <> " terminal job rows" :: Text)
  where
    pruneTable cutoff prune = do
        deleted <- prune cutoff
        rest <- if deleted == 0 then pure 0 else pruneTable cutoff prune
        pure (deleted + rest)

pruneEnrichAlertJobs :: (?modelContext :: ModelContext) => UTCTime -> IO Int64
pruneEnrichAlertJobs cutoff = sqlExecTyped [typedSql|
    DELETE FROM enrich_alert_jobs
    WHERE id IN (SELECT id FROM enrich_alert_jobs
        WHERE updated_at < ${cutoff}
        AND status::text IN ('job_status_succeeded', 'job_status_failed', 'job_status_timed_out')
        LIMIT 1000)
|]

pruneWriteBackJobs :: (?modelContext :: ModelContext) => UTCTime -> IO Int64
pruneWriteBackJobs cutoff = sqlExecTyped [typedSql|
    DELETE FROM write_back_jobs
    WHERE id IN (SELECT id FROM write_back_jobs
        WHERE updated_at < ${cutoff}
        AND status::text IN ('job_status_succeeded', 'job_status_failed', 'job_status_timed_out')
        LIMIT 1000)
|]

prunePushNotificationJobs :: (?modelContext :: ModelContext) => UTCTime -> IO Int64
prunePushNotificationJobs cutoff = sqlExecTyped [typedSql|
    DELETE FROM push_notification_jobs
    WHERE id IN (SELECT id FROM push_notification_jobs
        WHERE updated_at < ${cutoff}
        AND status::text IN ('job_status_succeeded', 'job_status_failed', 'job_status_timed_out')
        LIMIT 1000)
|]

pruneLlmAnalysisJobs :: (?modelContext :: ModelContext) => UTCTime -> IO Int64
pruneLlmAnalysisJobs cutoff = sqlExecTyped [typedSql|
    DELETE FROM llm_analysis_jobs
    WHERE id IN (SELECT id FROM llm_analysis_jobs
        WHERE updated_at < ${cutoff}
        AND status::text IN ('job_status_succeeded', 'job_status_failed', 'job_status_timed_out')
        LIMIT 1000)
|]
