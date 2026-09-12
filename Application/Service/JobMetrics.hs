module Application.Service.JobMetrics where

import Data.Int (Int64)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.TypedSql (sqlQueryTyped, typedSql)

-- Job failure observability (design_docs/milestone_5.md §8): counters read
-- from the IHP job tables (no metrics store). Per-type 24h counters plus the
-- last 20 failed rows across all job types.

data JobTypeMetrics = JobTypeMetrics
    { jobType :: Text
    , failed :: Int64
    , retried :: Int64
    , succeeded :: Int64
    }

data FailedJobRow = FailedJobRow
    { failedJobType :: Text
    , failedJobId :: Text
    , failedJobError :: Maybe Text
    , failedJobUpdatedAt :: UTCTime
    }

jobTables :: [Text]
jobTables =
    [ "poll_zabbix_jobs"
    , "poll_grafana_jobs"
    , "auto_close_jobs"
    , "push_notification_jobs"
    , "escalation_jobs"
    , "enrich_alert_jobs"
    , "write_back_jobs"
    , "jira_sync_jobs"
    , "llm_analysis_jobs"
    , "retention_jobs"
    , "source_health_jobs"
    ]

jobTypeMetrics :: (?modelContext :: ModelContext) => IO [JobTypeMetrics]
jobTypeMetrics =
    sequence
        [ metricsFor
            "poll_zabbix_jobs"
            [typedSql|
        SELECT count(*) FILTER (WHERE status = 'job_status_failed') AS failed,
               count(*) FILTER (WHERE attempts_count > 0) AS retried,
               count(*) FILTER (WHERE status = 'job_status_succeeded') AS succeeded
        FROM poll_zabbix_jobs WHERE updated_at > NOW() - INTERVAL '24 hours' |]
        , metricsFor
            "poll_grafana_jobs"
            [typedSql|
        SELECT count(*) FILTER (WHERE status = 'job_status_failed') AS failed,
               count(*) FILTER (WHERE attempts_count > 0) AS retried,
               count(*) FILTER (WHERE status = 'job_status_succeeded') AS succeeded
        FROM poll_grafana_jobs WHERE updated_at > NOW() - INTERVAL '24 hours' |]
        , metricsFor
            "auto_close_jobs"
            [typedSql|
        SELECT count(*) FILTER (WHERE status = 'job_status_failed') AS failed,
               count(*) FILTER (WHERE attempts_count > 0) AS retried,
               count(*) FILTER (WHERE status = 'job_status_succeeded') AS succeeded
        FROM auto_close_jobs WHERE updated_at > NOW() - INTERVAL '24 hours' |]
        , metricsFor
            "push_notification_jobs"
            [typedSql|
        SELECT count(*) FILTER (WHERE status = 'job_status_failed') AS failed,
               count(*) FILTER (WHERE attempts_count > 0) AS retried,
               count(*) FILTER (WHERE status = 'job_status_succeeded') AS succeeded
        FROM push_notification_jobs WHERE updated_at > NOW() - INTERVAL '24 hours' |]
        , metricsFor
            "escalation_jobs"
            [typedSql|
        SELECT count(*) FILTER (WHERE status = 'job_status_failed') AS failed,
               count(*) FILTER (WHERE attempts_count > 0) AS retried,
               count(*) FILTER (WHERE status = 'job_status_succeeded') AS succeeded
        FROM escalation_jobs WHERE updated_at > NOW() - INTERVAL '24 hours' |]
        , metricsFor
            "enrich_alert_jobs"
            [typedSql|
        SELECT count(*) FILTER (WHERE status = 'job_status_failed') AS failed,
               count(*) FILTER (WHERE attempts_count > 0) AS retried,
               count(*) FILTER (WHERE status = 'job_status_succeeded') AS succeeded
        FROM enrich_alert_jobs WHERE updated_at > NOW() - INTERVAL '24 hours' |]
        , metricsFor
            "write_back_jobs"
            [typedSql|
        SELECT count(*) FILTER (WHERE status = 'job_status_failed') AS failed,
               count(*) FILTER (WHERE attempts_count > 0) AS retried,
               count(*) FILTER (WHERE status = 'job_status_succeeded') AS succeeded
        FROM write_back_jobs WHERE updated_at > NOW() - INTERVAL '24 hours' |]
        , metricsFor
            "jira_sync_jobs"
            [typedSql|
        SELECT count(*) FILTER (WHERE status = 'job_status_failed') AS failed,
               count(*) FILTER (WHERE attempts_count > 0) AS retried,
               count(*) FILTER (WHERE status = 'job_status_succeeded') AS succeeded
        FROM jira_sync_jobs WHERE updated_at > NOW() - INTERVAL '24 hours' |]
        , metricsFor
            "llm_analysis_jobs"
            [typedSql|
        SELECT count(*) FILTER (WHERE status = 'job_status_failed') AS failed,
               count(*) FILTER (WHERE attempts_count > 0) AS retried,
               count(*) FILTER (WHERE status = 'job_status_succeeded') AS succeeded
        FROM llm_analysis_jobs WHERE updated_at > NOW() - INTERVAL '24 hours' |]
        , metricsFor
            "retention_jobs"
            [typedSql|
        SELECT count(*) FILTER (WHERE status = 'job_status_failed') AS failed,
               count(*) FILTER (WHERE attempts_count > 0) AS retried,
               count(*) FILTER (WHERE status = 'job_status_succeeded') AS succeeded
        FROM retention_jobs WHERE updated_at > NOW() - INTERVAL '24 hours' |]
        , metricsFor
            "source_health_jobs"
            [typedSql|
        SELECT count(*) FILTER (WHERE status = 'job_status_failed') AS failed,
               count(*) FILTER (WHERE attempts_count > 0) AS retried,
               count(*) FILTER (WHERE status = 'job_status_succeeded') AS succeeded
        FROM source_health_jobs WHERE updated_at > NOW() - INTERVAL '24 hours' |]
        ]
  where
    metricsFor name query = do
        rows <- sqlQueryTyped query
        pure case rows of
            (row : _) -> JobTypeMetrics name (get #failed row) (get #retried row) (get #succeeded row)
            [] -> JobTypeMetrics name 0 0 0

recentFailedJobs :: (?modelContext :: ModelContext) => IO [FailedJobRow]
recentFailedJobs = do
    all <-
        concat
            <$> sequence
                [ failuresFor
                    "poll_zabbix_jobs"
                    [typedSql|
            SELECT id, last_error, updated_at FROM poll_zabbix_jobs
            WHERE status = 'job_status_failed' ORDER BY updated_at DESC LIMIT 20 |]
                , failuresFor
                    "poll_grafana_jobs"
                    [typedSql|
            SELECT id, last_error, updated_at FROM poll_grafana_jobs
            WHERE status = 'job_status_failed' ORDER BY updated_at DESC LIMIT 20 |]
                , failuresFor
                    "auto_close_jobs"
                    [typedSql|
            SELECT id, last_error, updated_at FROM auto_close_jobs
            WHERE status = 'job_status_failed' ORDER BY updated_at DESC LIMIT 20 |]
                , failuresFor
                    "push_notification_jobs"
                    [typedSql|
            SELECT id, last_error, updated_at FROM push_notification_jobs
            WHERE status = 'job_status_failed' ORDER BY updated_at DESC LIMIT 20 |]
                , failuresFor
                    "escalation_jobs"
                    [typedSql|
            SELECT id, last_error, updated_at FROM escalation_jobs
            WHERE status = 'job_status_failed' ORDER BY updated_at DESC LIMIT 20 |]
                , failuresFor
                    "enrich_alert_jobs"
                    [typedSql|
            SELECT id, last_error, updated_at FROM enrich_alert_jobs
            WHERE status = 'job_status_failed' ORDER BY updated_at DESC LIMIT 20 |]
                , failuresFor
                    "write_back_jobs"
                    [typedSql|
            SELECT id, last_error, updated_at FROM write_back_jobs
            WHERE status = 'job_status_failed' ORDER BY updated_at DESC LIMIT 20 |]
                , failuresFor
                    "jira_sync_jobs"
                    [typedSql|
            SELECT id, last_error, updated_at FROM jira_sync_jobs
            WHERE status = 'job_status_failed' ORDER BY updated_at DESC LIMIT 20 |]
                , failuresFor
                    "llm_analysis_jobs"
                    [typedSql|
            SELECT id, last_error, updated_at FROM llm_analysis_jobs
            WHERE status = 'job_status_failed' ORDER BY updated_at DESC LIMIT 20 |]
                , failuresFor
                    "retention_jobs"
                    [typedSql|
            SELECT id, last_error, updated_at FROM retention_jobs
            WHERE status = 'job_status_failed' ORDER BY updated_at DESC LIMIT 20 |]
                , failuresFor
                    "source_health_jobs"
                    [typedSql|
            SELECT id, last_error, updated_at FROM source_health_jobs
            WHERE status = 'job_status_failed' ORDER BY updated_at DESC LIMIT 20 |]
                ]
    pure (take 20 (sortOn (Down . failedJobUpdatedAt) all))
  where
    failuresFor jobType query = do
        rows <- sqlQueryTyped query
        pure (map (\row -> FailedJobRow jobType (tshow (get #id row)) (get #last_error row) (get #updated_at row)) rows)
