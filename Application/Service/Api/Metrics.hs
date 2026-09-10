module Application.Service.Api.Metrics
    ( MetricSample (..)
    , renderFamily
    , renderMetrics
    , escapeLabel
    , halemansVersion
    , collectMetrics
    ) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder (query)
import IHP.Fetch (fetch)
import IHP.TypedSql (sqlQueryTyped, typedSql)
import IHP.TypedSql.RowType (SqlRow)
import Generated.Types
import qualified Data.Text as Text
import Data.Int (Int64)
import Application.Service.Live (liveConnectionCount)

halemansVersion :: Text
halemansVersion = "1.1.0"

data MetricSample = MetricSample
    { sampleName :: !Text
    , sampleLabels :: ![(Text, Text)]
    , sampleValue :: !Text
    } deriving (Eq, Show)

-- Prometheus text exposition format, hand-rolled (design_docs/milestone_6.md §5).
renderMetrics :: [MetricSample] -> Text
renderMetrics samples = Text.concat (map renderSample samples)

renderFamily :: Text -> [MetricSample] -> Text
renderFamily name samples = "# TYPE " <> name <> " gauge\n" <> renderMetrics samples

renderSample :: MetricSample -> Text
renderSample sample = sample.sampleName <> renderLabels sample.sampleLabels <> " " <> sample.sampleValue <> "\n"

renderLabels :: [(Text, Text)] -> Text
renderLabels [] = ""
renderLabels labels = "{" <> Text.intercalate "," [key <> "=\"" <> escapeLabel value <> "\"" | (key, value) <- labels] <> "}"

escapeLabel :: Text -> Text
escapeLabel = Text.concatMap \case
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    char -> Text.singleton char

sample :: Text -> [(Text, Text)] -> Text -> MetricSample
sample = MetricSample

type AlertCountRow = SqlRow '[ '("environment", Text), '("status", Text), '("severity", Text), '("count", Int64)]
type JobCountRow = SqlRow '[ '("job", Text), '("status", Text), '("count", Int64)]
type LlmCounterRow = SqlRow '[ '("provider", Text), '("tokens_in", Int64), '("tokens_out", Int64)]

-- Gauges computed from durable tables at scrape time (design §5): restarts
-- can't zero the series and the DB stays the single source of truth.
collectMetrics :: (?modelContext :: ModelContext) => IO Text
collectMetrics = do
    alertCounts <- sqlQueryTyped [typedSql|
        SELECT coalesce(nullif(a.facets ->> 'env', ''), a.env, 'unassigned') AS environment, a.status, a.severity, count(*) AS count
        FROM alerts a
        WHERE a.status <> 'closed'
        GROUP BY coalesce(nullif(a.facets ->> 'env', ''), a.env, 'unassigned'), a.status, a.severity
    |]
    sources <- query @Source |> fetch
    jobCounts <- sqlQueryTyped [typedSql|
        SELECT 'poll_zabbix' AS job, status::text AS status, count(*) AS count FROM poll_zabbix_jobs GROUP BY status
        UNION ALL SELECT 'poll_grafana', status::text, count(*) FROM poll_grafana_jobs GROUP BY status
        UNION ALL SELECT 'auto_close', status::text, count(*) FROM auto_close_jobs GROUP BY status
        UNION ALL SELECT 'push_notification', status::text, count(*) FROM push_notification_jobs GROUP BY status
        UNION ALL SELECT 'escalation', status::text, count(*) FROM escalation_jobs GROUP BY status
        UNION ALL SELECT 'enrich_alert', status::text, count(*) FROM enrich_alert_jobs GROUP BY status
        UNION ALL SELECT 'write_back', status::text, count(*) FROM write_back_jobs GROUP BY status
        UNION ALL SELECT 'jira_sync', status::text, count(*) FROM jira_sync_jobs GROUP BY status
        UNION ALL SELECT 'llm_analysis', status::text, count(*) FROM llm_analysis_jobs GROUP BY status
        UNION ALL SELECT 'retention', status::text, count(*) FROM retention_jobs GROUP BY status
        UNION ALL SELECT 'source_health', status::text, count(*) FROM source_health_jobs GROUP BY status
    |]
    llmCounters <- sqlQueryTyped [typedSql|
        SELECT provider, tokens_in, tokens_out
        FROM llm_budget_counters
        WHERE day = CURRENT_DATE
    |]
    wsCount <- liveConnectionCount
    pure $ Text.concat
        [ renderFamily "halemans_alerts"
            [ sample "halemans_alerts"
                [ ("environment", get #environment row)
                , ("status", get #status row)
                , ("severity", get #severity row)
                ] (tshow (get #count row))
            | row <- alertCounts
            ]
        , renderFamily "halemans_source_consecutive_failures"
            [ sample "halemans_source_consecutive_failures"
                [("source", source.name)] (tshow source.consecutiveFailures)
            | source <- sources
            ]
        , renderFamily "halemans_source_healthy"
            [ sample "halemans_source_healthy"
                [("source", source.name)] (if source.consecutiveFailures == 0 then "1" else "0")
            | source <- sources
            ]
        , renderFamily "halemans_job_runs_total"
            [ sample "halemans_job_runs_total"
                [ ("job", fromMaybe "" (get #job row))
                , ("status", fromMaybe "" (get #status row) |> Text.stripPrefix "job_status_" |> fromMaybe (fromMaybe "" (get #status row)))
                ] (tshow (fromMaybe 0 (get #count row)))
            | row <- jobCounts
            ]
        , renderFamily "halemans_llm_tokens_today" $ concat
            [ [ sample "halemans_llm_tokens_today"
                [("provider", get #provider row), ("direction", "in")] (tshow (get #tokens_in row))
              , sample "halemans_llm_tokens_today"
                [("provider", get #provider row), ("direction", "out")] (tshow (get #tokens_out row))
              ]
            | row <- llmCounters
            ]
        , renderFamily "halemans_ws_connections"
            [sample "halemans_ws_connections" [] (tshow wsCount)]
        , renderFamily "halemans_build_info"
            [sample "halemans_build_info" [("version", halemansVersion)] "1"]
        ]
