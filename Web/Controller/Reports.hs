module Web.Controller.Reports where

import Web.Controller.Prelude
import Web.View.Reports.Index
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Text.Read (readMaybe)
import qualified Application.Service.Reports as Reports

-- Static SVG reports (charts rendered server-side with diagrams, embedded
-- inline). On-demand only; no live updates by design.
instance Controller ReportsController where
    beforeAction = ensureIsUser

    action ReportsAction = do
        now <- getCurrentTime
        let windowHours = clampWindow (intParam 168 "windowHours")
            from = addUTCTime (negate (fromIntegral windowHours * 3600)) now
        severityRows <- sqlQueryTyped [typedSql|
            SELECT a.severity, count(*) AS n
            FROM alerts a
            WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
            GROUP BY a.severity
            ORDER BY n DESC |]
        envRows <- sqlQueryTyped [typedSql|
            SELECT coalesce(nullif(a.facets ->> 'env', ''), a.env) AS env, count(*) AS n
            FROM alerts a
            WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                AND coalesce(nullif(a.facets ->> 'env', ''), a.env) IS NOT NULL
            GROUP BY 1
            ORDER BY n DESC
            LIMIT 12 |]
        volumeRows <- sqlQueryTyped [typedSql|
            SELECT date_trunc('day', coalesce(a.started_at, a.first_seen_at))::date AS day, count(*) AS n
            FROM alerts a
            WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
            GROUP BY 1
            ORDER BY 1 |]
        mttrRows <- sqlQueryTyped [typedSql|
            SELECT a.severity, avg(extract(epoch from (a.resolved_at - a.started_at)))::float8 AS avg_seconds
            FROM alerts a
            WHERE a.resolved_at IS NOT NULL AND a.started_at IS NOT NULL
                AND a.resolved_at >= a.started_at
                AND coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
            GROUP BY a.severity
            ORDER BY avg_seconds DESC |]
        let severitySvg = Reports.severityChartSvg (map (\row -> (get #severity row, get #n row)) severityRows)
            envSvg = Reports.envChartSvg (map (\row -> (fromMaybe "unknown" (get #env row), get #n row)) envRows)
            volumeSvg = Reports.volumeChartSvg (mapMaybe (\row -> (, get #n row) <$> get #day row) volumeRows)
            mttrSvg = Reports.mttrChartSvg (map (\row -> (get #severity row, fromMaybe 0 (get #avg_seconds row))) mttrRows)
        render IndexView { .. }

clampWindow :: Int -> Int
clampWindow hours
    | hours `elem` [24, 168, 720] = hours
    | otherwise = 168

intParam :: (?request :: Request) => Int -> ByteString -> Int
intParam fallback name = fromMaybe fallback (paramOrNothing @Text name >>= readMaybe . cs)
