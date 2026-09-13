module Web.Controller.Reports where

import qualified Application.Service.Reports as Reports
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime (..))
import qualified Data.Time.Format as TimeFormat
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Text.Read (readMaybe)
import Web.Controller.Prelude
import Web.View.Reports.Index

-- Static SVG reports (charts rendered server-side with diagrams, embedded
-- inline). On-demand only; no live updates by design.
instance Controller ReportsController where
    beforeAction = ensureIsUser

    action ReportsAction = do
        now <- getCurrentTime
        let windowHours = clampWindow (intParam 168 "windowHours")
            from = addUTCTime (negate (fromIntegral windowHours * 3600)) now
            envFilter = fromMaybe "" (paramOrNothing @Text "env")
            selectedEnv = if envFilter == "" then Nothing else Just envFilter
            volumeBucket = case paramOrNothing @Text "bucket" of
                Just bucket | bucket `elem` ["hour", "day"] -> bucket
                _ -> ""
            bucketKind = if volumeBucket == "" then (if windowHours == 24 then "hour" else "day") else volumeBucket
        -- Selector options: every effective env ever seen (unbounded by window)
        envRows <-
            sqlQueryTyped
                [typedSql|
            SELECT DISTINCT coalesce(nullif(a.facets ->> 'env', ''), a.env) AS env
            FROM alerts a
            WHERE coalesce(nullif(a.facets ->> 'env', ''), a.env) IS NOT NULL
            ORDER BY 1 |]
        severityRows <-
            sqlQueryTyped
                [typedSql|
            SELECT a.severity, count(*) AS n
            FROM alerts a
            WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                AND (${envFilter} = '' OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${envFilter})
            GROUP BY a.severity
            ORDER BY n DESC |]
        -- No env selected: breakdown by environment. One env selected:
        -- breakdown by host inside that environment.
        breakdownRows <- case selectedEnv of
            Nothing ->
                sqlQueryTyped
                    [typedSql|
                SELECT coalesce(nullif(a.facets ->> 'env', ''), a.env) AS label, count(*) AS n
                FROM alerts a
                WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                    AND coalesce(nullif(a.facets ->> 'env', ''), a.env) IS NOT NULL
                GROUP BY 1
                ORDER BY n DESC
                LIMIT 12 |]
            Just _ ->
                sqlQueryTyped
                    [typedSql|
                SELECT a.host AS label, count(*) AS n
                FROM alerts a
                WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                    AND a.host IS NOT NULL
                    AND coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${envFilter}
                GROUP BY a.host
                ORDER BY n DESC
                LIMIT 12 |]
        -- Bucket granularity picked via the `bucket` param; default follows
        -- the window (24h -> hour, longer -> day)
        volumeRows <-
            sqlQueryTyped
                [typedSql|
            SELECT date_trunc(${bucketKind}::text, coalesce(a.started_at, a.first_seen_at)) AS bucket, count(*) AS n
            FROM alerts a
            WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                AND (${envFilter} = '' OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${envFilter})
            GROUP BY 1
            ORDER BY 1 |]
        mttrRows <-
            sqlQueryTyped
                [typedSql|
            SELECT a.severity, avg(extract(epoch from (a.resolved_at - a.started_at)))::float8 AS avg_seconds
            FROM alerts a
            WHERE a.resolved_at IS NOT NULL AND a.started_at IS NOT NULL
                AND a.resolved_at >= a.started_at
                AND coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                AND (${envFilter} = '' OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${envFilter})
            GROUP BY a.severity
            ORDER BY avg_seconds DESC |]
        let severitySvg = Reports.severityChartSvg (map (\row -> (get #severity row, get #n row)) severityRows)
            envs = case selectedEnv of
                -- keep a manually-typed/unknown selection visible in the dropdown
                Just env | env `notElem` knownEnvs -> env : knownEnvs
                _ -> knownEnvs
            knownEnvs = [env | Just env <- envRows]
            breakdownSvg = Reports.envChartSvg (map (\row -> (fromMaybe "unknown" (get #label row), get #n row)) breakdownRows)
            breakdownTitle = if isJust selectedEnv then "Alerts by host" else "Alerts by environment"
            bucketLabel = cs . TimeFormat.formatTime TimeFormat.defaultTimeLocale (if bucketKind == "hour" then "%H:%M" else "%m-%d")
            counts = Map.fromList (mapMaybe (\row -> (,get #n row) <$> get #bucket row) volumeRows)
            stepSeconds = if bucketKind == "hour" then 3600 else 86400
            buckets = takeWhile (< now) (iterate (addUTCTime stepSeconds) (truncateBucket stepSeconds from))
            volumeSvg = Reports.volumeChartSvg [(bucketLabel bucket, Map.findWithDefault 0 bucket counts) | bucket <- buckets]
            volumeTitle = if bucketKind == "hour" then "Alert volume per hour" else "Alert volume per day"
            mttrSvg = Reports.mttrChartSvg (map (\row -> (get #severity row, fromMaybe 0 (get #avg_seconds row))) mttrRows)
        render IndexView{..}

-- Align a timestamp down to its bucket boundary (hour for the 24h window,
-- day otherwise) so chart slots are positional in time, not in row order.
truncateBucket :: NominalDiffTime -> UTCTime -> UTCTime
truncateBucket step t = t{utctDayTime = fromIntegral (seconds `div` stepSeconds * stepSeconds)}
  where
    seconds = floor (utctDayTime t) :: Integer
    stepSeconds = floor step :: Integer

clampWindow :: Int -> Int
clampWindow hours
    | hours `elem` [24, 168, 720] = hours
    | otherwise = 168

intParam :: (?request :: Request) => Int -> ByteString -> Int
intParam fallback name = fromMaybe fallback (paramOrNothing @Text name >>= readMaybe . cs)
