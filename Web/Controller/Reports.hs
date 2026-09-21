module Web.Controller.Reports where

import qualified Application.Service.Reports as Reports
import Application.Service.TimeRange (resolveTimeExpr)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime (..))
import qualified Data.Time.Format as TimeFormat
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Web.Controller.Prelude
import Web.View.Reports.Index

-- Static reports page (charts rendered server-side: one hand-rolled SVG for
-- the volume timeline, HTML bar rows for the horizontal charts). On-demand
-- only; no live updates by design.
instance Controller ReportsController where
    beforeAction = ensureIsUser

    action ReportsAction = do
        now <- getCurrentTime
        let
            -- from/to accept relative "now() - 7d" expressions or absolute
            -- timestamps; empty/invalid `from` falls back to 7d, `to` to now.
            -- The field is pre-filled so the Window preset select can match
            -- it client-side (the preset itself is never submitted)
            rangeFrom = fromMaybe "now() - 168h" (paramOrNothing @Text "from" >>= nonEmptyText)
            rangeTo = fromMaybe "" (paramOrNothing @Text "to" >>= nonEmptyText)
            from = fromMaybe (addUTCTime (negate (7 * 86400)) now) (resolveTimeExpr now rangeFrom)
            to = fromMaybe now (resolveTimeExpr now rangeTo)
            envFilter = fromMaybe "" (paramOrNothing @Text "env")
            selectedEnv = if envFilter == "" then Nothing else Just envFilter
            volumeBucket = case paramOrNothing @Text "bucket" of
                Just bucket | bucket `elem` ["hour", "day"] -> bucket
                _ -> ""
            bucketKind = if volumeBucket == "" then (if diffUTCTime to from <= 86400 then "hour" else "day") else volumeBucket
            severities = paramList @Text "severity"
        -- Selector options: every effective env ever seen (unbounded by window)
        envRows <-
            sqlQueryTyped
                [typedSql|
            SELECT DISTINCT coalesce(nullif(a.facets ->> 'env', ''), a.env) AS env
            FROM alerts a
            WHERE coalesce(nullif(a.facets ->> 'env', ''), a.env) IS NOT NULL
            ORDER BY 1 |]
        -- Severity selector options: every severity ever seen (unbounded by window)
        severityOptionRows <-
            sqlQueryTyped
                [typedSql|
            SELECT DISTINCT a.severity
            FROM alerts a
            ORDER BY 1 |]
        severityRows <-
            sqlQueryTyped
                [typedSql|
            SELECT a.severity, count(*) AS n
            FROM alerts a
            WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                AND coalesce(a.started_at, a.first_seen_at) < ${to}::timestamptz
                AND (${envFilter} = '' OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${envFilter})
                AND (cardinality(${severities}::text[]) = 0 OR a.severity = ANY(${severities}))
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
                AND coalesce(a.started_at, a.first_seen_at) < ${to}::timestamptz
                    AND coalesce(nullif(a.facets ->> 'env', ''), a.env) IS NOT NULL
                    AND (cardinality(${severities}::text[]) = 0 OR a.severity = ANY(${severities}))
                GROUP BY 1
                ORDER BY n DESC
                LIMIT 12 |]
            Just _ ->
                sqlQueryTyped
                    [typedSql|
                SELECT a.host AS label, count(*) AS n
                FROM alerts a
                WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                AND coalesce(a.started_at, a.first_seen_at) < ${to}::timestamptz
                    AND a.host IS NOT NULL
                    AND coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${envFilter}
                    AND (cardinality(${severities}::text[]) = 0 OR a.severity = ANY(${severities}))
                GROUP BY a.host
                ORDER BY n DESC
                LIMIT 12 |]
        -- Top sources card: alerts per source name over the window
        sourceRows <-
            sqlQueryTyped
                [typedSql|
            SELECT coalesce(s.name, 'unknown') AS label, count(*) AS n
            FROM alerts a
            LEFT JOIN sources s ON s.id = a.source_id
            WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                AND coalesce(a.started_at, a.first_seen_at) < ${to}::timestamptz
                AND (${envFilter} = '' OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${envFilter})
                AND (cardinality(${severities}::text[]) = 0 OR a.severity = ANY(${severities}))
            GROUP BY 1
            ORDER BY n DESC
            LIMIT 8 |]
        -- Bucket granularity picked via the `bucket` param; default follows
        -- the window (24h -> hour, longer -> day). "hour" is NOT a timeline:
        -- it profiles the hour of day (0-23) across the whole window, so the
        -- chart answers "at which time of day do alerts fire".
        hourRows <-
            if bucketKind == "hour"
                then
                    sqlQueryTyped
                        [typedSql|
                SELECT extract(hour from coalesce(a.started_at, a.first_seen_at))::int AS hour_of_day, a.severity, count(*) AS n
                FROM alerts a
                WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                AND coalesce(a.started_at, a.first_seen_at) < ${to}::timestamptz
                    AND (${envFilter} = '' OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${envFilter})
                    AND (cardinality(${severities}::text[]) = 0 OR a.severity = ANY(${severities}))
                GROUP BY 1, 2
                ORDER BY 1 |]
                else pure []
        dayRows <-
            if bucketKind == "day"
                then
                    sqlQueryTyped
                        [typedSql|
                SELECT date_trunc('day', coalesce(a.started_at, a.first_seen_at)) AS bucket, a.severity, count(*) AS n
                FROM alerts a
                WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                AND coalesce(a.started_at, a.first_seen_at) < ${to}::timestamptz
                    AND (${envFilter} = '' OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${envFilter})
                    AND (cardinality(${severities}::text[]) = 0 OR a.severity = ANY(${severities}))
                GROUP BY 1, 2
                ORDER BY 1 |]
                else pure []
        mttrRows <-
            sqlQueryTyped
                [typedSql|
            SELECT a.severity, avg(extract(epoch from (a.resolved_at - a.started_at)))::float8 AS avg_seconds
            FROM alerts a
            WHERE a.resolved_at IS NOT NULL AND a.started_at IS NOT NULL
                AND a.resolved_at >= a.started_at
                AND coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                AND coalesce(a.started_at, a.first_seen_at) < ${to}::timestamptz
                AND (${envFilter} = '' OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${envFilter})
                AND (cardinality(${severities}::text[]) = 0 OR a.severity = ANY(${severities}))
            GROUP BY a.severity
            ORDER BY avg_seconds DESC |]
        -- KPI tile: mean resolution across ALL resolved alerts in the window
        avgRows <-
            sqlQueryTyped
                [typedSql|
            SELECT avg(extract(epoch from (a.resolved_at - a.started_at)))::float8 AS avg_seconds
            FROM alerts a
            WHERE a.resolved_at IS NOT NULL AND a.started_at IS NOT NULL
                AND a.resolved_at >= a.started_at
                AND coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                AND coalesce(a.started_at, a.first_seen_at) < ${to}::timestamptz
                AND (${envFilter} = '' OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${envFilter})
                AND (cardinality(${severities}::text[]) = 0 OR a.severity = ANY(${severities})) |]
        let envs = case selectedEnv of
                -- keep a manually-typed/unknown selection visible in the dropdown
                Just env | env `notElem` knownEnvs -> env : knownEnvs
                _ -> knownEnvs
            knownEnvs = [env | Just env <- envRows]
            -- keep manually-typed/unknown selections visible in the dropdown
            severityOptions = sortOn Reports.severityRank (nub (severityOptionRows <> severities))
            envPanelTitle = if isJust selectedEnv then tr "Alerts by host" else tr "Alerts by environment"
            hourCounts = Map.fromListWith (<>) (mapMaybe (\row -> (,[(get #severity row, get #n row)]) <$> get #hour_of_day row) hourRows)
            hourLabel :: Int -> Text
            hourLabel h = (if h < 10 then "0" else "") <> show h <> ":00"
            dayCounts = Map.fromListWith (<>) (mapMaybe (\row -> (,[(get #severity row, get #n row)]) <$> get #bucket row) dayRows)
            dayBuckets = takeWhile (< to) (iterate (addUTCTime 86400) (truncateBucket 86400 from))
            dayLabel = cs . TimeFormat.formatTime TimeFormat.defaultTimeLocale "%m-%d"
            volumeData =
                if bucketKind == "hour"
                    then [(hourLabel h, Map.findWithDefault [] h hourCounts) | h <- [0 .. 23]]
                    else [(dayLabel bucket, Map.findWithDefault [] bucket dayCounts) | bucket <- dayBuckets]
            hasEmptyBuckets = not (null volumeData) && any (\(_, segments) -> sum (map snd segments) == 0) volumeData
            volumeSeverities = sortOn Reports.severityRank (nub (concatMap (map fst . snd) volumeData))
            volumeSvg = Reports.volumeChartSvg volumeData
            volumeTitle = if bucketKind == "hour" then tr "Alert volume per hour" else tr "Alert volume per day"
            avgResolution = case avgRows of (value : _) -> value; [] -> Nothing
            -- KPI tiles (all derived from the same filtered rows as the charts)
            severityCounts = [(Reports.severityCssClass (get #severity row), get #n row) | row <- severityRows]
            totalAlerts = sum (map snd severityCounts)
            criticalCount = sum [n | (cls, n) <- severityCounts, cls == "chart-sev-critical"]
            highCount = sum [n | (cls, n) <- severityCounts, cls == "chart-sev-high"]
            (topBreakdownLabel, topBreakdownCount, topBreakdownPct) = case breakdownRows of
                (row : _) ->
                    let n = get #n row
                     in (fromMaybe "unknown" (get #label row), n, round (fromIntegral n / fromIntegral (max 1 totalAlerts) * 100))
                [] -> ("—", 0, 0)
            -- Horizontal bar panels: (label, value text, width %, fill class)
            severityBarData =
                sortOn
                    (\(name, n) -> (negate n, Reports.severityRank name))
                    [ (option, Map.findWithDefault 0 option severityCountsByName)
                    | option <- severityOptions
                    ]
            severityCountsByName = Map.fromList [(get #severity row, get #n row) | row <- severityRows]
            severityBars = [(name, tshow n, pctOf (fromIntegral n) (maxOf (map (fromIntegral . snd) severityBarData)), Reports.severityFillClass name) | (name, n) <- severityBarData]
            envBarData = [(fromMaybe "unknown" (get #label row), get #n row) | row <- breakdownRows]
            envBars = [(name, tshow n, pctOf (fromIntegral n) (maxOf (map (fromIntegral . snd) envBarData)), "bar-accent") | (name, n) <- envBarData]
            mttrData = [(get #severity row, fromMaybe 0 (get #avg_seconds row)) | row <- mttrRows]
            mttrBars = [(name, Reports.formatDurationHm secs, pctOf secs (maxOf (map snd mttrData)), Reports.severityFillClass name) | (name, secs) <- mttrData]
            sourceBars =
                [ (name, tshow n, pctOf (fromIntegral n) (maxOf (map (fromIntegral . snd) sourceBarData)), "bar-accent")
                | (name, n) <- sourceBarData
                ]
            sourceBarData = [(get #label row, get #n row) | row <- sourceRows]
        render IndexView{..}
      where
        pctOf :: Double -> Double -> Double
        pctOf value maxValue = value / max 1 maxValue * 100
        maxOf :: [Double] -> Double
        maxOf values = if null values then 1 else maximum values

-- Align a timestamp down to its day boundary so chart slots are positional
-- in time, not in row order.
truncateBucket :: NominalDiffTime -> UTCTime -> UTCTime
truncateBucket step t = t{utctDayTime = fromIntegral (seconds `div` stepSeconds * stepSeconds)}
  where
    seconds = floor (utctDayTime t) :: Integer
    stepSeconds = floor step :: Integer

nonEmptyText :: Text -> Maybe Text
nonEmptyText value = if value == "" then Nothing else Just value
