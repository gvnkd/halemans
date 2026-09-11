module Web.Controller.Flapping where

import Web.Controller.Prelude
import Web.View.Flapping.Index
import Application.Service.Flapping
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Text.Read (readMaybe)
import Data.List (groupBy, maximumBy)
import Data.Ord (comparing)

-- Flapping alerts report (design_docs/milestone_11.md §4): on-demand admin
-- analysis of alert_events + alert-row succession per fingerprint.
instance Controller FlappingController where
    beforeAction = ensureIsUser

    action FlappingAction = do
        requirePrivilege "admin"
        now <- getCurrentTime
        let windowHours = clampWindow (intParam 24 "windowHours")
            minFlaps = max 1 (intParam defaultFlapParams.minFlaps "minFlaps")
            maxGapSeconds = max 60 (intParam defaultFlapParams.maxGapSeconds "maxGapSeconds")
            -- Look back one max-gap past the window so a loud edge just
            -- before the window still pairs with an in-window resolve.
            from = addUTCTime (negate (fromIntegral (windowHours * 3600 + maxGapSeconds))) now
        rows <- sqlQueryTyped [typedSql|
            WITH window_events AS (
                SELECT e.alert_id, e.kind, e.payload, e.created_at
                FROM alert_events e
                WHERE e.created_at >= ${from}::timestamptz AND e.created_at < ${now}::timestamptz
                    AND (e.kind IN ('resolved', 'stalled')
                        OR (e.kind = 'repeated' AND e.payload ->> 'from' IN ('resolved', 'stalled')))
            ),
            candidates AS (
                SELECT DISTINCT a.fingerprint
                FROM alerts a
                WHERE a.id IN (SELECT alert_id FROM window_events)
                    OR (coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                        AND coalesce(a.started_at, a.first_seen_at) < ${now}::timestamptz)
            ),
            edges AS (
                SELECT a.fingerprint, 'loud'::text AS edge_kind,
                    coalesce(a.started_at, a.first_seen_at) AS edge_at, a.id AS alert_id
                FROM alerts a JOIN candidates c ON c.fingerprint = a.fingerprint
                WHERE coalesce(a.started_at, a.first_seen_at) >= ${from}::timestamptz
                    AND coalesce(a.started_at, a.first_seen_at) < ${now}::timestamptz
                UNION ALL
                SELECT a.fingerprint,
                    CASE WHEN w.kind = 'repeated' THEN 'loud'::text ELSE 'quiet'::text END AS edge_kind,
                    w.created_at AS edge_at, a.id AS alert_id
                FROM window_events w JOIN alerts a ON a.id = w.alert_id
            )
            SELECT e.fingerprint, e.edge_kind, e.edge_at, a.id, a.title, a.severity,
                coalesce(nullif(a.facets ->> 'env', ''), a.env) AS env, a.host, s.name AS source_name
            FROM edges e JOIN alerts a ON a.id = e.alert_id
            LEFT JOIN sources s ON s.id = a.source_id
            ORDER BY e.fingerprint, e.edge_at |]
        let grouped = groupBy (\a b -> get #fingerprint a == get #fingerprint b) rows
            subjects = map (\group -> (subjectOf group, mapMaybe edgeOf group)) grouped
            params = FlapParams { minFlaps, maxGapSeconds, windowSeconds = windowHours * 3600 }
            reports = detectFlapping params subjects
        render IndexView { .. }
      where
        subjectOf group =
            let latest = maximumBy (comparing (get #edge_at)) group
            in FlapSubject
                { fingerprint = fromMaybe "" (get #fingerprint latest)
                , latestAlertId = get #id latest
                , title = get #title latest
                , severity = get #severity latest
                , effectiveEnv = get #env latest
                , host = get #host latest
                , sourceName = get #source_name latest
                }
        -- UNION ALL subquery columns decode as Maybe (typedSql inference).
        edgeOf row = do
            at <- get #edge_at row
            kind <- get #edge_kind row
            pure (FlapEdge at (if kind == "loud" then Loud else Quiet))

clampWindow :: Int -> Int
clampWindow hours
    | hours `elem` [6, 24, 168, 720] = hours
    | otherwise = 24

intParam :: (?request :: Request) => Int -> ByteString -> Int
intParam fallback name = fromMaybe fallback (paramOrNothing @Text name >>= readMaybe . cs)
