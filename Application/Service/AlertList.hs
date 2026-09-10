module Application.Service.AlertList
( AlertListFilters (..)
, defaultAlertListFilters
, validSortColumns
, listAlerts
, countBySeverity
, effectiveEnvNames
, matchesFilters
, parseAlertFilters
, alertFiltersToValue
, alertFiltersFromValue
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.ModelSupport (Id' (..))
import IHP.Fetch (fetch)
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Generated.Types
import Application.Helper.DashboardConfig (validAlertSortColumns)
import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.!=), (.=))
import Data.Aeson.Types (Parser, parseMaybe)
import Data.UUID (UUID)
import qualified Data.Text as Text

-- Filter/sort state shared by the /alerts HTTP action and the websocket
-- broadcaster (which needs the same predicate to keep live updates
-- consistent with the filtered view).
data AlertListFilters = AlertListFilters
    { alfSeverities :: [Text]
    , alfStatuses :: [Text]
    , alfEnvs :: [Text]
    , alfHost :: Maybe Text
    , alfService :: Maybe Text
    , alfTitle :: Maybe Text
    , alfGroup :: Maybe Text
    , alfSort :: Text
    , alfDir :: Text
    } deriving (Eq, Show)

defaultAlertListFilters :: AlertListFilters
defaultAlertListFilters = AlertListFilters
    { alfSeverities = []
    , alfStatuses = []
    , alfEnvs = []
    , alfHost = Nothing
    , alfService = Nothing
    , alfTitle = Nothing
    , alfGroup = Nothing
    , alfSort = "last_seen_at"
    , alfDir = "desc"
    }

validSortColumns :: [Text]
validSortColumns = validAlertSortColumns

-- Sorting is dynamic, which the query builder cannot express (ORDER BY is
-- not parameterizable), so the id page comes from one typedSql statement
-- with a computed text sort key; rows are then fetched as model records.
-- Severity/status map to rank strings so textual ordering matches the
-- domain ordering. env/host/service filters and sort keys read the EFFECTIVE
-- value: a materialized facet named like the field overrides the raw column.
listAlerts :: (?modelContext :: ModelContext) => AlertListFilters -> Int -> IO [Alert]
listAlerts filters lim = do
    let sevs = filters.alfSeverities
        statuses = filters.alfStatuses
        envs = filters.alfEnvs
        host = fromMaybe "" filters.alfHost
        service = fromMaybe "" filters.alfService
        titlePattern = fromMaybe "" filters.alfTitle
        groupPattern = fromMaybe "" filters.alfGroup
        sort = filters.alfSort
        dir = filters.alfDir
        lim64 = fromIntegral lim :: Int64
    rows :: [Id Alert] <- sqlQueryTyped [typedSql|
        SELECT sorted.id
        FROM (
            SELECT a.id,
                CASE ${sort}
                    WHEN 'status' THEN CASE a.status WHEN 'firing' THEN '0' WHEN 'ack' THEN '1' WHEN 'resolved' THEN '2' ELSE '3' END
                    WHEN 'severity' THEN CASE a.severity WHEN 'critical' THEN '0' WHEN 'high' THEN '1' WHEN 'warning' THEN '2' WHEN 'info' THEN '3' ELSE '4' END
                    WHEN 'title' THEN lower(a.title)
                    WHEN 'env' THEN coalesce(nullif(a.facets ->> 'env', ''), a.env, '')
                    WHEN 'host' THEN coalesce(nullif(a.facets ->> 'host', ''), a.host, '')
                    WHEN 'occurrences' THEN lpad(a.occurrences::text, 12, '0')
                    ELSE to_char(a.last_seen_at AT TIME ZONE 'UTC', 'YYYY-MM-DD HH24:MI:SS.US')
                END AS sort_key,
                a.last_seen_at
            FROM alerts a
            LEFT JOIN alert_groups g ON g.id = a.group_id
            WHERE (CASE WHEN cardinality(${statuses}::text[]) = 0 THEN a.status <> 'closed' ELSE a.status = ANY(${statuses}) END)
              AND (cardinality(${sevs}::text[]) = 0 OR a.severity = ANY(${sevs}))
              AND (cardinality(${envs}::text[]) = 0 OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ANY(${envs}))
              AND ('' = ${host} OR coalesce(nullif(a.facets ->> 'host', ''), a.host) = ${host})
              AND ('' = ${service} OR coalesce(nullif(a.facets ->> 'service', ''), a.service) = ${service})
              AND ('' = ${titlePattern} OR a.title ILIKE '%' || ${titlePattern} || '%')
              AND ('' = ${groupPattern} OR g.group_key ILIKE '%' || ${groupPattern} || '%')
        ) AS sorted
        ORDER BY
            CASE WHEN ${dir} = 'asc' THEN sorted.sort_key END ASC,
            CASE WHEN ${dir} = 'desc' THEN sorted.sort_key END DESC,
            sorted.last_seen_at DESC,
            sorted.id DESC
        LIMIT ${lim64}
    |]
    mapM fetch rows

-- Totals per severity over the currently filtered set (drives the count
-- badges on /alerts).
countBySeverity :: (?modelContext :: ModelContext) => AlertListFilters -> IO [(Text, Int64)]
countBySeverity filters = do
    let sevs = filters.alfSeverities
        statuses = filters.alfStatuses
        envs = filters.alfEnvs
        host = fromMaybe "" filters.alfHost
        service = fromMaybe "" filters.alfService
        titlePattern = fromMaybe "" filters.alfTitle
        groupPattern = fromMaybe "" filters.alfGroup
    rows <- sqlQueryTyped [typedSql|
        SELECT a.severity, COUNT(*) AS n
        FROM alerts a
        LEFT JOIN alert_groups g ON g.id = a.group_id
        WHERE (CASE WHEN cardinality(${statuses}::text[]) = 0 THEN a.status <> 'closed' ELSE a.status = ANY(${statuses}) END)
          AND (cardinality(${sevs}::text[]) = 0 OR a.severity = ANY(${sevs}))
          AND (cardinality(${envs}::text[]) = 0 OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ANY(${envs}))
          AND ('' = ${host} OR coalesce(nullif(a.facets ->> 'host', ''), a.host) = ${host})
          AND ('' = ${service} OR coalesce(nullif(a.facets ->> 'service', ''), a.service) = ${service})
          AND ('' = ${titlePattern} OR a.title ILIKE '%' || ${titlePattern} || '%')
          AND ('' = ${groupPattern} OR g.group_key ILIKE '%' || ${groupPattern} || '%')
        GROUP BY a.severity
    |]
    pure (map (\row -> (get #severity row, get #n row)) rows)

-- | Distinct effective env names among non-closed alerts, for the /alerts
-- env filter dropdown (override-only names never appear in the environments
-- table). The controller merges these with the inventory names.
effectiveEnvNames :: (?modelContext :: ModelContext) => IO [Text]
effectiveEnvNames = do
    rows <- sqlQueryTyped [typedSql|
        SELECT DISTINCT coalesce(nullif(a.facets ->> 'env', ''), a.env) AS name
        FROM alerts a
        WHERE a.status <> 'closed'
          AND coalesce(nullif(a.facets ->> 'env', ''), a.env) IS NOT NULL
        ORDER BY name
    |]
    pure (catMaybes rows)

-- Pure predicate mirror of the list query, used by the websocket
-- broadcaster to decide whether an alert event is visible to a filtered
-- /alerts view. IO only for the group-key pattern (needs the group row).
matchesFilters :: (?modelContext :: ModelContext) => AlertListFilters -> Alert -> IO Bool
matchesFilters filters alert = do
    groupOk <- case filters.alfGroup of
        Nothing -> pure True
        Just pattern -> case alert.groupId of
            Nothing -> pure False
            Just groupId -> do
                group <- fetch groupId
                pure (Text.isInfixOf (Text.toLower pattern) (Text.toLower group.groupKey))
    pure (and
        [ null filters.alfSeverities || alert.severity `elem` filters.alfSeverities
        , statusOk
        , null filters.alfEnvs || maybe False (`elem` filters.alfEnvs) (effectiveFieldText FieldEnv alert)
        , maybe True (\host -> effectiveFieldText FieldHost alert == Just host) filters.alfHost
        , maybe True (\service -> effectiveFieldText FieldService alert == Just service) filters.alfService
        , maybe True (\pattern -> Text.isInfixOf (Text.toLower pattern) (Text.toLower alert.title)) filters.alfTitle
        , groupOk
        ])
    where
        statusOk = case filters.alfStatuses of
            [] -> alert.status /= "closed"
            selected -> alert.status `elem` selected

-- Subscribe-frame payload for the alerts scope: the client forwards the
-- current query string so live updates respect the rendered view.
parseAlertFilters :: Aeson.Value -> Parser AlertListFilters
parseAlertFilters = Aeson.withObject "filters" \o -> do
    severities <- o Aeson..:? "severity" .!= []
    statuses <- o Aeson..:? "status" .!= []
    envs <- o Aeson..:? "env" .!= []
    host <- nonEmptyField o "host"
    service <- nonEmptyField o "service"
    title <- nonEmptyField o "q"
    group <- nonEmptyField o "group"
    pure defaultAlertListFilters
        { alfSeverities = severities
        , alfStatuses = statuses
        , alfEnvs = envs
        , alfHost = host
        , alfService = service
        , alfTitle = title
        , alfGroup = group
        }
    where
        nonEmptyField o key = do
            raw <- o Aeson..:? key .!= ""
            pure (if raw == "" then Nothing else Just raw)

-- Persisted shape for users.settings.filters.alerts: same keys as the query
-- string plus sort/dir, so a bare /alerts visit can be redirected to the
-- stored URL verbatim.
alertFiltersToValue :: AlertListFilters -> Aeson.Value
alertFiltersToValue filters = Aeson.object
    [ "severity" .= filters.alfSeverities
    , "status" .= filters.alfStatuses
    , "env" .= filters.alfEnvs
    , "host" .= filters.alfHost
    , "service" .= filters.alfService
    , "q" .= filters.alfTitle
    , "group" .= filters.alfGroup
    , "sort" .= filters.alfSort
    , "dir" .= filters.alfDir
    ]

alertFiltersFromValue :: Aeson.Value -> Maybe AlertListFilters
alertFiltersFromValue = parseMaybe parser
    where
        parser = Aeson.withObject "alertFilters" \o -> do
            severities <- o Aeson..:? "severity" .!= []
            statuses <- o Aeson..:? "status" .!= []
            envs <- o Aeson..:? "env" .!= []
            host <- nonEmptyField o "host"
            service <- nonEmptyField o "service"
            title <- nonEmptyField o "q"
            group <- nonEmptyField o "group"
            sort :: Text <- o Aeson..:? "sort" .!= "last_seen_at"
            dir :: Text <- o Aeson..:? "dir" .!= "desc"
            pure defaultAlertListFilters
                { alfSeverities = severities
                , alfStatuses = statuses
                , alfEnvs = envs
                , alfHost = host
                , alfService = service
                , alfTitle = title
                , alfGroup = group
                , alfSort = if sort `elem` validSortColumns then sort else "last_seen_at"
                , alfDir = if dir == "asc" then "asc" else "desc"
                }
        nonEmptyField o key = do
            raw <- o Aeson..:? key .!= ""
            pure (if raw == "" then Nothing else Just raw)
