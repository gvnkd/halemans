module Application.Service.AlertList (
    AlertListFilters (..),
    defaultAlertListFilters,
    validSortColumns,
    listAlerts,
    countAlerts,
    countBySeverity,
    effectiveEnvNames,
    matchesFilters,
    parseAlertFilters,
    alertFiltersToValue,
    alertFiltersFromValue,
    parseRelativeWindow,
    seenCutoff,
    validColumns,
) where

import Application.Helper.DashboardConfig (alertListColumnKeys, alertListPageSizes, defaultAlertListColumns, defaultAlertListPageSize, validAlertSortColumns)
import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)
import Control.Monad (guard)
import Data.Aeson ((.!=), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Char (isDigit)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, fromGregorian)
import Data.UUID (UUID)
import Generated.Types
import IHP.Fetch (fetch)
import IHP.ModelSupport
import IHP.ModelSupport (Id' (..))
import IHP.Prelude
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Text.Read (readMaybe)

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
    , alfColumns :: [Text]
    -- ^ Visible column keys (subset of alertListColumnKeys).
    , alfPage :: Int
    -- ^ 1-based page; page itself is never persisted to prefs.
    , alfPageSize :: Int
    , alfMinOccurrences :: Maybe Int
    -- ^ occurrences >= N filter (Nothing = off).
    , alfSeenWithin :: Maybe Text
    -- ^ "last seen within" relative window ("15m"/"1h"/"24h"/"7d").
    , alfIncludeClosed :: Bool
    -- ^ /alerts hides closed alerts by default; the env page shows them.
    }
    deriving (Eq, Show)

defaultAlertListFilters :: AlertListFilters
defaultAlertListFilters =
    AlertListFilters
        { alfSeverities = []
        , alfStatuses = []
        , alfEnvs = []
        , alfHost = Nothing
        , alfService = Nothing
        , alfTitle = Nothing
        , alfGroup = Nothing
        , alfSort = "last_seen_at"
        , alfDir = "desc"
        , alfColumns = defaultAlertListColumns
        , alfPage = 1
        , alfPageSize = defaultAlertListPageSize
        , alfMinOccurrences = Nothing
        , alfSeenWithin = Nothing
        , alfIncludeClosed = False
        }

-- | Parses the "seen within" filter value: a positive count with an
-- m/h/d/w suffix. Anything else is treated as no filter.
parseRelativeWindow :: Text -> Maybe NominalDiffTime
parseRelativeWindow raw = do
    let (digits, suffix) = Text.span isDigit raw
    count <- readMaybe (cs digits)
    guard (count > (0 :: Integer))
    factor <- lookup suffix [("m", 60), ("h", 3600), ("d", 86400), ("w", 604800)]
    pure (fromIntegral (count * factor))

-- | Cutoff instant for the "seen within" filter relative to `now`;
-- the epoch when the filter is off/invalid, which makes the SQL predicate
-- (`last_seen_at > ${cutoff}`) trivially true.
seenCutoff :: UTCTime -> Maybe Text -> UTCTime
seenCutoff now window = maybe (UTCTime (fromGregorian 1970 1 1) 0) (\d -> addUTCTime (negate d) now) (window >>= parseRelativeWindow)

validSortColumns :: [Text]
validSortColumns = validAlertSortColumns

-- Sorting is dynamic, which the query builder cannot express (ORDER BY is
-- not parameterizable), so the id page comes from one typedSql statement
-- with a computed text sort key; rows are then fetched as model records.
-- Severity/status map to rank strings so textual ordering matches the
-- domain ordering. env/host/service filters and sort keys read the EFFECTIVE
-- value: a materialized facet named like the field overrides the raw column.
-- Pagination is LIMIT/OFFSET over the same ordering (offset paging keeps
-- arbitrary sort columns and direct page jumps possible).
listAlerts :: (?modelContext :: ModelContext) => AlertListFilters -> IO [Alert]
listAlerts filters = do
    now <- getCurrentTime
    let sevs = filters.alfSeverities
        statuses = filters.alfStatuses
        envs = filters.alfEnvs
        host = fromMaybe "" filters.alfHost
        service = fromMaybe "" filters.alfService
        titlePattern = fromMaybe "" filters.alfTitle
        groupPattern = fromMaybe "" filters.alfGroup
        sort = filters.alfSort
        dir = filters.alfDir
        lim64 = fromIntegral filters.alfPageSize :: Int64
        off64 = fromIntegral ((filters.alfPage - 1) * filters.alfPageSize) :: Int64
        inclClosed = filters.alfIncludeClosed
        occMin = fromMaybe 0 filters.alfMinOccurrences
        cutoff = seenCutoff now filters.alfSeenWithin
    rows :: [Id Alert] <-
        sqlQueryTyped
            [typedSql|
        SELECT sorted.id
        FROM (
            SELECT a.id,
                CASE ${sort}
                    WHEN 'status' THEN CASE a.status WHEN 'firing' THEN '0' WHEN 'ack' THEN '1' WHEN 'resolved' THEN '2' WHEN 'stalled' THEN '3' ELSE '4' END
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
            WHERE (CASE WHEN cardinality(${statuses}::text[]) = 0 THEN (${inclClosed} OR a.status <> 'closed') ELSE a.status = ANY(${statuses}) END)
              AND (cardinality(${sevs}::text[]) = 0 OR a.severity = ANY(${sevs}))
              AND (cardinality(${envs}::text[]) = 0 OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ANY(${envs}))
              AND ('' = ${host} OR coalesce(nullif(a.facets ->> 'host', ''), a.host) = ${host})
              AND ('' = ${service} OR coalesce(nullif(a.facets ->> 'service', ''), a.service) = ${service})
              AND ('' = ${titlePattern} OR a.title ILIKE '%' || ${titlePattern} || '%')
              AND ('' = ${groupPattern} OR g.group_key ILIKE '%' || ${groupPattern} || '%')
              AND (${occMin} = 0 OR a.occurrences >= ${occMin})
              AND (a.last_seen_at > ${cutoff})
        ) AS sorted
        ORDER BY
            CASE WHEN ${dir} = 'asc' THEN sorted.sort_key END ASC,
            CASE WHEN ${dir} = 'desc' THEN sorted.sort_key END DESC,
            sorted.last_seen_at DESC,
            sorted.id DESC
        LIMIT ${lim64} OFFSET ${off64}
    |]
    mapM fetch rows

-- Total rows matching the filters (drives the /alerts pager). Same WHERE as
-- listAlerts/countBySeverity — keep the three in sync.
countAlerts :: (?modelContext :: ModelContext) => AlertListFilters -> IO Int64
countAlerts filters = do
    now <- getCurrentTime
    let sevs = filters.alfSeverities
        statuses = filters.alfStatuses
        envs = filters.alfEnvs
        host = fromMaybe "" filters.alfHost
        service = fromMaybe "" filters.alfService
        titlePattern = fromMaybe "" filters.alfTitle
        groupPattern = fromMaybe "" filters.alfGroup
        inclClosed = filters.alfIncludeClosed
        occMin = fromMaybe 0 filters.alfMinOccurrences
        cutoff = seenCutoff now filters.alfSeenWithin
    rows <-
        sqlQueryTyped
            [typedSql|
        SELECT COUNT(*) AS n
        FROM alerts a
        LEFT JOIN alert_groups g ON g.id = a.group_id
        WHERE (CASE WHEN cardinality(${statuses}::text[]) = 0 THEN (${inclClosed} OR a.status <> 'closed') ELSE a.status = ANY(${statuses}) END)
          AND (cardinality(${sevs}::text[]) = 0 OR a.severity = ANY(${sevs}))
          AND (cardinality(${envs}::text[]) = 0 OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ANY(${envs}))
          AND ('' = ${host} OR coalesce(nullif(a.facets ->> 'host', ''), a.host) = ${host})
          AND ('' = ${service} OR coalesce(nullif(a.facets ->> 'service', ''), a.service) = ${service})
          AND ('' = ${titlePattern} OR a.title ILIKE '%' || ${titlePattern} || '%')
          AND ('' = ${groupPattern} OR g.group_key ILIKE '%' || ${groupPattern} || '%')
          AND (${occMin} = 0 OR a.occurrences >= ${occMin})
          AND (a.last_seen_at > ${cutoff})
    |]
    -- Single-column typedSql results decode as bare values (no get #n).
    pure (fromMaybe 0 (listToMaybe rows))

-- Totals per severity over the currently filtered set (drives the count
-- badges on /alerts).
countBySeverity :: (?modelContext :: ModelContext) => AlertListFilters -> IO [(Text, Int64)]
countBySeverity filters = do
    now <- getCurrentTime
    let sevs = filters.alfSeverities
        statuses = filters.alfStatuses
        envs = filters.alfEnvs
        host = fromMaybe "" filters.alfHost
        service = fromMaybe "" filters.alfService
        titlePattern = fromMaybe "" filters.alfTitle
        groupPattern = fromMaybe "" filters.alfGroup
        inclClosed = filters.alfIncludeClosed
        occMin = fromMaybe 0 filters.alfMinOccurrences
        cutoff = seenCutoff now filters.alfSeenWithin
    rows <-
        sqlQueryTyped
            [typedSql|
        SELECT a.severity, COUNT(*) AS n
        FROM alerts a
        LEFT JOIN alert_groups g ON g.id = a.group_id
        WHERE (CASE WHEN cardinality(${statuses}::text[]) = 0 THEN (${inclClosed} OR a.status <> 'closed') ELSE a.status = ANY(${statuses}) END)
          AND (cardinality(${sevs}::text[]) = 0 OR a.severity = ANY(${sevs}))
          AND (cardinality(${envs}::text[]) = 0 OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ANY(${envs}))
          AND ('' = ${host} OR coalesce(nullif(a.facets ->> 'host', ''), a.host) = ${host})
          AND ('' = ${service} OR coalesce(nullif(a.facets ->> 'service', ''), a.service) = ${service})
          AND ('' = ${titlePattern} OR a.title ILIKE '%' || ${titlePattern} || '%')
          AND ('' = ${groupPattern} OR g.group_key ILIKE '%' || ${groupPattern} || '%')
          AND (${occMin} = 0 OR a.occurrences >= ${occMin})
          AND (a.last_seen_at > ${cutoff})
        GROUP BY a.severity
    |]
    pure (map (\row -> (get #severity row, get #n row)) rows)

-- | Distinct effective env names among non-closed alerts, for the /alerts
-- env filter dropdown (override-only names never appear in the environments
-- table). The controller merges these with the inventory names.
effectiveEnvNames :: (?modelContext :: ModelContext) => IO [Text]
effectiveEnvNames = do
    rows <-
        sqlQueryTyped
            [typedSql|
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
        Just pat -> case alert.groupId of
            Nothing -> pure False
            Just groupId -> do
                group <- fetch groupId
                pure (Text.isInfixOf (Text.toLower pat) (Text.toLower group.groupKey))
    now <- getCurrentTime
    pure
        ( and
            [ null filters.alfSeverities || alert.severity `elem` filters.alfSeverities
            , statusOk
            , null filters.alfEnvs || maybe False (`elem` filters.alfEnvs) (effectiveFieldText FieldEnv alert)
            , maybe True (\host -> effectiveFieldText FieldHost alert == Just host) filters.alfHost
            , maybe True (\service -> effectiveFieldText FieldService alert == Just service) filters.alfService
            , maybe True (\pat -> Text.isInfixOf (Text.toLower pat) (Text.toLower alert.title)) filters.alfTitle
            , groupOk
            , maybe True (\n -> alert.occurrences >= n) filters.alfMinOccurrences
            , alert.lastSeenAt > seenCutoff now filters.alfSeenWithin
            ]
        )
  where
    statusOk = case filters.alfStatuses of
        [] -> filters.alfIncludeClosed || alert.status /= "closed"
        selected -> alert.status `elem` selected

-- Subscribe-frame payload for the alerts scope: the client forwards the
-- current query string so live updates respect the rendered view. cols are
-- parsed too: the broadcaster re-renders rows with the visible columns.
parseAlertFilters :: Aeson.Value -> Parser AlertListFilters
parseAlertFilters = Aeson.withObject "filters" \o -> do
    severities <- o Aeson..:? "severity" .!= []
    statuses <- o Aeson..:? "status" .!= []
    envs <- o Aeson..:? "env" .!= []
    host <- nonEmptyField o "host"
    service <- nonEmptyField o "service"
    title <- nonEmptyField o "q"
    group <- nonEmptyField o "group"
    colsRaw <- o Aeson..:? "cols" .!= []
    occMin <- o Aeson..:? "occ_min"
    seen <- o Aeson..:? "seen"
    pure
        defaultAlertListFilters
            { alfSeverities = severities
            , alfStatuses = statuses
            , alfEnvs = envs
            , alfHost = host
            , alfService = service
            , alfTitle = title
            , alfGroup = group
            , alfColumns = validColumns colsRaw
            , alfMinOccurrences = occMin
            , alfSeenWithin = seen
            }
  where
    nonEmptyField o key = do
        raw <- o Aeson..:? key .!= ""
        pure (if raw == "" then Nothing else Just raw)

-- Keep known column keys in canonical order; an all-unknown/empty selection
-- falls back to the default visible set.
validColumns :: [Text] -> [Text]
validColumns requested = case [c | c <- alertListColumnKeys, c `elem` requested] of
    [] -> defaultAlertListColumns
    valid -> valid

-- Persisted shape for users.settings.filters.alerts: same keys as the query
-- string plus sort/dir/cols/pageSize, so a bare /alerts visit can be
-- redirected to the stored URL verbatim. Page is deliberately excluded —
-- returning to /alerts always lands on page 1 of the stored view.
alertFiltersToValue :: AlertListFilters -> Aeson.Value
alertFiltersToValue filters =
    Aeson.object
        [ "severity" .= filters.alfSeverities
        , "status" .= filters.alfStatuses
        , "env" .= filters.alfEnvs
        , "host" .= filters.alfHost
        , "service" .= filters.alfService
        , "q" .= filters.alfTitle
        , "group" .= filters.alfGroup
        , "sort" .= filters.alfSort
        , "dir" .= filters.alfDir
        , "cols" .= filters.alfColumns
        , "pageSize" .= filters.alfPageSize
        , "occ_min" .= filters.alfMinOccurrences
        , "seen" .= filters.alfSeenWithin
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
        colsRaw <- o Aeson..:? "cols" .!= []
        pageSize <- o Aeson..:? "pageSize" .!= defaultAlertListPageSize
        occMin <- o Aeson..:? "occ_min"
        seenRaw <- o Aeson..:? "seen"
        let seen = seenRaw >>= \w -> if isJust (parseRelativeWindow w) then Just w else Nothing
        pure
            defaultAlertListFilters
                { alfSeverities = severities
                , alfStatuses = statuses
                , alfEnvs = envs
                , alfHost = host
                , alfService = service
                , alfTitle = title
                , alfGroup = group
                , alfSort = if sort `elem` validSortColumns then sort else "last_seen_at"
                , alfDir = if dir == "asc" then "asc" else "desc"
                , alfColumns = validColumns colsRaw
                , alfPageSize = if pageSize `elem` alertListPageSizes then pageSize else defaultAlertListPageSize
                , alfMinOccurrences = occMin
                , alfSeenWithin = seen
                }
    nonEmptyField o key = do
        raw <- o Aeson..:? key .!= ""
        pure (if raw == "" then Nothing else Just raw)
