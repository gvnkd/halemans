module Application.Service.Api.Alerts (
    AlertFilters (..),
    AlertDetail (..),
    AlertPage (..),
    defaultFilters,
    listAlertsPage,
    alertDetail,
) where

import Application.Helper.DashboardConfig (globToLike)
import Application.Service.AlertScope (alertVisibleWith)
import Application.Service.Api.Cursor (Cursor (..), encodeCursor)
import Data.Int (Int64)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, limit, orderByAsc, orderByDesc, query)
import IHP.TypedSql (sqlQueryTyped, typedSql)

-- Filter set for GET /api/v1/alerts (design_docs/milestone_6.md §3): empty
-- text means "no filter"; keyset pagination on (last_seen_at desc, id desc).
-- env/host/service/title values containing * or ? match as shell globs
-- (translated to LIKE), everything else stays exact; a title without
-- wildcards still matches exactly because globToLike escapes LIKE specials.
data AlertFilters = AlertFilters
    { afEnvironment :: !Text
    , afStatus :: !Text
    , afSeverity :: !Text
    , afFingerprint :: !Text
    , afHost :: !Text
    , afService :: !Text
    , afTitle :: !Text
    , afSince :: !UTCTime
    , afUntil :: !UTCTime
    , afCursor :: !(Maybe Cursor)
    , afLimit :: !Int
    }

defaultFilters :: AlertFilters
defaultFilters =
    AlertFilters
        { afEnvironment = ""
        , afStatus = ""
        , afSeverity = ""
        , afFingerprint = ""
        , afHost = ""
        , afService = ""
        , afTitle = ""
        , afSince = UTCTime (fromGregorian 1970 1 1) 0
        , afUntil = UTCTime (fromGregorian 9999 12 31) 0
        , afCursor = Nothing
        , afLimit = 100
        }

-- | One page of matching alerts: the rows, the cursor for the next page
-- (Nothing when the page was not full) and the TOTAL number of alerts
-- matching the filters (ignoring the page window) so clients can render
-- "20 of N".
data AlertPage = AlertPage
    { apAlerts :: [Alert]
    , apNextCursor :: Maybe Text
    , apTotal :: Int64
    }

-- | Fetches limit+1 keys to detect the last page. scopeNames is the SAME
-- per-user host group visibility the web list uses (AlertScope): Nothing =
-- unrestricted, Just names = only zabbix alerts whose host groups intersect
-- the names (plus non-zabbix and internal).
listAlertsPage :: (?modelContext :: ModelContext) => AlertFilters -> Maybe [Text] -> IO AlertPage
listAlertsPage AlertFilters{..} scopeNames = do
    let (cursorSeenAt, cursorId) = case afCursor of
            Just Cursor{..} -> (cursorLastSeenAt, cursorAlertId)
            Nothing -> (UTCTime (fromGregorian 9999 12 31) 0, maxUuid)
        unrestricted = isNothing scopeNames
        scope = fromMaybe [] scopeNames
        envLike = globLike afEnvironment
        hostLike = globLike afHost
        serviceLike = globLike afService
        titleLike = globLike afTitle
    rows <-
        sqlQueryTyped
            [typedSql|
        SELECT a.id, a.last_seen_at
        FROM alerts a
        LEFT JOIN sources sc ON sc.id = a.source_id
        WHERE ('' = ${afEnvironment} OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${afEnvironment} OR ('' <> ${envLike} AND coalesce(nullif(a.facets ->> 'env', ''), a.env) LIKE ${envLike}))
          AND ('' = ${afStatus} OR a.status = ${afStatus})
          AND ('' = ${afSeverity} OR a.severity = ${afSeverity})
          AND ('' = ${afFingerprint} OR a.fingerprint = ${afFingerprint})
          AND ('' = ${afHost} OR coalesce(nullif(a.facets ->> 'host', ''), a.host) = ${afHost} OR ('' <> ${hostLike} AND coalesce(nullif(a.facets ->> 'host', ''), a.host) LIKE ${hostLike}))
          AND ('' = ${afService} OR coalesce(nullif(a.facets ->> 'service', ''), a.service) = ${afService} OR ('' <> ${serviceLike} AND coalesce(nullif(a.facets ->> 'service', ''), a.service) LIKE ${serviceLike}))
          AND ('' = ${afTitle} OR a.title LIKE ${titleLike})
          AND a.last_seen_at >= ${afSince}
          AND a.last_seen_at <= ${afUntil}
          AND (a.last_seen_at, a.id) < (${cursorSeenAt}, ${cursorId})
          AND (${unrestricted} OR a.fingerprint LIKE 'halemans:%' OR sc.type IS DISTINCT FROM 'zabbix' OR a.host_groups ?| ${scope})
        ORDER BY a.last_seen_at DESC, a.id DESC
        LIMIT (${afLimit} + 1)
    |]
    -- The total is a separate COUNT with the SAME filter legs but WITHOUT the
    -- keyset cursor, so every page reports the same "of N" total. Keep the
    -- two predicates in sync when adding filters.
    totalRows <-
        sqlQueryTyped
            [typedSql|
        SELECT COUNT(*)::bigint AS n
        FROM alerts a
        LEFT JOIN sources sc ON sc.id = a.source_id
        WHERE ('' = ${afEnvironment} OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${afEnvironment} OR ('' <> ${envLike} AND coalesce(nullif(a.facets ->> 'env', ''), a.env) LIKE ${envLike}))
          AND ('' = ${afStatus} OR a.status = ${afStatus})
          AND ('' = ${afSeverity} OR a.severity = ${afSeverity})
          AND ('' = ${afFingerprint} OR a.fingerprint = ${afFingerprint})
          AND ('' = ${afHost} OR coalesce(nullif(a.facets ->> 'host', ''), a.host) = ${afHost} OR ('' <> ${hostLike} AND coalesce(nullif(a.facets ->> 'host', ''), a.host) LIKE ${hostLike}))
          AND ('' = ${afService} OR coalesce(nullif(a.facets ->> 'service', ''), a.service) = ${afService} OR ('' <> ${serviceLike} AND coalesce(nullif(a.facets ->> 'service', ''), a.service) LIKE ${serviceLike}))
          AND ('' = ${afTitle} OR a.title LIKE ${titleLike})
          AND a.last_seen_at >= ${afSince}
          AND a.last_seen_at <= ${afUntil}
          AND (${unrestricted} OR a.fingerprint LIKE 'halemans:%' OR sc.type IS DISTINCT FROM 'zabbix' OR a.host_groups ?| ${scope})
    |]
    let page = take afLimit rows
    alerts <- mapM (\row -> fetch (get #id row)) page
    let nextCursor = case reverse page of
            lastRow : _
                | length rows > afLimit ->
                    Just (encodeCursor (Cursor (get #last_seen_at lastRow) (unwrapId (get #id lastRow))))
            _ -> Nothing
        total = case totalRows of
            (rowTotal : _) -> rowTotal
            [] -> 0
    pure AlertPage{apAlerts = alerts, apNextCursor = nextCursor, apTotal = total}

-- | LIKE translation for a filter value: empty for plain values (the exact
-- match leg handles them), the translated glob otherwise.
globLike :: Text -> Text
globLike value
    | Text.any (\c -> c == '*' || c == '?') value = globToLike value
    | otherwise = ""

maxUuid :: UUID
maxUuid = UUID.fromWords maxBound maxBound maxBound maxBound

unwrapId :: Id' table -> PrimaryKey table
unwrapId (Id uuid) = uuid

data AlertDetail = AlertDetail
    { adAlert :: !Alert
    , adEnvironment :: !(Maybe Environment)
    , adHost :: !(Maybe Host)
    , adService :: !(Maybe Service)
    , adGroup :: !(Maybe AlertGroup)
    , adJiraLinks :: ![JiraLink]
    , adCmdb :: !(Maybe CmdbEntry)
    , adAnalysis :: !(Maybe LlmAnalysis)
    , adTimeline :: ![(AlertEvent, Maybe Text)]
    }

-- Alert detail (design §3 D4): refs resolved, latest done LLM analysis, and
-- the alert_events timeline ordered by created_at. No live upstream calls.
-- scopeNames is the per-user host group visibility (AlertScope): an
-- out-of-scope alert is indistinguishable from a missing one (Nothing).
alertDetail :: (?modelContext :: ModelContext) => Id Alert -> Maybe [Text] -> IO (Maybe AlertDetail)
alertDetail alertId scopeNames = do
    alertOrNothing <-
        query @Alert
            |> filterWhere (#id, alertId)
            |> fetchOneOrNothing
    visible <- case alertOrNothing of
        Nothing -> pure False
        Just alert -> case scopeNames of
            Nothing -> pure True
            Just names -> do
                isZabbix <- case alert.sourceId of
                    Nothing -> pure False
                    Just sourceId -> maybe False (\source -> source.type_ == "zabbix") <$> fetchOneOrNothing sourceId
                pure (alertVisibleWith names isZabbix alert)
    case (alertOrNothing, visible) of
        (Just alert, True) -> detailFor alert
        _ -> pure Nothing
  where
    detailFor alert = do
        environment <- mapM fetch alert.environmentId
        host <- mapM fetch alert.hostId
        service <- mapM fetch alert.serviceId
        group <- mapM fetch alert.groupId
        jiraLinks <-
            query @JiraLink
                |> filterWhere (#alertId, alertId)
                |> orderByAsc #createdAt
                |> fetch
        cmdbEntry <- case (alert.hostId, alert.serviceId) of
            (Just hostId, _) ->
                query @CmdbEntry
                    |> filterWhere (#hostId, Just hostId)
                    |> fetchOneOrNothing
            (Nothing, Just serviceId) ->
                query @CmdbEntry
                    |> filterWhere (#serviceId, Just serviceId)
                    |> fetchOneOrNothing
            (Nothing, Nothing) -> pure Nothing
        analysis <-
            query @LlmAnalysis
                |> filterWhere (#alertId, alertId)
                |> filterWhere (#status, "done" :: Text)
                |> orderByDesc #createdAt
                |> limit 1
                |> fetchOneOrNothing
        events <-
            query @AlertEvent
                |> filterWhere (#alertId, alertId)
                |> orderByAsc #createdAt
                |> fetch
        emails <- mapM (\event -> fmap (fmap (get #email)) (mapM fetch event.userId)) events
        pure $
            Just
                AlertDetail
                    { adAlert = alert
                    , adEnvironment = environment
                    , adHost = host
                    , adService = service
                    , adGroup = group
                    , adJiraLinks = jiraLinks
                    , adCmdb = cmdbEntry
                    , adAnalysis = analysis
                    , adTimeline = zip events emails
                    }
