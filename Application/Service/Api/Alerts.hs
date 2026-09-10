module Application.Service.Api.Alerts
    ( AlertFilters (..)
    , AlertDetail (..)
    , defaultFilters
    , listAlertsPage
    , alertDetail
    ) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder (query, filterWhere, orderByAsc, orderByDesc, limit)
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Generated.Types
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Time.Calendar (fromGregorian)
import Application.Service.Api.Cursor (Cursor (..), encodeCursor)

-- Filter set for GET /api/v1/alerts (design_docs/milestone_6.md §3): empty
-- text means "no filter"; keyset pagination on (last_seen_at desc, id desc).
data AlertFilters = AlertFilters
    { afEnvironment :: !Text
    , afStatus :: !Text
    , afSeverity :: !Text
    , afFingerprint :: !Text
    , afHost :: !Text
    , afService :: !Text
    , afSince :: !UTCTime
    , afUntil :: !UTCTime
    , afCursor :: !(Maybe Cursor)
    , afLimit :: !Int
    }

defaultFilters :: AlertFilters
defaultFilters = AlertFilters
    { afEnvironment = ""
    , afStatus = ""
    , afSeverity = ""
    , afFingerprint = ""
    , afHost = ""
    , afService = ""
    , afSince = UTCTime (fromGregorian 1970 1 1) 0
    , afUntil = UTCTime (fromGregorian 9999 12 31) 0
    , afCursor = Nothing
    , afLimit = 100
    }

-- One page of matching alerts plus the cursor for the next page (Nothing
-- when the page was not full). Fetches limit+1 keys to detect the last page.
listAlertsPage :: (?modelContext :: ModelContext) => AlertFilters -> IO ([Alert], Maybe Text)
listAlertsPage AlertFilters { .. } = do
    let (cursorSeenAt, cursorId) = case afCursor of
            Just Cursor { .. } -> (cursorLastSeenAt, cursorAlertId)
            Nothing -> (UTCTime (fromGregorian 9999 12 31) 0, maxUuid)
    rows <- sqlQueryTyped [typedSql|
        SELECT a.id, a.last_seen_at
        FROM alerts a
        WHERE ('' = ${afEnvironment} OR coalesce(nullif(a.facets ->> 'env', ''), a.env) = ${afEnvironment})
          AND ('' = ${afStatus} OR a.status = ${afStatus})
          AND ('' = ${afSeverity} OR a.severity = ${afSeverity})
          AND ('' = ${afFingerprint} OR a.fingerprint = ${afFingerprint})
          AND ('' = ${afHost} OR coalesce(nullif(a.facets ->> 'host', ''), a.host) = ${afHost})
          AND ('' = ${afService} OR coalesce(nullif(a.facets ->> 'service', ''), a.service) = ${afService})
          AND a.last_seen_at >= ${afSince}
          AND a.last_seen_at <= ${afUntil}
          AND (a.last_seen_at, a.id) < (${cursorSeenAt}, ${cursorId})
        ORDER BY a.last_seen_at DESC, a.id DESC
        LIMIT (${afLimit} + 1)
    |]
    let page = take afLimit rows
    alerts <- mapM (\row -> fetch (get #id row)) page
    let nextCursor = case reverse page of
            lastRow : _ | length rows > afLimit ->
                Just (encodeCursor (Cursor (get #last_seen_at lastRow) (unwrapId (get #id lastRow))))
            _ -> Nothing
    pure (alerts, nextCursor)

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
alertDetail :: (?modelContext :: ModelContext) => Id Alert -> IO (Maybe AlertDetail)
alertDetail alertId = do
    alertOrNothing <- query @Alert
        |> filterWhere (#id, alertId)
        |> fetchOneOrNothing
    case alertOrNothing of
        Nothing -> pure Nothing
        Just alert -> do
            environment <- mapM fetch alert.environmentId
            host <- mapM fetch alert.hostId
            service <- mapM fetch alert.serviceId
            group <- mapM fetch alert.groupId
            jiraLinks <- query @JiraLink
                |> filterWhere (#alertId, alertId)
                |> orderByAsc #createdAt
                |> fetch
            cmdbEntry <- case (alert.hostId, alert.serviceId) of
                (Just hostId, _) -> query @CmdbEntry
                    |> filterWhere (#hostId, Just hostId)
                    |> fetchOneOrNothing
                (Nothing, Just serviceId) -> query @CmdbEntry
                    |> filterWhere (#serviceId, Just serviceId)
                    |> fetchOneOrNothing
                (Nothing, Nothing) -> pure Nothing
            analysis <- query @LlmAnalysis
                |> filterWhere (#alertId, alertId)
                |> filterWhere (#status, "done" :: Text)
                |> orderByDesc #createdAt
                |> limit 1
                |> fetchOneOrNothing
            events <- query @AlertEvent
                |> filterWhere (#alertId, alertId)
                |> orderByAsc #createdAt
                |> fetch
            emails <- mapM (\event -> fmap (fmap (get #email)) (mapM fetch event.userId)) events
            pure $ Just AlertDetail
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
