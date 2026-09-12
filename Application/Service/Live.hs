module Application.Service.Live (
    Scope (..),
    liveBroadcastLoop,
    ensureBroadcaster,
    liveConnectionCount,
) where

import Application.Helper.DashboardConfig (DashboardCard (..), clauseValue, decodeDashboardConfig, matchCardAlert)
import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)
import Application.Service.AlertList (AlertListFilters (..), defaultAlertListFilters, matchesFilters, parseAlertFilters)
import qualified Application.Service.Assets.Cache as AssetsCache
import Application.Service.DashboardCards (ExpandedCard (..), expandDashboardCards, expandedDomId)
import Application.Service.Llm.Queue (latestJobErrors)
import Application.Service.Timeline (headTimelineGroup, timelineHiddenKind)
import qualified Control.Exception.Safe as Exception
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.FrameworkConfig (FrameworkConfig (..))
import IHP.HSX.Markup (Markup, renderMarkupText)
import IHP.ModelSupport
import qualified IHP.PGListener as PGListener
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, limit, orderByAsc, orderByDesc, query)
import IHP.RequestVault ()
import IHP.WebSocket
import Network.Wai (Request)
import qualified Network.WebSockets as WS
import System.IO.Unsafe (unsafePerformIO)
import Web.View.Dashboard.Index (EnvCard (..), cardDomId, computeEnvCards, renderCard)
import Web.View.Dashboards.Show (fetchCardData, renderCardSection)
import Web.View.Fragments

-- Websocket fan-out (milestone_1.md §7): one PG LISTEN subscription per
-- process; each browser connection registers its scope in the registry and
-- receives pre-rendered HSX fragments.

data Scope
    = ScopeDashboard
    | ScopeAlerts AlertListFilters
    | ScopeEnv Text AlertListFilters
    | ScopeAlert UUID
    | ScopeGroup UUID
    | ScopeUserDashboard UUID
    | ScopeNone
    deriving (Eq, Show)

type ConnectionEntry = (UUID, IORef [Scope], Text -> IO ())

registry :: IORef [ConnectionEntry]
registry = unsafePerformIO (newIORef [])
{-# NOINLINE registry #-}

broadcasterStarted :: IORef Bool
broadcasterStarted = unsafePerformIO (newIORef False)
{-# NOINLINE broadcasterStarted #-}

-- Live connection gauge for the /metrics exporter (milestone_6.md §5).
liveConnectionCount :: IO Int
liveConnectionCount = length <$> readIORef registry

-- | Connection loop of the /ws WSApp (see Web.Controller.Live).
liveBroadcastLoop ::
    (?request :: Request, ?modelContext :: ModelContext, ?connection :: WS.Connection) =>
    IO ()
liveBroadcastLoop = do
    connectionId <- UUIDV4.nextRandom
    scopeRef <- newIORef []
    let send = WS.sendTextData ?connection
    modifyIORef' registry ((connectionId, scopeRef, send) :)
    flip Exception.finally (modifyIORef' registry (filter (\(id, _, _) -> id /= connectionId))) do
        forever do
            message <- receiveData @LByteString
            if isResetFrame message
                -- The client navigated (turbolinks swaps pages without
                -- reopening the socket): drop the previous page's scopes
                -- before it sends the new ones.
                then writeIORef scopeRef []
                else case parseScope message of
                    -- A page may subscribe to several scopes (dashboard pages
                    -- cover one env:<name> scope per included card, §7).
                    Just scope -> modifyIORef' scopeRef (scope :)
                    Nothing -> pure ()

isResetFrame :: LByteString -> Bool
isResetFrame message = fromMaybe False do
    value <- Aeson.decode message
    parseMaybe
        ( Aeson.withObject
            "frame"
            ( \o -> do
                scopeType <- o Aeson..: "type" :: Parser Text
                pure (scopeType == "reset")
            )
        )
        value

parseScope :: LByteString -> Maybe Scope
parseScope message = do
    value <- Aeson.decode message
    flip parseMaybe value $ Aeson.withObject "subscribe" \o -> do
        scopeType <- o Aeson..: "type" :: Parser Text
        case scopeType of
            "dashboard" -> pure ScopeDashboard
            "alerts" -> do
                filterValue <- o Aeson..:? "filters"
                filters <- maybe (pure defaultAlertListFilters) parseAlertFilters filterValue
                pure (ScopeAlerts filters)
            "env" -> do
                name <- o Aeson..: "name"
                filterValue <- o Aeson..:? "filters"
                filters <- maybe (pure defaultAlertListFilters) parseAlertFilters filterValue
                pure (ScopeEnv name filters)
            "alert" -> ScopeAlert <$> o Aeson..: "id"
            "group" -> ScopeGroup <$> o Aeson..: "id"
            "dash" -> ScopeUserDashboard <$> o Aeson..: "id"
            _ -> fail "unknown scope"

-- | Idempotent process-wide LISTEN subscription. Called from the first
-- websocket connection (see ensureBroadcaster usage in Web.Controller.Live).
ensureBroadcaster :: (?request :: Request, ?modelContext :: ModelContext) => IO ()
ensureBroadcaster = do
    alreadyStarted <- readIORef broadcasterStarted
    unless alreadyStarted do
        writeIORef broadcasterStarted True
        let frameworkConfig = ?request.frameworkConfig
        listener <- PGListener.init (cs frameworkConfig.databaseUrl) frameworkConfig.logger
        _ <- PGListener.subscribe "halemans_events" (\notification -> let ?request = ?request in broadcast ?modelContext notification) listener
        pure ()

data LiveEvent = LiveEvent
    { leAlertId :: Maybe UUID
    , leGroupId :: Maybe UUID
    , leEnv :: Maybe Text
    , leKind :: Text
    , leTitle :: Maybe Text
    , leSeverity :: Maybe Text
    }

parseLiveEvent :: ByteString -> Maybe LiveEvent
parseLiveEvent bytes = do
    value <- Aeson.decode (BL.fromStrict bytes)
    flip parseMaybe value $ Aeson.withObject "event" \o -> do
        alertId <- o Aeson..:? "alertId" >>= maybe (pure Nothing) (fmap Just . parseUuid)
        groupId <- o Aeson..:? "groupId" >>= maybe (pure Nothing) (fmap Just . parseUuid)
        env <- o Aeson..:? "env"
        kind <- o Aeson..: "kind"
        title <- o Aeson..:? "title"
        severity <- o Aeson..:? "severity"
        pure LiveEvent{leAlertId = alertId, leGroupId = groupId, leEnv = env, leKind = kind, leTitle = title, leSeverity = severity}
  where
    parseUuid raw = maybe (fail "bad uuid") pure (UUID.fromText raw)

broadcast :: (?request :: Request) => ModelContext -> PGListener.Notification -> IO ()
broadcast modelContext notification = do
    let ?modelContext = modelContext
    let ?context = ?request
    case parseLiveEvent notification.notificationData of
        Nothing -> pure ()
        Just event -> do
            connections <- readIORef registry
            forM_ connections \(_, scopeRef, send) -> do
                scopes <- readIORef scopeRef
                updates <- concat <$> forM scopes \scope -> updatesFor scope event
                unless (null updates && not (isBanner event)) do
                    let banner = case (isBanner event, event.leAlertId) of
                            (True, Just alertId) -> Just (object ["title" .= event.leTitle, "severity" .= event.leSeverity, "alertId" .= alertId])
                            _ -> Nothing
                    send (cs (Aeson.encode (object ["updates" .= updates, "banner" .= banner])))

isBanner :: LiveEvent -> Bool
isBanner event = event.leKind == "created" && event.leSeverity `elem` [Just "critical", Just "high"] && isJust event.leAlertId

-- | Compute the fragment updates relevant for a connection scope.
updatesFor :: (?modelContext :: ModelContext, ?request :: Request, ?context :: Request) => Scope -> LiveEvent -> IO [Aeson.Value]
updatesFor scope event = case (scope, event.leAlertId, event.leGroupId) of
    (ScopeDashboard, Just alertId, _) -> do
        (cards, unassigned) <- computeEnvCards
        let allCards = cards ++ maybeToList unassigned
        relevant <- pure $ filter (cardMatches event) allCards
        pure [fragment (cardDomId card) (renderCard card) "replace" "" | card <- relevant]
    -- User dashboard pages (milestone_9.md §6): server-side card evaluation;
    -- any alert event (incl. "enriched", so facet changes re-evaluate) whose
    -- alert matches a card re-renders that card's whole section.
    (ScopeUserDashboard dashUuid, Just alertId, _) -> do
        dashboard <- fetchOneOrNothing (Id dashUuid :: Id Dashboard)
        case dashboard of
            Nothing -> pure []
            Just dash -> case decodeDashboardConfig dash.config of
                Left _ -> pure []
                Right cards -> do
                    alert <- fetch (Id alertId)
                    -- Expand the FULL config first: filtering templates before
                    -- expansion would shift ecIndex and re-render the wrong
                    -- sections. No status pre-filter on the alert: a closed
                    -- alert still matches its template's match clauses, and
                    -- counts/summaries/hideWhen re-evaluate against the
                    -- non-closed base query, so resolve/close updates cards.
                    expanded <- expandDashboardCards cards
                    let matching = [ec | ec <- expanded, matchCardAlert ec.ecCard alert]
                    updates <- forM matching \expandedCard -> do
                        result <- fetchCardData expandedCard
                        pure (fragment expandedCard.ecDomId (renderCardSection (Id dashUuid) (expandedCard, result)) "replaceOrPrepend" "dashboard-cards")
                    -- A forEach value vanishes when its last non-closed alert
                    -- leaves: no expanded card matches the alert anymore, so
                    -- remove the now-stale section explicitly.
                    let removals =
                            [ object ["id" .= expandedDomId index card value, "mode" .= ("remove" :: Text)]
                            | (index, card) <- zip [0 ..] cards
                            , matchCardAlert card alert
                            , Just facetRef <- [card.cardForEach]
                            , value <- [clauseValue facetRef alert]
                            , not (any (\ec -> ec.ecIndex == index && ec.ecValue == value) expanded)
                            ]
                    pure (updates ++ removals)
    (ScopeAlerts scopeFilters, Just alertId, _) -> do
        alert <- fetch (Id alertId)
        matches <- matchesFilters scopeFilters alert
        pure
            [ if matches
                then fragment (alertRowDomId alert) (alertRowHtml alert) "replaceOrPrepend" "alerts-tbody"
                else object ["id" .= alertRowDomId alert, "mode" .= ("remove" :: Text)]
            ]
    (ScopeEnv name scopeFilters, Just alertId, _)
        | event.leEnv == Just name -> do
            alert <- fetch (Id alertId)
            matches <- matchesEnvFilters scopeFilters alert
            pure
                [ if matches
                    then fragment (alertRowDomId alert) (alertRowHtml alert) "replaceOrPrepend" "env-alerts-tbody"
                    else object ["id" .= alertRowDomId alert, "mode" .= ("remove" :: Text)]
                ]
        | otherwise -> pure []
    (ScopeAlert alertUuid, Just alertId, _)
        | alertId == alertUuid -> do
            alert <- fetch (Id alertId)
            latestEvents <-
                query @AlertEvent
                    |> filterWhere (#alertId, Id alertId)
                    |> orderByDesc #createdAt
                    |> limit 50
                    |> fetch
            -- Panel-only kinds (milestone_8.md §4: "assets" refreshes the
            -- context panels without touching status badge or timeline).
            if event.leKind == "assets"
                then contextPanelUpdates alert
                else do
                    panelUpdates <-
                        if event.leKind `elem` ["enriched", "writeback", "writeback_failed"]
                            then contextPanelUpdates alert
                            else pure []
                    -- Internal-error events (enrichment_failed etc.) never
                    -- reach the timeline; visible kinds update the leading
                    -- aggregated group in place via its stable dom id.
                    let timelineUpdates = case latestEvents of
                            (latest : _) | not (timelineHiddenKind (get #kind latest)) ->
                                case headTimelineGroup latestEvents of
                                    Just group -> [fragment (timelineGroupDomId group) (timelineGroupHtml group) "replaceOrPrepend" timelineDomId]
                                    Nothing -> []
                            _ -> []
                    pure
                        ( [ fragment (alertStatusDomId alert) (alertStatusBadgeHtml alert) "replace" ""
                          , fragment alertDetailsDomId (alertDetailsCardHtml alert) "replace" ""
                          ]
                            ++ timelineUpdates
                            ++ panelUpdates
                        )
        | otherwise -> pure []
    -- Group events (kind "group", milestone_2.md §9): the group card header
    -- for group-scoped connections, the env page group row for env scopes
    -- (replace-only: a no-op while the page is in flat view).
    (ScopeGroup groupUuid, _, Just groupId)
        | groupId == groupUuid -> do
            group <- fetch (Id groupId)
            pure [fragment (groupHeaderDomId group) (groupHeaderHtml group) "replace" ""]
        | otherwise -> pure []
    (ScopeEnv name _, _, Just groupId)
        | event.leEnv == Just name -> do
            group <- fetch (Id groupId)
            members <-
                query @Alert
                    |> filterWhere (#groupId, Just (Id groupId))
                    |> orderByDesc #lastSeenAt
                    |> fetch
            pure [fragment (groupRowDomId group) (groupRowHtml (group, members)) "replace" ""]
        | otherwise -> pure []
    -- Alert events also refresh the member row on an open group card.
    (ScopeGroup groupUuid, Just alertId, Nothing) -> do
        alert <- fetch (Id alertId)
        if alert.groupId == Just (Id groupUuid)
            then pure [fragment (alertRowDomId alert) (alertRowHtml alert) "replaceOrPrepend" "group-members-tbody"]
            else pure []
    _ -> pure []

-- | Predicate mirror of the /env/:name alert list query
-- (Web.Controller.Environments.renderEnv). Unlike /alerts, an empty status
-- selection shows ALL statuses there (closed included).
matchesEnvFilters :: (?modelContext :: ModelContext) => AlertListFilters -> Alert -> IO Bool
matchesEnvFilters filters alert = do
    groupOk <- case filters.alfGroup of
        Nothing -> pure True
        Just pattern -> case alert.groupId of
            Nothing -> pure False
            Just groupId -> do
                group <- fetch groupId
                pure (Text.isInfixOf (Text.toLower pattern) (Text.toLower group.groupKey))
    pure
        ( and
            [ null filters.alfSeverities || alert.severity `elem` filters.alfSeverities
            , null filters.alfStatuses || alert.status `elem` filters.alfStatuses
            , maybe True (\host -> effectiveFieldText FieldHost alert == Just host) filters.alfHost
            , maybe True (\service -> effectiveFieldText FieldService alert == Just service) filters.alfService
            , maybe True (\pattern -> Text.isInfixOf (Text.toLower pattern) (Text.toLower alert.title)) filters.alfTitle
            , groupOk
            ]
        )

cardMatches :: LiveEvent -> EnvCard -> Bool
cardMatches event card = event.leEnv == card.cardEnvName

-- CMDB/Jira/write-back panels refresh when enrichment or write-back state
-- changes land (milestone_3.md §3/§6).
contextPanelUpdates :: (?modelContext :: ModelContext, ?request :: Request, ?context :: Request) => Alert -> IO [Aeson.Value]
contextPanelUpdates alert = do
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
    jiraLinks <-
        query @JiraLink
            |> filterWhere (#alertId, get #id alert)
            |> orderByAsc #createdAt
            |> fetch
    linkedAssets <- AssetsCache.linkedAssetsForAlert alert
    assetConfigs <- forM linkedAssets \(_, object) -> fetch object.configId
    let linkedAssetEntries = zipWith (\(link, object) config -> (link, object, config)) linkedAssets assetConfigs
    agentRoles <-
        query @LlmAgentRole
            |> filterWhere (#enabled, True)
            |> orderByAsc #name
            |> fetch
    latestAttempt <-
        query @WriteBackAttempt
            |> filterWhere (#alertId, get #id alert)
            |> orderByDesc #createdAt
            |> limit 1
            |> fetchOneOrNothing
    analyses <-
        query @LlmAnalysis
            |> filterWhere (#alertId, get #id alert)
            |> orderByDesc #createdAt
            |> limit 10
            |> fetch
    llmJobErrors <- latestJobErrors (map (get #id) analyses)
    pure
        [ fragment cmdbPanelDomId (cmdbPanelHtml alert cmdbEntry) "replace" ""
        , fragment assetsPanelDomId (assetsPanelHtml alert linkedAssetEntries) "replace" ""
        , fragment jiraLinksDomId (jiraLinksHtml alert jiraLinks) "replace" ""
        , fragment writeBackChipDomId (writeBackChipHtml latestAttempt) "replace" ""
        , fragment llmPanelDomId (llmPanelHtml alert analyses [] llmJobErrors agentRoles) "replace" ""
        ]

fragment :: Text -> Markup -> Text -> Text -> Aeson.Value
fragment domId html mode parent =
    object ["id" .= domId, "html" .= renderMarkupText html, "mode" .= mode, "parent" .= parent]
