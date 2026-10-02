module Application.Service.Live (
    Scope (..),
    liveBroadcastLoop,
    ensureBroadcaster,
    liveConnectionCount,
    -- agent chat turn completion, backstop for a lost SSE done frame
    broadcastAgentTurn,
    -- exposed for Test.LiveSpec (milestone 12 §7)
    isResetFrame,
    parseScope,
    registry,
) where

import Application.Helper.DashboardConfig (DashboardCard (..), clauseValue, decodeDashboardConfig, matchCardAlert)
import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)
import Application.Service.AlertList (AlertListFilters (..), defaultAlertListFilters, matchesFilters, parseAlertFilters, seenCutoff)
import Application.Service.AlertScope (alertScopeBypassFromSettings, alertVisibleWith, scopeNamesFor)
import qualified Application.Service.Assets.Cache as AssetsCache
import Application.Service.DashboardCards (ExpandedCard (..), expandDashboardCards, expandedDomId)
import Application.Service.Llm.Queue (latestJobErrors)
import Application.Service.Timeline (headTimelineGroup, timelineHiddenKind)
import qualified Control.Exception.Safe as Exception
import Control.Monad (guard)
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
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord, currentUserOrNothing)
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
    | -- this browser tab has the agent chat widget open; frames carry the
      -- user id because the agent chat is per-user, not per-page
      ScopeAgentUser UUID
    | ScopeNone
    deriving (Eq, Show)

-- The second component is the logged-in user's id (Nothing for anonymous
-- pages), so broadcast-time checks (per-user host group alert visibility)
-- never trust client-sent scope data.
type ConnectionEntry = (UUID, Maybe UUID, IORef [Scope], Text -> IO ())

userIdUuid :: Id User -> UUID
userIdUuid (Id uuid) = uuid

registry :: IORef [ConnectionEntry]
registry = unsafePerformIO (newIORef [])
{-# NOINLINE registry #-}

broadcasterStarted :: IORef Bool
broadcasterStarted = unsafePerformIO (newIORef False)
{-# NOINLINE broadcasterStarted #-}

-- Live connection gauge for the /metrics exporter (milestone_6.md §5).
liveConnectionCount :: IO Int
liveConnectionCount = length <$> readIORef registry

-- | Push an agent-chat turn completion to every tab of this user. Backstop
-- channel for the SSE stream: the reply is persisted BEFORE this is called,
-- so a browser that lost the done frame renders immediately instead of
-- waiting for the stall timer's history poll.
broadcastAgentTurn :: UUID -> Aeson.Value -> IO ()
broadcastAgentTurn userId payload = do
    connections <- readIORef registry
    forM_ connections \(_, _, scopeRef, send) -> do
        scopes <- readIORef scopeRef
        when (ScopeAgentUser userId `elem` scopes) do
            -- one dead peer must not take down the fan-out (or, worse, the
            -- agent turn thread that called us before emitting SSE done)
            send (cs (Aeson.encode (object ["agent_turn" .= payload])))
                `Exception.catch` \(_ :: Exception.SomeException) -> pure ()

-- | Connection loop of the /ws WSApp (see Web.Controller.Live).
liveBroadcastLoop ::
    (?request :: Request, ?modelContext :: ModelContext, ?connection :: WS.Connection, CurrentUserRecord ~ User) =>
    IO ()
liveBroadcastLoop = do
    connectionId <- UUIDV4.nextRandom
    scopeRef <- newIORef []
    let send = WS.sendTextData ?connection
    let user = currentUserOrNothing
        userUuid = case user of
            Just u -> Just (userIdUuid (get #id u))
            Nothing -> Nothing
    modifyIORef' registry ((connectionId, userUuid, scopeRef, send) :)
    flip Exception.finally (modifyIORef' registry (filter (\(id, _, _, _) -> id /= connectionId))) do
        forever do
            message <- receiveData @LByteString
            if isResetFrame message
                -- The client navigated (turbolinks swaps pages without
                -- reopening the socket): drop the previous page's scopes
                -- before it sends the new ones.
                then writeIORef scopeRef []
                else case parseAgentScope message <|> parseScope message of
                    -- A page may subscribe to several scopes (dashboard pages
                    -- cover one env:<name> scope per included card, §7).
                    Just scope -> modifyIORef' scopeRef (scope :)
                    Nothing -> pure ()

-- The agent scope frame is {type:"agent"}; the user id comes from the
-- session, not the frame (the client doesn't know its own id).
parseAgentScope :: (?request :: Request) => LByteString -> Maybe Scope
parseAgentScope message = do
    guard (not (isResetFrame message))
    value <- Aeson.decode message
    scopeType <- parseMaybe (Aeson.withObject "frame" (\o -> o Aeson..:? "type")) value :: Maybe (Maybe Text)
    guard (scopeType == Just "agent")
    user <- currentUserOrNothing
    let Id uuid = get #id user
    pure (ScopeAgentUser uuid)

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
    , leHost :: Maybe Text
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
        host <- o Aeson..:? "host"
        kind <- o Aeson..: "kind"
        title <- o Aeson..:? "title"
        severity <- o Aeson..:? "severity"
        pure LiveEvent{leAlertId = alertId, leGroupId = groupId, leEnv = env, leHost = host, leKind = kind, leTitle = title, leSeverity = severity}
  where
    parseUuid raw = maybe (fail "bad uuid") pure (UUID.fromText raw)

broadcast :: (?request :: Request) => ModelContext -> PGListener.Notification -> IO ()
broadcast modelContext notification = do
    let ?modelContext = modelContext
    let ?context = ?request
    case parseLiveEvent notification.notificationData of
        Nothing -> pure ()
        Just event -> do
            -- The banner popup (in-page + browser Notification) must respect
            -- the same per-user host group visibility as the list rows, so
            -- the alert row is fetched once and checked per connection.
            bannerAlert <- case (isBanner event, event.leAlertId) of
                (True, Just alertId) -> fetchOneOrNothing (Id alertId :: Id Alert)
                _ -> pure Nothing
            connections <- readIORef registry
            forM_ connections \(_, userUuid, scopeRef, send) -> do
                scopes <- readIORef scopeRef
                updates <- concat <$> forM scopes \scope -> updatesFor userUuid scope event
                banner <- bannerForConnection userUuid event bannerAlert
                unless (null updates && isNothing banner) do
                    send (cs (Aeson.encode (object ["updates" .= updates, "banner" .= banner])))

isBanner :: LiveEvent -> Bool
isBanner event = event.leKind == "created" && event.leSeverity `elem` [Just "critical", Just "high"] && isJust event.leAlertId

-- The new-alert popup for one connection: Nothing for non-banner events or
-- when the alert is invisible to the user's team host group scope (the same
-- AlertScope predicate the /alerts list query applies).
bannerForConnection :: (?modelContext :: ModelContext) => Maybe UUID -> LiveEvent -> Maybe Alert -> IO (Maybe Aeson.Value)
bannerForConnection _ _ Nothing = pure Nothing
bannerForConnection userUuid event (Just alert) = do
    scope <- scopeNamesForConnection userUuid
    visible <- scopeVisible scope alert
    pure $
        if visible
            then Just (object ["title" .= event.leTitle, "severity" .= event.leSeverity, "host" .= event.leHost, "alertId" .= event.leAlertId])
            else Nothing

-- Per-user host group visibility for a websocket connection: Nothing user
-- (anonymous page) or bypass on = unrestricted; otherwise the union of the
-- user's teams' zabbix host groups (AlertScope).
scopeNamesForConnection :: (?modelContext :: ModelContext) => Maybe UUID -> IO (Maybe [Text])
scopeNamesForConnection Nothing = pure Nothing
scopeNamesForConnection (Just uuid) = do
    user <- fetchOneOrNothing (Id uuid :: Id User)
    case user of
        Nothing -> pure Nothing
        Just u
            | alertScopeBypassFromSettings u.settings -> pure Nothing
            | otherwise -> scopeNamesFor (get #id u)

-- Visibility leg alone (for the env page, which has its own filter predicate).
scopeVisible :: (?modelContext :: ModelContext) => Maybe [Text] -> Alert -> IO Bool
scopeVisible Nothing _ = pure True
scopeVisible (Just names) alert = do
    isZabbix <- case alert.sourceId of
        Nothing -> pure False
        Just sourceId -> maybe False (\source -> source.type_ == "zabbix") <$> fetchOneOrNothing sourceId
    pure (alertVisibleWith names isZabbix alert)

-- | Compute the fragment updates relevant for a connection scope. userUuid is
-- the connection's logged-in user: the alert-list scopes apply the SAME
-- server-side per-user host group visibility as the list query (AlertScope).
updatesFor :: (?modelContext :: ModelContext, ?request :: Request, ?context :: Request) => Maybe UUID -> Scope -> LiveEvent -> IO [Aeson.Value]
updatesFor userUuid scope event = case (scope, event.leAlertId, event.leGroupId) of
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
        scope <- scopeNamesForConnection userUuid
        matches <- matchesFilters scopeFilters scope alert
        -- Rows re-render with the subscribed view's visible columns; the
        -- group key is fetched only when the group column is shown.
        groupKey <- case alert.groupId of
            Just groupId | "group" `elem` scopeFilters.alfColumns -> do
                group <- fetch groupId
                pure (Just group.groupKey)
            _ -> pure Nothing
        pure
            [ if matches
                then fragment (alertRowDomId alert) (alertRowHtmlCols groupKey scopeFilters.alfColumns alert) "replaceOrPrepend" "alerts-tbody"
                else object ["id" .= alertRowDomId alert, "mode" .= ("remove" :: Text)]
            ]
    (ScopeEnv name scopeFilters, Just alertId, _)
        | event.leEnv == Just name -> do
            alert <- fetch (Id alertId)
            scope <- scopeNamesForConnection userUuid
            matches <- (&&) <$> matchesEnvFilters scopeFilters alert <*> scopeVisible scope alert
            groupKey <- case alert.groupId of
                Just groupId | "group" `elem` scopeFilters.alfColumns -> do
                    group <- fetch groupId
                    pure (Just group.groupKey)
                _ -> pure Nothing
            pure
                [ if matches
                    then fragment (alertRowDomId alert) (alertRowHtmlCols groupKey scopeFilters.alfColumns alert) "replaceOrPrepend" "env-alerts-tbody"
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
                    ackedByName <- forM alert.acknowledgedBy \userId -> do
                        user <- fetch userId
                        pure user.displayName
                    pure
                        ( [ fragment (alertStatusDomId alert) (alertStatusBadgeHtml alert) "replace" ""
                          , fragment alertDetailsDomId (alertDetailsCardHtml alert ackedByName) "replace" ""
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
        Just pat -> case alert.groupId of
            Nothing -> pure False
            Just groupId -> do
                group <- fetch groupId
                pure (Text.isInfixOf (Text.toLower pat) (Text.toLower group.groupKey))
    now <- getCurrentTime
    pure
        ( and
            [ null filters.alfSeverities || alert.severity `elem` filters.alfSeverities
            , null filters.alfStatuses || alert.status `elem` filters.alfStatuses
            , maybe True (\host -> effectiveFieldText FieldHost alert == Just host) filters.alfHost
            , maybe True (\service -> effectiveFieldText FieldService alert == Just service) filters.alfService
            , maybe True (\pat -> Text.isInfixOf (Text.toLower pat) (Text.toLower alert.title)) filters.alfTitle
            , groupOk
            , maybe True (\n -> alert.occurrences >= n) filters.alfMinOccurrences
            , alert.lastSeenAt > seenCutoff now filters.alfSeenWithin
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
