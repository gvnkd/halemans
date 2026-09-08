module Application.Service.Live
( Scope (..)
, liveBroadcastLoop
, ensureBroadcaster
, liveConnectionCount
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.Fetch (fetch, fetchOne, fetchOneOrNothing)
import IHP.QueryBuilder (query, filterWhere, orderByAsc, orderByDesc, limit)
import IHP.FrameworkConfig (FrameworkConfig (..))
import IHP.RequestVault ()
import Generated.Types
import IHP.WebSocket
import qualified IHP.PGListener as PGListener
import qualified Network.WebSockets as WS
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe, Parser)
import Data.Aeson (object, (.=))
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception.Safe as Exception
import IHP.HSX.Markup (renderMarkupText)
import IHP.HSX.Markup (Markup)
import qualified Data.ByteString.Lazy as BL
import Network.Wai (Request)
import Web.View.Fragments
import Web.View.Dashboard.Index (computeEnvCards, EnvCard (..), renderCard, cardDomId)
import Application.Service.AlertList (AlertListFilters, defaultAlertListFilters, matchesFilters, parseAlertFilters)
import Application.Service.Llm.Queue (latestJobErrors)
import qualified Application.Service.Assets.Cache as AssetsCache

-- Websocket fan-out (milestone_1.md §7): one PG LISTEN subscription per
-- process; each browser connection registers its scope in the registry and
-- receives pre-rendered HSX fragments.

data Scope
    = ScopeDashboard
    | ScopeAlerts AlertListFilters
    | ScopeEnv Text
    | ScopeAlert UUID
    | ScopeGroup UUID
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
liveBroadcastLoop
    :: (?request :: Request, ?modelContext :: ModelContext, ?connection :: WS.Connection)
    => IO ()
liveBroadcastLoop = do
    connectionId <- UUIDV4.nextRandom
    scopeRef <- newIORef []
    let send = WS.sendTextData ?connection
    modifyIORef' registry ((connectionId, scopeRef, send) :)
    flip Exception.finally (modifyIORef' registry (filter (\(id, _, _) -> id /= connectionId))) do
        forever do
            message <- receiveData @LByteString
            case parseScope message of
                -- A page may subscribe to several scopes (dashboard pages
                -- cover one env:<name> scope per included card, §7).
                Just scope -> modifyIORef' scopeRef (scope :)
                Nothing -> pure ()

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
            "env" -> ScopeEnv <$> o Aeson..: "name"
            "alert" -> ScopeAlert <$> o Aeson..: "id"
            "group" -> ScopeGroup <$> o Aeson..: "id"
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
        pure LiveEvent { leAlertId = alertId, leGroupId = groupId, leEnv = env, leKind = kind, leTitle = title, leSeverity = severity }
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
    (ScopeAlerts scopeFilters, Just alertId, _) -> do
        alert <- fetch (Id alertId)
        matches <- matchesFilters scopeFilters alert
        pure [ if matches
            then fragment (alertRowDomId alert) (alertRowHtml alert) "replaceOrPrepend" "alerts-tbody"
            else object [ "id" .= alertRowDomId alert, "mode" .= ("remove" :: Text) ]
            ]
    (ScopeEnv name, Just alertId, _)
        | event.leEnv == Just name -> rowUpdate "env-alerts-tbody" alertId
        | otherwise -> pure []
    (ScopeAlert alertUuid, Just alertId, _)
        | alertId == alertUuid -> do
            alert <- fetch (Id alertId)
            latestEvent <- query @AlertEvent
                |> filterWhere (#alertId, Id alertId)
                |> orderByDesc #createdAt
                |> limit 1
                |> fetchOne
            -- Panel-only kinds (milestone_8.md §4: "assets" refreshes the
            -- context panels without touching status badge or timeline).
            if event.leKind == "assets"
                then contextPanelUpdates alert
                else do
                    panelUpdates <- if event.leKind `elem` ["enriched", "writeback", "writeback_failed"]
                        then contextPanelUpdates alert
                        else pure []
                    pure
                        ( [ fragment (alertStatusDomId alert) (alertStatusBadgeHtml alert) "replace" ""
                          , fragment timelineDomId (timelineEventHtml latestEvent) "prepend" ""
                          ] ++ panelUpdates )
        | otherwise -> pure []
    -- Group events (kind "group", milestone_2.md §9): the group card header
    -- for group-scoped connections, the env page group row for env scopes
    -- (replace-only: a no-op while the page is in flat view).
    (ScopeGroup groupUuid, _, Just groupId)
        | groupId == groupUuid -> do
            group <- fetch (Id groupId)
            pure [fragment (groupHeaderDomId group) (groupHeaderHtml group) "replace" ""]
        | otherwise -> pure []
    (ScopeEnv name, _, Just groupId)
        | event.leEnv == Just name -> do
            group <- fetch (Id groupId)
            members <- query @Alert
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
    where
        rowUpdate parentId alertId = do
            alert <- fetch (Id alertId)
            pure [fragment (alertRowDomId alert) (alertRowHtml alert) "replaceOrPrepend" parentId]

cardMatches :: LiveEvent -> EnvCard -> Bool
cardMatches event card = case card.cardEnvironment of
    Just environment -> event.leEnv == Just environment.name
    Nothing -> isNothing event.leEnv

-- CMDB/Jira/write-back panels refresh when enrichment or write-back state
-- changes land (milestone_3.md §3/§6).
contextPanelUpdates :: (?modelContext :: ModelContext, ?request :: Request, ?context :: Request) => Alert -> IO [Aeson.Value]
contextPanelUpdates alert = do
    cmdbEntry <- case (alert.hostId, alert.serviceId) of
        (Just hostId, _) -> query @CmdbEntry
            |> filterWhere (#hostId, Just hostId)
            |> fetchOneOrNothing
        (Nothing, Just serviceId) -> query @CmdbEntry
            |> filterWhere (#serviceId, Just serviceId)
            |> fetchOneOrNothing
        (Nothing, Nothing) -> pure Nothing
    jiraLinks <- query @JiraLink
        |> filterWhere (#alertId, get #id alert)
        |> orderByAsc #createdAt
        |> fetch
    linkedAssets <- AssetsCache.linkedAssetsForAlert alert
    assetConfigs <- forM linkedAssets \(_, object) -> fetch object.configId
    let linkedAssetEntries = zipWith (\(link, object) config -> (link, object, config)) linkedAssets assetConfigs
    agentRoles <- query @LlmAgentRole
        |> filterWhere (#enabled, True)
        |> orderByAsc #name
        |> fetch
    latestAttempt <- query @WriteBackAttempt
        |> filterWhere (#alertId, get #id alert)
        |> orderByDesc #createdAt
        |> limit 1
        |> fetchOneOrNothing
    analyses <- query @LlmAnalysis
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
    object [ "id" .= domId, "html" .= renderMarkupText html, "mode" .= mode, "parent" .= parent ]
