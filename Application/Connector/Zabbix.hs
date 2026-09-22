module Application.Connector.Zabbix (
    ZabbixEvent (..),
    eventGet,
    ZabbixGroup (..),
    hostGroupsGetAll,
    toNormalizedEvent,
    eventAcknowledge,
    ZabbixEventAck (..),
    ZabbixAckRow (..),
    ackStateGet,
    ZabbixTriggerState (..),
    triggerStateGet,
    usersGet,
    ZabbixTriggerItem (..),
    triggerItemsGet,
    historyGet,
) where

import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..))
import qualified Application.Service.Http as Http
import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.List (nubBy)
import qualified Data.Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import IHP.Prelude
import qualified Network.Wreq as Wreq
import Text.Read (readMaybe)

-- A trigger event from zabbix event.get (source=0, object=0).
data ZabbixEvent = ZabbixEvent
    { eventId :: Text
    , triggerId :: Text
    , name :: Text
    , clock :: Integer
    , value :: Text -- "1" problem, "0" OK
    , severity :: Text -- "0".."5"
    , host :: Maybe Text
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ZabbixEvent where
    parseJSON = Aeson.withObject "ZabbixEvent" $ \o -> do
        eventId <- o .: "eventid"
        triggerId <- o .: "objectid"
        name <- o .: "name"
        clockText <- o .: "clock"
        clock <- maybe mempty pure (readMaybe clockText)
        value <- o .: "value"
        severity <- o .: "severity"
        hosts <- o .:? "hosts"
        let host = case hosts of
                Just (h : _) -> parseMaybe (Aeson.withObject "host" (.: "name")) h
                _ -> Nothing
        pure ZabbixEvent{..}

-- | Fetch trigger events (problems and OKs) newer than the cursor, paging
-- until a short page: after an outage the backlog can exceed one page and a
-- single truncated fetch would permanently skip the OK events past the page
-- boundary (stuck firing alerts). Pages overlap at the boundary clock second
-- (time_from is inclusive), so results are deduped by eventid. Empty groupIds
-- means no host group restriction.
eventGet :: Text -> Text -> Integer -> [Text] -> Int -> IO (Either Text [ZabbixEvent])
eventGet baseUrl token timeFrom groupIds pageLimit = go timeFrom []
  where
    go cursor acc = do
        result <- eventGetPage baseUrl token cursor groupIds pageLimit
        case result of
            Left err -> pure (Left err)
            Right page ->
                let acc' = acc ++ page
                    -- A full page whose max clock didn't move means more than
                    -- pageLimit events share one clock second; stop rather
                    -- than loop forever (reconcile covers any state we miss).
                    nextCursor = maximum (map (.clock) page)
                 in if length page < pageLimit || nextCursor <= cursor
                        then pure (Right (dedupEvents acc'))
                        else go nextCursor acc'
    dedupEvents = nubBy (\a b -> a.eventId == b.eventId)

eventGetPage :: Text -> Text -> Integer -> [Text] -> Int -> IO (Either Text [ZabbixEvent])
eventGetPage baseUrl token timeFrom groupIds pageLimit = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body =
            Aeson.object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "method" .= ("event.get" :: Text)
                , "id" .= (1 :: Int)
                , "params"
                    .= Aeson.object
                        ( [ "source" .= (0 :: Int)
                          , "object" .= (0 :: Int)
                          , "value" .= ([0, 1] :: [Int])
                          , "time_from" .= timeFrom
                          , "sortfield" .= (["clock", "eventid"] :: [Text])
                          , "sortorder" .= ("ASC" :: Text)
                          , "selectHosts" .= (["name"] :: [Text])
                          , "limit" .= pageLimit
                          ]
                            ++ ["groupids" .= groupIds | not (null groupIds)]
                        )
                ]
    resp <- Http.postFollowing opts (cs (baseUrl <> "/api_jsonrpc.php")) body
    case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
        Left err -> pure (Left (cs err))
        Right decoded ->
            case parseMaybe (Aeson.withObject "rpc" (.: "result")) decoded of
                Just events -> pure (Right events)
                Nothing -> pure (Left (rpcError decoded))

-- | A zabbix host group (hostgroup.get).
data ZabbixGroup = ZabbixGroup
    { groupId :: Text
    , groupName :: Text
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ZabbixGroup where
    parseJSON = Aeson.withObject "ZabbixGroup" $ \o -> do
        groupId <- o .: "groupid"
        groupName <- o .: "name"
        pure ZabbixGroup{..}

-- | Fetch ALL host groups (hostgroup.get, no filter). Host groups are
-- near-static; this backs the manual sync that populates the
-- zabbix_host_groups cache table.
hostGroupsGetAll :: Text -> Text -> IO (Either Text [ZabbixGroup])
hostGroupsGetAll baseUrl token = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body =
            Aeson.object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "method" .= ("hostgroup.get" :: Text)
                , "id" .= (1 :: Int)
                , "params"
                    .= Aeson.object
                        [ "output" .= (["groupid", "name"] :: [Text])
                        , "sortfield" .= (["name"] :: [Text])
                        ]
                ]
    resp <- Http.postFollowing opts (cs (baseUrl <> "/api_jsonrpc.php")) body
    case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
        Left err -> pure (Left (cs err))
        Right decoded ->
            case parseMaybe (Aeson.withObject "rpc" (.: "result")) decoded of
                Just groups -> pure (Right groups)
                Nothing -> pure (Left (rpcError decoded))

-- Fingerprint is trigger-scoped (a zabbix trigger has at most one open
-- problem at a time), so the OK event resolves the alert created by the
-- corresponding problem event. Zabbix events carry no environment concept,
-- so the env comes from the source row (sources.env).
toNormalizedEvent :: Text -> Text -> ZabbixEvent -> NormalizedEvent
toNormalizedEvent baseUrl envName event =
    NormalizedEvent
        { fingerprint = "zabbix:trigger:" <> event.triggerId
        , externalId = Just event.eventId
        , status = if event.value == "1" then Firing else Resolved
        , severity = severityFromZabbix event.severity
        , title = event.name
        , description = ""
        , env = Just envName
        , host = event.host
        , service = Nothing
        , checkName = Just event.name
        , labels = Aeson.object ["source" .= ("zabbix" :: Text)]
        , annotations = Aeson.object []
        , startedAt = Just (posixSecondsToUTCTime (fromIntegral event.clock))
        , sourceUrl = Just (baseUrl <> "/tr_events.php?triggerid=" <> event.triggerId <> "&eventid=" <> event.eventId)
        }

severityFromZabbix :: Text -> Text
severityFromZabbix = \case
    "5" -> "critical" -- disaster
    "4" -> "high" -- high
    "3" -> "warning" -- average
    "2" -> "warning" -- warning
    _ -> "info" -- information / not classified

-- Write-back + ack reconciliation (milestone_3.md §6). event.acknowledge
-- action bits (zabbix 7.0): 1 close, 2 acknowledge, 4 add message,
-- 8 change severity, 16 unacknowledge, 32 suppress, 64 unsuppress.

eventAcknowledge :: Text -> Text -> [Text] -> Int -> Text -> IO (Either Text ())
eventAcknowledge baseUrl token eventIds actionBits message = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body =
            Aeson.object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "method" .= ("event.acknowledge" :: Text)
                , "id" .= (1 :: Int)
                , "params"
                    .= Aeson.object
                        [ "eventids" .= eventIds
                        , "action" .= actionBits
                        , "message" .= message
                        ]
                ]
    result <- try (Http.postFollowing opts (cs (baseUrl <> "/api_jsonrpc.php")) body)
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right resp -> case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
            Left err -> pure (Left (cs err))
            Right decoded -> do
                let rpcResult :: Maybe Aeson.Value
                    rpcResult = parseMaybe (Aeson.withObject "rpc" (.: "result")) decoded
                case rpcResult of
                    Just _ -> pure (Right ())
                    Nothing -> pure (Left (rpcError decoded))

rpcError :: Aeson.Value -> Text
rpcError decoded = case errValue of
    Just err -> cs (Aeson.encode err)
    Nothing -> "zabbix rpc: unexpected response"
  where
    errValue :: Maybe Aeson.Value
    errValue = parseMaybe (Aeson.withObject "rpc" (.: "error")) decoded

data ZabbixAckRow = ZabbixAckRow
    { ackClock :: Integer
    , ackAction :: Integer
    , ackMessage :: Text
    , ackUserId :: Text
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ZabbixAckRow where
    parseJSON = Aeson.withObject "ZabbixAckRow" $ \o -> do
        clockText <- o .: "clock"
        ackClock <- maybe mempty pure (readMaybe clockText)
        actionText <- o .: "action"
        ackAction <- maybe mempty pure (readMaybe actionText)
        ackMessage <- o .:? "message" .!= ""
        ackUserId <- o .:? "userid" .!= ""
        pure ZabbixAckRow{..}

data ZabbixEventAck = ZabbixEventAck
    { ackEventId :: Text
    , ackAcknowledged :: Text -- "0" / "1"
    , ackRows :: [ZabbixAckRow]
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ZabbixEventAck where
    parseJSON = Aeson.withObject "ZabbixEventAck" $ \o -> do
        ackEventId <- o .: "eventid"
        ackAcknowledged <- o .:? "acknowledged" .!= "0"
        ackRows <- o .:? "acknowledges" .!= []
        pure ZabbixEventAck{..}

-- | Ack state for a known set of problem events (reverse reconciliation,
-- milestone_3.md §6): the poller re-fetches ack flags for alerts it already
-- knows about, since event.get by cursor only returns NEW events.
ackStateGet :: Text -> Text -> [Text] -> IO (Either Text [ZabbixEventAck])
ackStateGet baseUrl token eventIds = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body =
            Aeson.object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "method" .= ("event.get" :: Text)
                , "id" .= (1 :: Int)
                , "params"
                    .= Aeson.object
                        [ "eventids" .= eventIds
                        , "selectAcknowledges" .= ("extend" :: Text)
                        ]
                ]
    result <- try (Http.postFollowing opts (cs (baseUrl <> "/api_jsonrpc.php")) body)
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right resp -> case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
            Left err -> pure (Left (cs err))
            Right decoded ->
                case parseMaybe (Aeson.withObject "rpc" (.: "result")) decoded of
                    Just events -> pure (Right events)
                    Nothing -> pure (Left (rpcError decoded))

-- | Current state of a set of triggers (resolved-state reconciliation).
-- trigger.get reports the LIVE trigger value plus lastchange (when it last
-- flipped), so a missed OK event is recoverable no matter how old it is.
-- problem.get can't serve this: recent=false returns only UNRESOLVED
-- problems, recent=true only covers zabbix's ok_period — an old resolve is
-- indistinguishable from a housekeeper purge there. A trigger absent from
-- the result was deleted or is invisible to the token (permissions) — the
-- caller decides what that means. Disabled triggers keep their last value.
data ZabbixTriggerState = ZabbixTriggerState
    { triggerStateId :: Text
    , triggerStateValue :: Text -- "0" OK, "1" problem
    , triggerStateLastChange :: Integer -- unix time of last state flip
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ZabbixTriggerState where
    parseJSON = Aeson.withObject "ZabbixTriggerState" $ \o -> do
        triggerStateId <- o .: "triggerid"
        triggerStateValue <- o .:? "value" .!= "1"
        lastChangeText <- o .:? "lastchange" .!= "0"
        triggerStateLastChange <- maybe mempty pure (readMaybe lastChangeText)
        pure ZabbixTriggerState{..}

triggerStateGet :: Text -> Text -> [Text] -> IO (Either Text [ZabbixTriggerState])
triggerStateGet baseUrl token triggerIds = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body =
            Aeson.object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "method" .= ("trigger.get" :: Text)
                , "id" .= (1 :: Int)
                , "params"
                    .= Aeson.object
                        [ "triggerids" .= triggerIds
                        , "output" .= (["triggerid", "value", "lastchange"] :: [Text])
                        ]
                ]
    result <- try (Http.postFollowing opts (cs (baseUrl <> "/api_jsonrpc.php")) body)
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right resp -> case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
            Left err -> pure (Left (cs err))
            Right decoded ->
                case parseMaybe (Aeson.withObject "rpc" (.: "result")) decoded of
                    Just triggers -> pure (Right triggers)
                    Nothing -> pure (Left (rpcError decoded))

-- | userid -> display name (alias or name+surname), for ack attribution.
usersGet :: Text -> Text -> [Text] -> IO (Either Text [(Text, Text)])
usersGet baseUrl token userIds = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body =
            Aeson.object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "method" .= ("user.get" :: Text)
                , "id" .= (1 :: Int)
                , "params"
                    .= Aeson.object
                        [ "userids" .= userIds
                        , "output" .= (["userid", "username", "name", "surname"] :: [Text])
                        ]
                ]
    result <- try (Http.postFollowing opts (cs (baseUrl <> "/api_jsonrpc.php")) body)
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right resp -> case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
            Left err -> pure (Left (cs err))
            Right decoded ->
                case parseMaybe (Aeson.withObject "rpc" (.: "result")) decoded of
                    Just users -> pure (Right (mapMaybe userName users))
                    Nothing -> pure (Left (rpcError decoded))
  where
    userName = parseMaybe $ Aeson.withObject "user" \o -> do
        userId <- o .: "userid"
        username <- o .:? "username" .!= ""
        name <- o .:? "name" .!= ""
        surname <- o .:? "surname" .!= ""
        let display =
                if null username
                    then Data.Text.unwords (filter (not . null) [name, surname])
                    else username
        pure (userId, display)

-- Metric chart support (milestone 13): the alert fingerprint carries the
-- trigger id, trigger.get with selectItems returns the items referenced by
-- the trigger expression (server-resolved, templates included), and raw
-- numeric history is enough to plot — trends.get is not required.

data ZabbixTriggerItem = ZabbixTriggerItem
    { ztiTriggerId :: Text
    , ztiItemId :: Text
    , ztiKey :: Text
    , ztiName :: Text
    , ztiValueType :: Text -- "0" float, "3" unsigned are numeric; others skipped
    , ztiUnits :: Text
    , ztiExpression :: Text -- trigger-level expression (copied per item); may be ""
    }
    deriving (Eq, Show)

-- | Items referenced by the given triggers' expressions. Requires no mapping
-- table and no expression parsing; trigger read permission covers the items.
triggerItemsGet :: Text -> Text -> [Text] -> IO (Either Text [ZabbixTriggerItem])
triggerItemsGet baseUrl token triggerIds = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body =
            Aeson.object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "method" .= ("trigger.get" :: Text)
                , "id" .= (1 :: Int)
                , "params"
                    .= Aeson.object
                        [ "triggerids" .= triggerIds
                        , "output" .= (["triggerid", "expression"] :: [Text])
                        , "selectItems" .= (["itemid", "key_", "name", "value_type", "units"] :: [Text])
                        ]
                ]
    result <- try (Http.postFollowing opts (cs (baseUrl <> "/api_jsonrpc.php")) body)
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right resp -> case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
            Left err -> pure (Left (cs err))
            Right decoded ->
                case parseMaybe (Aeson.withObject "rpc" (.: "result")) decoded of
                    Just triggers -> pure (Right (concatMap flatten (triggers :: [Aeson.Value])))
                    Nothing -> pure (Left (rpcError decoded))
  where
    flatten trigger =
        [ ZabbixTriggerItem
            { ztiTriggerId = triggerId
            , ztiItemId = itemId
            , ztiKey = key
            , ztiName = name
            , ztiValueType = valueType
            , ztiUnits = units
            , ztiExpression = triggerExpression
            }
        | value <- itemList trigger
        , Just (itemId, key, name, valueType, units) <- [itemOf value]
        ]
      where
        triggerId = fromMaybe "" (parseMaybe (Aeson.withObject "trigger" (.: "triggerid")) trigger)
        triggerExpression = fromMaybe "" (parseMaybe (Aeson.withObject "trigger" (\o -> o .:? "expression" .!= "")) trigger)
    itemOf value = parseMaybe (Aeson.withObject "item" parseItem) value
    parseItem o = do
        itemId <- o .: "itemid"
        key <- o .:? "key_" .!= ""
        name <- o .:? "name" .!= ""
        valueType <- o .:? "value_type" .!= ""
        units <- o .:? "units" .!= ""
        pure (itemId, key, name, valueType, units)
    itemList trigger = fromMaybe [] (parseMaybe (Aeson.withObject "trigger" (.: "items")) trigger)

-- | Raw history points for one item, ascending by clock. historyType is the
-- zabbix history table selector (0 = float, 3 = unsigned); the caller picks
-- it from the item's value_type. Pages with limit + clock cursor because the
-- server caps results per call; time_from is inclusive, so the boundary
-- second is re-fetched and deduped here. Non-numeric values are skipped.
historyGet :: Text -> Text -> Text -> Int -> Integer -> Integer -> Int -> IO (Either Text [(UTCTime, Double)])
historyGet baseUrl token itemId historyType timeFrom timeTill pageLimit = do
    result <- go timeFrom []
    pure (map toUtcPoint . dedupPoints <$> result)
  where
    go cursor acc = do
        page <- historyPage cursor
        case page of
            Left err -> pure (Left err)
            Right rows ->
                let acc' = acc ++ rows
                    nextCursor = maximum (map (fst . toPair) rows)
                 in if length rows < pageLimit || nextCursor <= cursor
                        then pure (Right (map toPair acc'))
                        else go nextCursor acc'
    historyPage cursor = do
        let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
            body =
                Aeson.object
                    [ "jsonrpc" .= ("2.0" :: Text)
                    , "method" .= ("history.get" :: Text)
                    , "id" .= (1 :: Int)
                    , "params"
                        .= Aeson.object
                            [ "itemids" .= ([itemId] :: [Text])
                            , "history" .= historyType
                            , "time_from" .= cursor
                            , "time_till" .= timeTill
                            , "sortfield" .= (["clock"] :: [Text])
                            , "sortorder" .= ("ASC" :: Text)
                            , "limit" .= pageLimit
                            ]
                    ]
        rpcResult <- try (Http.postFollowing opts (cs (baseUrl <> "/api_jsonrpc.php")) body)
        case rpcResult of
            Left err -> pure (Left (tshow (err :: SomeException)))
            Right resp -> case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
                Left err -> pure (Left (cs err))
                Right decoded ->
                    case parseMaybe (Aeson.withObject "rpc" (.: "result")) decoded of
                        Just rows -> pure (Right (rows :: [Aeson.Value]))
                        Nothing -> pure (Left (rpcError decoded))
    toPair value =
        let clock = fromMaybe 0 (parseMaybe (Aeson.withObject "history" (.: "clock")) value >>= readMaybe)
            number = parseMaybe (Aeson.withObject "history" (.: "value")) value >>= readMaybe
         in (clock, fromMaybe 0 number :: Double)
    toUtcPoint (clock, value) = (posixSecondsToUTCTime (fromIntegral clock) :: UTCTime, value)
    dedupPoints = nubBy (\a b -> fst a == fst b)
