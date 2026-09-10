module Application.Connector.Zabbix
( ZabbixEvent (..)
, eventGet
, ZabbixGroup (..)
, hostGroupsGetAll
, toNormalizedEvent
, eventAcknowledge
, ZabbixEventAck (..)
, ZabbixAckRow (..)
, ackStateGet
, ZabbixProblemState (..)
, problemStateGet
, usersGet
) where

import IHP.Prelude
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..))
import Data.Aeson ((.:), (.:?), (.=), (.!=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Network.Wreq as Wreq
import qualified Application.Service.Http as Http
import Control.Lens ((&), (^.), (.~))
import Control.Exception (try, SomeException)
import Data.List (nubBy)
import qualified Data.Text
import Text.Read (readMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- A trigger event from zabbix event.get (source=0, object=0).
data ZabbixEvent = ZabbixEvent
    { eventId :: Text
    , triggerId :: Text
    , name :: Text
    , clock :: Integer
    , value :: Text        -- "1" problem, "0" OK
    , severity :: Text     -- "0".."5"
    , host :: Maybe Text
    } deriving (Eq, Show)

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
                Just (h:_) -> parseMaybe (Aeson.withObject "host" (.: "name")) h
                _ -> Nothing
        pure ZabbixEvent { .. }

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
        body = Aeson.object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= ("event.get" :: Text)
            , "id" .= (1 :: Int)
            , "params" .= Aeson.object
                ([ "source" .= (0 :: Int)
                , "object" .= (0 :: Int)
                , "value" .= ([0, 1] :: [Int])
                , "time_from" .= timeFrom
                , "sortfield" .= (["clock", "eventid"] :: [Text])
                , "sortorder" .= ("ASC" :: Text)
                , "selectHosts" .= (["name"] :: [Text])
                , "limit" .= pageLimit
                ] ++ [ "groupids" .= groupIds | not (null groupIds) ])
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
    } deriving (Eq, Show)

instance Aeson.FromJSON ZabbixGroup where
    parseJSON = Aeson.withObject "ZabbixGroup" $ \o -> do
        groupId <- o .: "groupid"
        groupName <- o .: "name"
        pure ZabbixGroup { .. }

-- | Fetch ALL host groups (hostgroup.get, no filter). Host groups are
-- near-static; this backs the manual sync that populates the
-- zabbix_host_groups cache table.
hostGroupsGetAll :: Text -> Text -> IO (Either Text [ZabbixGroup])
hostGroupsGetAll baseUrl token = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body = Aeson.object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= ("hostgroup.get" :: Text)
            , "id" .= (1 :: Int)
            , "params" .= Aeson.object
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
toNormalizedEvent baseUrl envName event = NormalizedEvent
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
    , labels = Aeson.object [ "source" .= ("zabbix" :: Text) ]
    , annotations = Aeson.object []
    , startedAt = Just (posixSecondsToUTCTime (fromIntegral event.clock))
    , sourceUrl = Just (baseUrl <> "/tr_events.php?triggerid=" <> event.triggerId <> "&eventid=" <> event.eventId)
    }

severityFromZabbix :: Text -> Text
severityFromZabbix = \case
    "5" -> "critical" -- disaster
    "4" -> "high"     -- high
    "3" -> "warning"  -- average
    "2" -> "warning"  -- warning
    _ -> "info"       -- information / not classified

-- Write-back + ack reconciliation (milestone_3.md §6). event.acknowledge
-- action bits (zabbix 7.0): 1 close, 2 acknowledge, 4 add message,
-- 8 change severity, 16 unacknowledge, 32 suppress, 64 unsuppress.

eventAcknowledge :: Text -> Text -> [Text] -> Int -> Text -> IO (Either Text ())
eventAcknowledge baseUrl token eventIds actionBits message = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body = Aeson.object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= ("event.acknowledge" :: Text)
            , "id" .= (1 :: Int)
            , "params" .= Aeson.object
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
    } deriving (Eq, Show)

instance Aeson.FromJSON ZabbixAckRow where
    parseJSON = Aeson.withObject "ZabbixAckRow" $ \o -> do
        clockText <- o .: "clock"
        ackClock <- maybe mempty pure (readMaybe clockText)
        actionText <- o .: "action"
        ackAction <- maybe mempty pure (readMaybe actionText)
        ackMessage <- o .:? "message" .!= ""
        ackUserId <- o .:? "userid" .!= ""
        pure ZabbixAckRow { .. }

data ZabbixEventAck = ZabbixEventAck
    { ackEventId :: Text
    , ackAcknowledged :: Text   -- "0" / "1"
    , ackRows :: [ZabbixAckRow]
    } deriving (Eq, Show)

instance Aeson.FromJSON ZabbixEventAck where
    parseJSON = Aeson.withObject "ZabbixEventAck" $ \o -> do
        ackEventId <- o .: "eventid"
        ackAcknowledged <- o .:? "acknowledged" .!= "0"
        ackRows <- o .:? "acknowledges" .!= []
        pure ZabbixEventAck { .. }

-- | Ack state for a known set of problem events (reverse reconciliation,
-- milestone_3.md §6): the poller re-fetches ack flags for alerts it already
-- knows about, since event.get by cursor only returns NEW events.
ackStateGet :: Text -> Text -> [Text] -> IO (Either Text [ZabbixEventAck])
ackStateGet baseUrl token eventIds = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body = Aeson.object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= ("event.get" :: Text)
            , "id" .= (1 :: Int)
            , "params" .= Aeson.object
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

-- | Problem state for a set of triggers (resolved-state reconciliation):
-- problem.get with recent=false returns open AND resolved problems, so the
-- poller can learn that a tracked trigger resolved while we weren't polling
-- (outage, truncated catch-up, housekeeper-purged OK events). A trigger with
-- no rows at all was purged or deleted — the caller decides what that means.
data ZabbixProblemState = ZabbixProblemState
    { problemEventId :: Text
    , problemTriggerId :: Text
    , problemClock :: Integer
    , problemREventId :: Text   -- "0" while the problem is open
    , problemRClock :: Integer  -- 0 while the problem is open
    } deriving (Eq, Show)

instance Aeson.FromJSON ZabbixProblemState where
    parseJSON = Aeson.withObject "ZabbixProblemState" $ \o -> do
        problemEventId <- o .: "eventid"
        problemTriggerId <- o .: "objectid"
        clockText <- o .: "clock"
        problemClock <- maybe mempty pure (readMaybe clockText)
        problemREventId <- o .:? "r_eventid" .!= "0"
        rClockText <- o .:? "r_clock" .!= "0"
        problemRClock <- maybe mempty pure (readMaybe rClockText)
        pure ZabbixProblemState { .. }

problemStateGet :: Text -> Text -> [Text] -> IO (Either Text [ZabbixProblemState])
problemStateGet baseUrl token triggerIds = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body = Aeson.object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= ("problem.get" :: Text)
            , "id" .= (1 :: Int)
            , "params" .= Aeson.object
                [ "triggerids" .= triggerIds
                , "recent" .= False
                , "output" .= (["eventid", "objectid", "clock", "r_eventid", "r_clock"] :: [Text])
                ]
            ]
    result <- try (Http.postFollowing opts (cs (baseUrl <> "/api_jsonrpc.php")) body)
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right resp -> case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
            Left err -> pure (Left (cs err))
            Right decoded ->
                case parseMaybe (Aeson.withObject "rpc" (.: "result")) decoded of
                    Just problems -> pure (Right problems)
                    Nothing -> pure (Left (rpcError decoded))

-- | userid -> display name (alias or name+surname), for ack attribution.
usersGet :: Text -> Text -> [Text] -> IO (Either Text [(Text, Text)])
usersGet baseUrl token userIds = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body = Aeson.object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= ("user.get" :: Text)
            , "id" .= (1 :: Int)
            , "params" .= Aeson.object
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
            let display = if null username
                    then Data.Text.unwords (filter (not . null) [name, surname])
                    else username
            pure (userId, display)
