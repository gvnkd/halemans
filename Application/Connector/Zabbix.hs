module Application.Connector.Zabbix
( ZabbixEvent (..)
, eventGet
, toNormalizedEvent
) where

import IHP.Prelude
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..))
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Network.Wreq as Wreq
import Control.Lens ((&), (^.), (.~))
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

-- | Fetch trigger events (problems and OKs) newer than the cursor.
eventGet :: Text -> Text -> Integer -> IO (Either Text [ZabbixEvent])
eventGet baseUrl token timeFrom = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        body = Aeson.object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= ("event.get" :: Text)
            , "id" .= (1 :: Int)
            , "params" .= Aeson.object
                [ "source" .= (0 :: Int)
                , "object" .= (0 :: Int)
                , "value" .= ([0, 1] :: [Int])
                , "time_from" .= timeFrom
                , "sortfield" .= (["clock", "eventid"] :: [Text])
                , "sortorder" .= ("ASC" :: Text)
                , "selectHosts" .= (["name"] :: [Text])
                , "limit" .= (1000 :: Int)
                ]
            ]
    resp <- Wreq.postWith opts (cs (baseUrl <> "/api_jsonrpc.php")) body
    case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
        Left err -> pure (Left (cs err))
        Right decoded ->
            case parseMaybe (Aeson.withObject "rpc" (.: "result")) decoded of
                Just events -> pure (Right events)
                Nothing -> pure (Left "zabbix rpc: response has no result field")

-- Fingerprint is trigger-scoped (a zabbix trigger has at most one open
-- problem at a time), so the OK event resolves the alert created by the
-- corresponding problem event.
toNormalizedEvent :: Text -> ZabbixEvent -> NormalizedEvent
toNormalizedEvent baseUrl event = NormalizedEvent
    { fingerprint = "zabbix:trigger:" <> event.triggerId
    , externalId = Just event.eventId
    , status = if event.value == "1" then Firing else Resolved
    , severity = severityFromZabbix event.severity
    , title = event.name
    , description = ""
    , env = Just "dev"
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
