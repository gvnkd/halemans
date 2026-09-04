module Application.Connector.Alertmanager
( normalize
, normalizeSeverity
, AmSilence (..)
, silencesGet
, silenceCreate
, silenceDelete
, silenceCoversLabels
) where

import IHP.Prelude
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..))
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
import Data.Time.Calendar (fromGregorian)
import qualified Network.Wreq as Wreq
import Control.Lens ((&), (^.), (.~))
import Control.Exception (try, SomeException)

-- Alertmanager webhook payload (v4):
-- { "status": "firing", "externalURL": "...", "alerts": [ { "status",
--   "labels", "annotations", "startsAt", "endsAt", "generatorURL",
--   "fingerprint" } ] }
-- Milestone 2 hardening (milestone_2.md §8): multi-alert payloads, missing
-- or zero endsAt never auto-resolves, deep links fall back to externalURL.
normalize :: Value -> Either Text [NormalizedEvent]
normalize payload@(Object o) = case KeyMap.lookup "alerts" o of
    Just (Array alerts) -> mapM (toEvent (lookupText "externalURL" payload)) (Vector.toList alerts)
    _ -> Left "alertmanager payload: missing alerts array"
normalize _ = Left "alertmanager payload: not an object"

toEvent :: Maybe Text -> Value -> Either Text NormalizedEvent
toEvent externalUrl a@(Object _) = do
    let labels = fromMaybe (Object KeyMap.empty) (lookupKey "labels" a)
        annotations = fromMaybe (Object KeyMap.empty) (lookupKey "annotations" a)
        labelText k = lookupText k labels
        annotationText k = lookupText k annotations
    fp <- maybe (Left "alertmanager alert: missing fingerprint") Right (lookupText "fingerprint" a)
    let statusText = fromMaybe "firing" (lookupText "status" a)
        endsAt = lookupTime "endsAt" a
        -- A resolved status with a missing/zero endsAt is contradictory;
        -- alertmanager sends endsAt=0001-01-01 for still-firing alerts.
        status = if statusText == "resolved" && not (unknownEnd endsAt) then Resolved else Firing
        title = fromMaybe (fromMaybe "Alertmanager alert" (labelText "alertname")) (annotationText "summary")
    Right NormalizedEvent
        { fingerprint = "alertmanager:" <> fp
        , externalId = Just fp
        , status
        , severity = normalizeSeverity (labelText "severity")
        , title
        , description = fromMaybe "" (annotationText "description")
        , env = labelText "env"
        , host = labelText "host"
        , service = labelText "service"
        , checkName = labelText "check" <|> labelText "alertname"
        , labels
        , annotations
        , startedAt = lookupTime "startsAt" a
        , sourceUrl = lookupText "generatorURL" a <|> externalUrl
        }
toEvent _ _ = Left "alertmanager alert: not an object"

unknownEnd :: Maybe UTCTime -> Bool
unknownEnd Nothing = True
unknownEnd (Just time) = time <= epoch
    where epoch = UTCTime (fromGregorian 1 1 2) 0

lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupKey _ _ = Nothing

lookupText :: Text -> Value -> Maybe Text
lookupText k o = case lookupKey k o of
    Just (String t) -> Just t
    _ -> Nothing

lookupTime :: Text -> Value -> Maybe UTCTime
lookupTime k o = case lookupKey k o of
    Just (String t) -> case fromJSON (String t) of
        Success time -> Just time
        Error _ -> Nothing
    _ -> Nothing

normalizeSeverity :: Maybe Text -> Text
normalizeSeverity = \case
    Just "critical" -> "critical"
    Just "high" -> "high"
    Just "info" -> "info"
    _ -> "warning"

-- Silence API (milestone_3.md §6): write-back for alertmanager and grafana
-- sources (grafana's embedded alertmanager speaks the same v2 API under
-- /api/alertmanager/grafana/api/v2) and silence-based ack reconciliation.

data AmSilence = AmSilence
    { silenceId :: Text
    , silenceState :: Text          -- active | pending | expired
    , silenceMatchers :: [(Text, Text)]  -- equality matchers only
    , silenceCreatedBy :: Text
    , silenceComment :: Text
    , silenceUpdatedAt :: Maybe UTCTime
    } deriving (Eq, Show)

instance FromJSON AmSilence where
    parseJSON = withObject "AmSilence" \o -> do
        silenceId <- o .: "id"
        status <- o .:? "status"
        silenceState <- case status of
            Just s -> s .:? "state" .!= ""
            Nothing -> pure ""
        matchers <- o .:? "matchers" .!= []
        parsedMatchers <- forM matchers \m -> do
            name <- m .: "name"
            value <- m .: "value"
            isRegex <- m .:? "isRegex" .!= False
            pure (if isRegex then Nothing else Just (name, value))
        silenceCreatedBy <- o .:? "createdBy" .!= ""
        silenceComment <- o .:? "comment" .!= ""
        silenceUpdatedAt <- o .:? "updatedAt"
        pure AmSilence { silenceMatchers = mapMaybe id parsedMatchers, .. }

amOpts :: Maybe Text -> Wreq.Options
amOpts token = Wreq.defaults
    & Wreq.header "Authorization" .~ maybe [] (\t -> ["Bearer " <> cs t]) token

silencesGet :: Text -> Maybe Text -> Text -> IO (Either Text [AmSilence])
silencesGet baseUrl token apiPrefix = do
    result <- try (Wreq.getWith (amOpts token) (cs (baseUrl <> apiPrefix <> "/silences")))
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right response -> case eitherDecode (response ^. Wreq.responseBody) of
            Left err -> pure (Left (cs err))
            Right silences -> pure (Right silences)

-- | POST a silence; returns the new silence id.
silenceCreate :: Text -> Maybe Text -> Text -> Value -> IO (Either Text Text)
silenceCreate baseUrl token apiPrefix body = do
    result <- try (Wreq.postWith (amOpts token) (cs (baseUrl <> apiPrefix <> "/silences")) body)
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right response -> case eitherDecode (response ^. Wreq.responseBody) of
            Left err -> pure (Left (cs err))
            Right decoded -> do
                let silenceId = parseMaybe (withObject "silence" (.: "silenceID")) decoded
                pure (maybe (Left "alertmanager silence: no silenceID in response") Right silenceId)

silenceDelete :: Text -> Maybe Text -> Text -> Text -> IO (Either Text ())
silenceDelete baseUrl token apiPrefix silenceId = do
    result <- try (Wreq.deleteWith (amOpts token) (cs (baseUrl <> apiPrefix <> "/silence/" <> silenceId)))
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right _ -> pure (Right ())

-- | An active silence covers an alert when every equality matcher matches
-- the alert's labels.
silenceCoversLabels :: AmSilence -> Value -> Bool
silenceCoversLabels silence (Object labels) =
    silence.silenceState == "active"
        && all matcherMatches silence.silenceMatchers
    where
        matcherMatches (name, value) = case KeyMap.lookup (Key.fromText name) labels of
            Just (String labelValue) -> labelValue == value
            _ -> False
silenceCoversLabels _ _ = False
