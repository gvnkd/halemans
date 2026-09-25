module Application.Connector.Grafana (normalize, normalizeAlertnameFirst, GrafanaAmAlert (..), alertsGet, amAlertToNormalized) where

import Application.Connector.Alertmanager (normalizeSeverity)
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..))
import qualified Application.Service.Http as Http
import Control.Lens ((&), (.~), (^.))
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import IHP.Prelude
import qualified Network.Wreq as Wreq

-- Grafana unified alerting webhook payload:
-- { "status": "firing", "title": "...", "alerts": [ { "status", "labels",
--   "annotations", "startsAt", "endsAt", "fingerprint", "generatorURL" } ] }
--
-- Two title contracts, picked by the SOURCE TYPE at the hook (see
-- Web.Controller.Hooks): sources of type "grafana" take the rule name
-- (alertname label) as the title — summary/description annotations are body
-- text; every other source keeps the legacy contract where annotations.summary
-- is the title (arbitrary alertmanager-style senders often carry a static
-- alertname and the human text in summary).
normalize :: Value -> Either Text [NormalizedEvent]
normalize = normalizeWith PreferSummary

normalizeAlertnameFirst :: Value -> Either Text [NormalizedEvent]
normalizeAlertnameFirst = normalizeWith PreferAlertname

data TitleMode = PreferSummary | PreferAlertname

normalizeWith :: TitleMode -> Value -> Either Text [NormalizedEvent]
normalizeWith mode (Object o) = case KeyMap.lookup "alerts" o of
    Just (Array alerts) -> mapM (toEvent mode) (Vector.toList alerts)
    _ -> Left "grafana payload: missing alerts array"
normalizeWith _ _ = Left "grafana payload: not an object"

-- Grafana's `instance` label is the monitored host identity; `host` is a
-- fallback for manually labelled alerts.
hostFromLabels :: (Text -> Maybe Text) -> Maybe Text
hostFromLabels labelText = labelText "instance" <|> labelText "host"

toEvent :: TitleMode -> Value -> Either Text NormalizedEvent
toEvent mode a@(Object _) = do
    let labels = fromMaybe (Object KeyMap.empty) (lookupKey "labels" a)
        annotations = fromMaybe (Object KeyMap.empty) (lookupKey "annotations" a)
        labelText k = lookupText k labels
        annotationText k = lookupText k annotations
    fp <- maybe (Left "grafana alert: missing fingerprint") Right (lookupText "fingerprint" a)
    let statusText = fromMaybe "firing" (lookupText "status" a)
        status = if statusText == "resolved" then Resolved else Firing
        title = case mode of
            PreferAlertname ->
                fromMaybe (fromMaybe "Grafana alert" (labelText "check")) (labelText "alertname")
            PreferSummary ->
                fromMaybe (fromMaybe "Grafana alert" (labelText "check")) (annotationText "summary")
        description = joinParts [annotationText "summary", annotationText "description"]
    Right
        NormalizedEvent
            { fingerprint = "grafana:" <> fp
            , externalId = Just fp
            , status
            , severity = normalizeSeverity (labelText "severity")
            , title
            , description = description
            , env = labelText "env"
            , host = hostFromLabels labelText
            , service = labelText "service"
            , checkName = labelText "check"
            , labels
            , annotations
            , startedAt = lookupTime "startsAt" a
            , sourceUrl = lookupText "generatorURL" a
            }
toEvent _ _ = Left "grafana alert: not an object"

-- | Body text: non-empty parts joined with a blank line, deduped (a summary
-- copied verbatim into description must not render twice).
joinParts :: [Maybe Text] -> Text
joinParts parts =
    Text.intercalate "\n\n" (nub [p | Just p <- parts, not (Text.null (Text.strip p))])

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

-- Poller path (milestone_2.md §8): the embedded alertmanager's
-- /api/alertmanager/grafana/api/v2/alerts listing, used by PollGrafanaJob to
-- reconcile states missed by the webhook. Fingerprints are identical to the
-- webhook path ("grafana:<fingerprint>") so both dedupe onto one alert.

data GrafanaAmAlert = GrafanaAmAlert
    { amFingerprint :: Text
    , amLabels :: Value
    , amAnnotations :: Value
    , amStartsAt :: Maybe UTCTime
    , amEndsAt :: Maybe UTCTime
    , amUpdatedAt :: Maybe UTCTime
    , amGeneratorUrl :: Maybe Text
    }
    deriving (Eq, Show)

instance FromJSON GrafanaAmAlert where
    parseJSON = Aeson.withObject "GrafanaAmAlert" \o -> do
        amFingerprint <- o .: "fingerprint"
        amLabels <- o .:? "labels" .!= Object KeyMap.empty
        amAnnotations <- o .:? "annotations" .!= Object KeyMap.empty
        amStartsAt <- o .:? "startsAt"
        amEndsAt <- o .:? "endsAt"
        amUpdatedAt <- o .:? "updatedAt"
        amGeneratorUrl <- o .:? "generatorURL"
        pure GrafanaAmAlert{..}

alertsGet :: Text -> Text -> IO (Either Text [GrafanaAmAlert])
alertsGet baseUrl token = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
    response <- Http.getFollowing opts (cs (baseUrl <> "/api/alertmanager/grafana/api/v2/alerts"))
    case eitherDecode (response ^. Wreq.responseBody) of
        Left err -> pure (Left (cs err))
        Right alerts -> pure (Right alerts)

-- | Zero/missing endsAt means firing-with-unknown-end (§8); an endsAt in the
-- past means the alert resolved while we were not listening. Title/severity
-- resolution matches the webhook path so both create the same alert row.
amAlertToNormalized :: UTCTime -> GrafanaAmAlert -> NormalizedEvent
amAlertToNormalized now amAlert =
    let labelText k = textAt k amAlert.amLabels
        annotationText k = textAt k amAlert.amAnnotations
        resolved = case amAlert.amEndsAt of
            Just endsAt -> endsAt <= now
            Nothing -> False
     in NormalizedEvent
            { fingerprint = "grafana:" <> amAlert.amFingerprint
            , externalId = Just amAlert.amFingerprint
            , status = if resolved then Resolved else Firing
            , severity = normalizeSeverity (labelText "severity" <|> annotationText "severity")
            , title = fromMaybe (fromMaybe "Grafana alert" (labelText "alertname")) (labelText "check")
            , description = joinParts [annotationText "summary", annotationText "description"]
            , env = labelText "env"
            , host = hostFromLabels labelText
            , service = labelText "service"
            , checkName = labelText "check"
            , labels = amAlert.amLabels
            , annotations = amAlert.amAnnotations
            , startedAt = amAlert.amStartsAt
            , sourceUrl = amAlert.amGeneratorUrl
            }
  where
    textAt k value = case value of
        Object o -> case KeyMap.lookup (Key.fromText k) o of
            Just (String t) -> Just t
            _ -> Nothing
        _ -> Nothing
