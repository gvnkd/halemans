module Application.Connector.Grafana (normalize) where

import IHP.Prelude
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..))
import Application.Connector.Alertmanager (normalizeSeverity)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector

-- Grafana unified alerting webhook payload:
-- { "status": "firing", "title": "...", "alerts": [ { "status", "labels",
--   "annotations", "startsAt", "endsAt", "fingerprint", "generatorURL" } ] }
normalize :: Value -> Either Text [NormalizedEvent]
normalize (Object o) = case KeyMap.lookup "alerts" o of
    Just (Array alerts) -> mapM toEvent (Vector.toList alerts)
    _ -> Left "grafana payload: missing alerts array"
normalize _ = Left "grafana payload: not an object"

toEvent :: Value -> Either Text NormalizedEvent
toEvent a@(Object _) = do
    let labels = fromMaybe (Object KeyMap.empty) (lookupKey "labels" a)
        annotations = fromMaybe (Object KeyMap.empty) (lookupKey "annotations" a)
        labelText k = lookupText k labels
        annotationText k = lookupText k annotations
    fp <- maybe (Left "grafana alert: missing fingerprint") Right (lookupText "fingerprint" a)
    let statusText = fromMaybe "firing" (lookupText "status" a)
        status = if statusText == "resolved" then Resolved else Firing
        title = fromMaybe (fromMaybe "Grafana alert" (labelText "check")) (annotationText "summary")
    Right NormalizedEvent
        { fingerprint = "grafana:" <> fp
        , externalId = Just fp
        , status
        , severity = normalizeSeverity (labelText "severity")
        , title
        , description = fromMaybe "" (annotationText "description")
        , env = labelText "env"
        , host = labelText "host"
        , service = labelText "service"
        , checkName = labelText "check"
        , labels
        , annotations
        , startedAt = lookupTime "startsAt" a
        , sourceUrl = lookupText "generatorURL" a
        }
toEvent _ = Left "grafana alert: not an object"

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
