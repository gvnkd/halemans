module Application.Connector.Alertmanager (normalize, normalizeSeverity) where

import IHP.Prelude
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..))
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
import Data.Time.Calendar (fromGregorian)

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
