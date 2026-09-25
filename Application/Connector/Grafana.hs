module Application.Connector.Grafana (normalize, normalizeAlertnameFirst, GrafanaAmAlert (..), alertsFold, amAlertToNormalized) where

import Application.Connector.Alertmanager (normalizeSeverity)
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..))
import qualified Application.Service.Http as Http
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Parser as AesonParser
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import IHP.Prelude
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import System.IO.Unsafe (unsafePerformIO)

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

-- Shared connection manager (Push.hs pattern): creating a manager per poll
-- cycle would leak its reaper thread every few seconds.
manager :: HTTP.Manager
manager = unsafePerformIO (HTTP.newManager TLS.tlsManagerSettings)
{-# NOINLINE manager #-}

-- | Streaming fold over the alertmanager listing (replaces the old
-- buffering alertsGet): the response is parsed incrementally, one alert at
-- a time, so a source with a huge listing never materializes the whole
-- array — the full aeson decode of a big source's listing OOMed the worker.
-- The fold callback runs per alert as it streams in; the accumulator is
-- typically a count plus a fingerprint set for absence reconciliation.
alertsFold :: Text -> Text -> (acc -> GrafanaAmAlert -> IO acc) -> acc -> IO (Either Text acc)
alertsFold baseUrl token stepAlert acc0 =
    Http.getFollowingStream
        manager
        (cs (baseUrl <> "/api/alertmanager/grafana/api/v2/alerts"))
        [("Authorization", "Bearer " <> cs token)]
        \response -> do
            let bodyReader = HTTP.responseBody response
            opened <- runStep bodyReader openArray BS.empty
            case opened of
                Left err -> pure (Left err)
                Right ((), leftover) -> foldAlerts bodyReader leftover acc0
  where
    foldAlerts bodyReader leftover acc = do
        stepped <- runStep bodyReader elementStep leftover
        case stepped of
            Left err -> pure (Left err)
            Right (Nothing, _) -> pure (Right acc)
            Right (Just (value, more), rest) -> case fromJSON value of
                Error err -> pure (Left (cs err))
                Success alert -> do
                    acc' <- stepAlert acc alert
                    if more
                        then foldAlerts bodyReader rest acc'
                        else pure (Right acc')

-- Incremental JSON-array stepping: openArray consumes the leading "[";
-- each elementStep invocation parses ONE element plus its trailing comma
-- (Bool True = more elements) or the closing "]" (Nothing / Bool False).
-- Leftover input threads from one step into the next, so only one parser
-- step's worth of bytes is ever retained.
openArray :: Atto.Parser ()
openArray = Atto.skipSpace >> Atto.char '[' >> pure ()

elementStep :: Atto.Parser (Maybe (Value, Bool))
elementStep = do
    Atto.skipSpace
    closer <- Atto.peekChar'
    if closer == ']'
        then Atto.char ']' >> pure Nothing
        else do
            value <- AesonParser.value
            Atto.skipSpace
            separator <- Atto.satisfy (\word -> word == ',' || word == ']')
            pure (Just (value, separator == ','))

-- | Runs one parser step against the streaming body, refilling from the
-- socket while attoparsec reports Partial; returns the result with any
-- unconsumed input.
runStep :: HTTP.BodyReader -> Atto.Parser step -> BS.ByteString -> IO (Either Text (step, BS.ByteString))
runStep bodyReader parser initial = advance (Atto.parse parser initial)
  where
    advance (Atto.Done leftover result) = pure (Right (result, leftover))
    advance (Atto.Fail _ _ err) = pure (Left (cs err))
    advance (Atto.Partial continue) = do
        chunk <- HTTP.brRead bodyReader
        if BS.null chunk
            then finish (continue chunk)
            else advance (continue chunk)
    finish (Atto.Done leftover result) = pure (Right (result, leftover))
    finish (Atto.Partial _) = pure (Left "alerts listing: unexpected end of input")
    finish (Atto.Fail _ _ err) = pure (Left (cs err))

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
