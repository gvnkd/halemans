module Application.Service.WriteBack
( enqueueForAction
, executeAttempt
, backoffSeconds
, maxWriteBackAttempts
, silenceMatchersFor
, silenceEndsAt
, zabbixActionBits
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch, fetchOneOrNothing)
import Generated.Types
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Application.Connector.Zabbix as Zabbix
import qualified Application.Connector.Alertmanager as Am
import Application.Helper.Ingest (publishAlertUpdate)
import Control.Monad (void)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import System.Environment (lookupEnv)

-- Hybrid write-back (design_docs/milestone_3.md §6): user-initiated
-- ack/unack/close inserts a write_back_attempts row and enqueues a
-- WriteBackJob; the job is a thin executor over the attempt row so retries
-- survive worker restarts and the alert card reads its status chip from the
-- row directly. System actions (actor Nothing: auto-close, reconciliation
-- mirrors) never enqueue, so reverse reconciliation can never loop back.

maxWriteBackAttempts :: Int
maxWriteBackAttempts = 5

-- | 1m / 5m / 15m then 15m caps (§6).
backoffSeconds :: Int -> Int
backoffSeconds attempts
    | attempts <= 1 = 60
    | attempts == 2 = 300
    | otherwise = 900

-- | event.acknowledge bits: ack = 2|4, unack = 16|4, close = 1|4.
zabbixActionBits :: Text -> Int
zabbixActionBits = \case
    "ack" -> 6
    "unack" -> 20
    "close" -> 5
    _ -> 4

-- | One equality matcher per alert label.
silenceMatchersFor :: Alert -> Value
silenceMatchersFor alert = case alert.labels of
    Aeson.Object labels -> Aeson.toJSON
        [ object ["name" .= Key.toText name, "value" .= value, "isRegex" .= False]
        | (name, Aeson.String value) <- KeyMap.toList labels
        ]
    _ -> Aeson.toJSON ([] :: [Value])

-- | Silence horizon: ack uses the ack expiry (or 24h), close always 24h (§6).
silenceEndsAt :: UTCTime -> Text -> Alert -> UTCTime
silenceEndsAt now action alert = case action of
    "ack" -> fromMaybe fallback alert.ackExpiresAt
    _ -> fallback
    where
        fallback = addUTCTime 86400 now

writeBackEnabled :: Source -> Bool
writeBackEnabled source =
    parseMaybe (Aeson.withObject "config" (\o -> o Aeson..: "writeBack")) source.config == Just True

enqueueForAction :: (?modelContext :: ModelContext) => Alert -> Text -> IO ()
enqueueForAction alert action = do
    forM_ alert.sourceId \sourceId -> do
        source <- fetch sourceId
        if not (writeBackEnabled source)
            then pure ()
            else if source.type_ == "webhook"
                then void do
                    newRecord @WriteBackAttempt
                        |> set #alertId (get #id alert)
                        |> set #action action
                        |> set #sourceId sourceId
                        |> set #status "done"
                        |> set #lastError (Just "unsupported: generic webhook sources have no write-back")
                        |> createRecord
                else do
                    attempt <- newRecord @WriteBackAttempt
                        |> set #alertId (get #id alert)
                        |> set #action action
                        |> set #sourceId sourceId
                        |> set #status "queued"
                        |> createRecord
                    void do
                        newRecord @WriteBackJob
                            |> set #attemptId (get #id attempt)
                            |> createRecord

executeAttempt :: (?modelContext :: ModelContext) => WriteBackAttempt -> IO ()
executeAttempt attempt = do
    alert <- fetch attempt.alertId
    source <- fetch attempt.sourceId
    now <- getCurrentTime
    result <- dispatch now alert source attempt
    case result of
        Right silenceId -> do
            _ <- attempt
                |> set #status "done"
                |> set #attempts (attempt.attempts + 1)
                |> set #silenceId silenceId
                |> set #updatedAt now
                |> updateRecord
            publishAlertUpdate alert "writeback"
        Left err -> do
            let attempts = attempt.attempts + 1
            maxAttempts <- maxAttemptsConfigured
            if attempts >= maxAttempts
                then do
                    _ <- attempt
                        |> set #status "failed"
                        |> set #attempts attempts
                        |> set #lastError (Just err)
                        |> set #updatedAt now
                        |> updateRecord
                    void do
                        newRecord @AlertEvent
                            |> set #alertId (get #id alert)
                            |> set #userId Nothing
                            |> set #kind "writeback_failed"
                            |> set #payload (object
                                [ "source" .= get #name source
                                , "action" .= attempt.action
                                , "error" .= err
                                ])
                            |> createRecord
                    publishAlertUpdate alert "writeback"
                else do
                    _ <- attempt
                        |> set #attempts attempts
                        |> set #lastError (Just err)
                        |> set #updatedAt now
                        |> updateRecord
                    retryAfter <- retryDelay attempts
                    void do
                        newRecord @WriteBackJob
                            |> set #attemptId (get #id attempt)
                            |> set #runAt (addUTCTime (fromIntegral retryAfter) now)
                            |> createRecord

-- Test override: HALEMANS_WRITEBACK_BACKOFF_SECONDS="0,0,0" shortens the
-- retry schedule; HALEMANS_WRITEBACK_MAX_ATTEMPTS lowers the cap.
retryDelay :: Int -> IO Int
retryDelay attempts = do
    override <- lookupEnv "HALEMANS_WRITEBACK_BACKOFF_SECONDS"
    pure case override of
        Just raw -> case mapMaybe (readMaybe . cs) (Text.splitOn "," (cs raw)) of
            [] -> backoffSeconds attempts
            schedule -> schedule !! min (attempts - 1) (length schedule - 1)
        Nothing -> backoffSeconds attempts

maxAttemptsConfigured :: IO Int
maxAttemptsConfigured = do
    override <- lookupEnv "HALEMANS_WRITEBACK_MAX_ATTEMPTS"
    pure (fromMaybe maxWriteBackAttempts (override >>= (readMaybe . cs)))

dispatch :: (?modelContext :: ModelContext) => UTCTime -> Alert -> Source -> WriteBackAttempt -> IO (Either Text (Maybe Text))
dispatch now alert source attempt = case source.type_ of
    "zabbix" -> case alert.externalId of
        Nothing -> pure (Left "alert has no zabbix event id")
        Just eventId -> do
            token <- sourceToken source
            case token of
                Nothing -> pure (Left "source token not available")
                Just token -> do
                    actor <- actorName alert attempt.action
                    let message = attempt.action <> " by " <> actor <> " via Halemans"
                    result <- Zabbix.eventAcknowledge source.baseUrl token [eventId] (zabbixActionBits attempt.action) message
                    pure (fmap (const Nothing) result)
    "alertmanager" -> amDispatch now alert source Nothing attempt "/api/v2"
    "grafana" -> do
        token <- sourceToken source
        amDispatch now alert source token attempt "/api/alertmanager/grafana/api/v2"
    _ -> pure (Right Nothing) -- webhook attempts are resolved at enqueue time

amDispatch :: (?modelContext :: ModelContext) => UTCTime -> Alert -> Source -> Maybe Text -> WriteBackAttempt -> Text -> IO (Either Text (Maybe Text))
amDispatch now alert source token attempt apiPrefix = case attempt.action of
    "unack" -> do
        prior <- query @WriteBackAttempt
            |> filterWhere (#alertId, get #id alert)
            |> filterWhere (#sourceId, get #id source)
            |> filterWhere (#status, "done" :: Text)
            |> orderByDesc #createdAt
            |> fetch
        case mapMaybe (.silenceId) prior of
            [] -> pure (Right Nothing) -- nothing to expire
            (silenceId:_) -> do
                result <- Am.silenceDelete source.baseUrl token apiPrefix silenceId
                pure (fmap (const Nothing) result)
    action -> do
        let body = object
                [ "matchers" .= silenceMatchersFor alert
                , "startsAt" .= now
                , "endsAt" .= silenceEndsAt now action alert
                , "createdBy" .= ("halemans" :: Text)
                , "comment" .= (action <> " via Halemans (silence-based write-back)" :: Text)
                ]
        result <- Am.silenceCreate source.baseUrl token apiPrefix body
        pure (fmap Just result)

sourceToken :: Source -> IO (Maybe Text)
sourceToken source = do
    let tokenEnv :: Maybe Text
        tokenEnv = parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: "tokenEnv")) source.config
    case tokenEnv of
        Just envVar -> fmap cs <$> lookupEnv (cs envVar)
        Nothing -> pure Nothing

actorName :: (?modelContext :: ModelContext) => Alert -> Text -> IO Text
actorName alert action = do
    let userRef = case action of
            "ack" -> alert.acknowledgedBy
            "close" -> alert.closedBy
            _ -> Nothing
    case userRef of
        Just userId -> do
            user <- fetch userId
            pure user.email
        Nothing -> pure "halemans"
