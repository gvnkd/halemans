module Application.Job.PollZabbix where

import IHP.Prelude
import IHP.FrameworkConfig (FrameworkConfig (..))
import IHP.Job.Types
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Generated.Types
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingestEvents)
import Application.Service.Reconcile (shouldMirror, mirrorExternalAck, mirrorExternalUnack)
import Application.Service.SourceHealth (pollDue, recordFailure, recordSuccess)
import Application.Service.HostGroups (HostGroupScope (..), hostGroupScope, teamHostGroupNames)
import Application.Service.Log (logDebug, logInfo, logWarn)
import qualified Application.Connector.Zabbix as Zabbix
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Bits ((.&.))
import Data.Either (fromRight)
import Data.List (nub, sortOn)
import Control.Monad (void)
import Control.Exception (try, SomeException)
import System.Environment (lookupEnv)

-- Self-rescheduling zabbix poller (milestone 0). Seeded by EnqueuePollers
-- (via `seed`); drops duplicate pending siblings before rescheduling so
-- re-running seed never spawns a second loop. Stops rescheduling when no
-- enabled zabbix sources exist; creating/enabling one re-arms the loop via
-- Application.Service.PollerControl.
instance Job PollZabbixJob where
    perform _job = do
        now <- getCurrentTime
        sources <- query @Source
            |> filterWhere (#type_, "zabbix" :: Text)
            |> filterWhere (#enabled, True)
            |> fetch
        forM_ (filter (pollDue now) sources) pollSource

        if null sources
            then do
                logInfo "no enabled zabbix sources; poll loop stopped (re-arms on source create/enable)"
                void $ sqlExecTyped [typedSql|
                    DELETE FROM poll_zabbix_jobs
                    WHERE status = 'job_status_not_started'
                |]
            else do
                now <- getCurrentTime
                next <- newRecord @PollZabbixJob
                    |> set #runAt (addUTCTime 5 now)
                    |> createRecord
                let nextId = get #id next
                void $ sqlExecTyped [typedSql|
                    DELETE FROM poll_zabbix_jobs
                    WHERE status = 'job_status_not_started' AND id <> ${nextId}
                |]

    -- Pick up future-run_at reschedules quickly (default is 60s).
    queuePollInterval = 3 * 1000000
    maxAttempts = 3

pollSource :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Source -> IO ()
pollSource source = do
    let tokenEnv :: Maybe Text
        tokenEnv = parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: "tokenEnv")) source.config
    token <- case tokenEnv of
        Just envVar -> fmap cs <$> lookupEnv (cs envVar)
        Nothing -> pure Nothing
    case token of
        Nothing -> logDebug ("zabbix source \"" <> source.name <> "\": token env var " <> fromMaybe "<none configured>" tokenEnv <> " not set; skipping poll cycle")
        Just token -> do
            now <- getCurrentTime
            let cursor = initialCursor now source
            scopeOutcome <- try (resolveGroupIds source token)
            scope <- pure case scopeOutcome of
                Left err -> Left (tshow (err :: SomeException))
                Right result -> result
            case scope of
                Left err -> do
                    recordFailure source err
                    logWarn ("zabbix source \"" <> source.name <> "\" host group scope failed: " <> err)
                Right Nothing -> logDebug ("zabbix source \"" <> source.name <> "\": hostGroupScope=teams but no cached groups match; skipping poll cycle")
                Right (Just groupIds) -> do
                    outcome <- try (Zabbix.eventGet source.baseUrl token cursor groupIds)
                    result <- pure case outcome of
                        Left err -> Left (tshow (err :: SomeException))
                        Right result -> result
                    case result of
                        Left err -> do
                            recordFailure source err
                            logWarn ("zabbix source \"" <> source.name <> "\" poll failed: " <> err)
                        Right events -> do
                            logDebug ("zabbix source \"" <> source.name <> "\": event.get returned " <> tshow (length events) <> " events")
                            recordSuccess source
                            ingestEvents source (map (Zabbix.toNormalizedEvent source.baseUrl source.env) events)
                            reconcileAcks source token
                            case maximumMaybe (map (.clock) events) of
                                Just maxClock -> do
                                    _ <- source
                                        |> set #lastSyncCursor (Just (posixSecondsToUTCTime (fromIntegral maxClock)))
                                        |> updateRecord
                                    pure ()
                                Nothing -> pure ()

-- | Host group ids for event.get: Right Nothing means skip this cycle
-- (hostGroupScope=teams but no cached group matches the teams' names — never
-- synced yet, or no team has groups configured — so fetch nothing rather than
-- everything). ScopeAll yields Just [] (no restriction). Names are resolved
-- against the zabbix_host_groups cache, populated by manual sync
-- (SyncHostGroupsAction); host groups are near-static so no API call here.
resolveGroupIds :: (?modelContext :: ModelContext) => Source -> Text -> IO (Either Text (Maybe [Text]))
resolveGroupIds source _token = case hostGroupScope source of
    ScopeAll -> pure (Right (Just []))
    ScopeTeams -> do
        teams <- query @Team |> fetch
        case teamHostGroupNames teams of
            [] -> pure (Right Nothing)
            names -> do
                rows <- query @ZabbixHostGroup
                    |> filterWhere (#sourceId, get #id source)
                    |> filterWhereIn (#name, names)
                    |> fetch
                pure case rows of
                    [] -> Right Nothing
                    _ -> Right (Just (map (.groupId) rows))

maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just (maximum xs)

-- | event.get time_from for a poll. With a stored cursor this is the cursor;
-- the FIRST poll of a source is bounded to config.initialHistoryDays back
-- from now (default 1 day) so attaching to a zabbix with years of history
-- doesn't ingest all of it. Set the key higher to import older history.
initialCursor :: UTCTime -> Source -> Integer
initialCursor now source =
    case source.lastSyncCursor of
        Just cursor -> floor (utcTimeToPOSIXSeconds cursor)
        Nothing -> floor (utcTimeToPOSIXSeconds (addUTCTime (negate (fromIntegral days * 86400)) now))
  where
    days = initialHistoryDays source

initialHistoryDays :: Source -> Int
initialHistoryDays source =
    fromMaybe 1 (parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: Key.fromText "initialHistoryDays")) source.config)

-- Reverse reconciliation (milestone_3.md §6): re-fetch ack flags for the
-- alerts we already track (cursor-based event.get only returns NEW events,
-- so acks on existing problems never show up there). LWW: source state
-- newer than the last local action mirrors in, older never clobbers.
reconcileAcks :: (?modelContext :: ModelContext) => Source -> Text -> IO ()
reconcileAcks source token = do
    alerts <- query @Alert
        |> filterWhere (#sourceId, Just (get #id source))
        |> filterWhereIn (#status, ["firing", "ack"] :: [Text])
        |> fetch
    let eventIds = mapMaybe (.externalId) alerts
    unless (null eventIds) do
        result <- Zabbix.ackStateGet source.baseUrl token eventIds
        case result of
            Left _err -> pure ()
            Right states -> do
                let userIds = nub [row.ackUserId | state <- states, row <- state.ackRows, row.ackUserId /= ""]
                usersResult <- if null userIds
                    then pure (Right [])
                    else Zabbix.usersGet source.baseUrl token userIds
                let userNames = fromRight [] usersResult
                forM_ states (mirrorState alerts userNames)

mirrorState :: (?modelContext :: ModelContext) => [Alert] -> [(Text, Text)] -> Zabbix.ZabbixEventAck -> IO ()
mirrorState alerts userNames state =
    case find (\alert -> alert.externalId == Just state.ackEventId) alerts of
        Nothing -> pure ()
        Just alert -> do
            let rows = sortOn (.ackClock) state.ackRows
                latestUnack = lastMaybe [row | row <- rows, row.ackAction .&. 16 /= 0]
                latestAction = lastMaybe rows
                actorOf row = fromMaybe row.ackUserId (lookup row.ackUserId userNames)
            case (state.ackAcknowledged, alert.status) of
                ("1", "firing") -> forM_ latestAction \row -> do
                    let sourceAt = posixSecondsToUTCTime (fromIntegral row.ackClock)
                    when (shouldMirror alert.acknowledgedAt sourceAt) do
                        void (mirrorExternalAck alert "zabbix" (actorOf row) sourceAt)
                ("0", "ack") -> forM_ latestUnack \row -> do
                    let sourceAt = posixSecondsToUTCTime (fromIntegral row.ackClock)
                    when (shouldMirror alert.acknowledgedAt sourceAt) do
                        void (mirrorExternalUnack alert "zabbix" (actorOf row) sourceAt)
                _ -> pure ()

lastMaybe :: [a] -> Maybe a
lastMaybe = last
