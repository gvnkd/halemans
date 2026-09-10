module Application.Job.PollZabbix where

import IHP.Prelude
import IHP.FrameworkConfig (FrameworkConfig (..))
import IHP.Job.Types
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Generated.Types
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest, ingestEvents)
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
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
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
                    outcome <- try (Zabbix.eventGet source.baseUrl token cursor groupIds (eventPageLimit source))
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
                            when (reconcileResolvedEnabled source && reconcileDue now source) do
                                reconcileProblemStates source token now
                                void (source |> set #lastReconcileAt (Just now) |> updateRecord)
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

-- Resolved-state reconciliation: cursor-based event.get only returns NEW
-- events, so an OK event missed during an outage (truncated catch-up page,
-- housekeeper-purged history) leaves the local alert firing forever. Each
-- due cycle re-fetches problem state for the triggers behind our tracked
-- (firing/ack) alerts via ONE batched problem.get and locally resolves
-- whatever zabbix no longer reports as open. Keyed on the trigger id from
-- the fingerprint, not alerts.external_id: refires don't rotate external_id,
-- so the stored event id can point at an already-resolved older problem.
--
-- Source config keys (all optional, defaults in the accessors below):
--   reconcileResolved           bool  master switch (default true)
--   reconcileGraceSeconds       int   min age of last local activity before
--                                     trusting source state (default 60)
--   reconcileIntervalSeconds    int   min seconds between reconciles,
--                                     0 = every poll cycle (default 0)
--   absentResolveMinAgeSeconds  int   min alert age before "no problem rows
--                                     at all" is treated as resolved
--                                     (housekeeper purge or deleted trigger;
--                                     guards permission gaps) (default 86400)
--   eventPageLimit              int   event.get page size (default 1000)
reconcileProblemStates :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Source -> Text -> UTCTime -> IO ()
reconcileProblemStates source token now = do
    alerts <- query @Alert
        |> filterWhere (#sourceId, Just (get #id source))
        |> filterWhereIn (#status, ["firing", "ack"] :: [Text])
        |> fetch
    let tracked = [(triggerId, alert) | alert <- alerts, Just triggerId <- [triggerIdOf alert]]
    unless (null tracked) do
        result <- Zabbix.problemStateGet source.baseUrl token (nub (map fst tracked))
        case result of
            Left err -> logWarn ("zabbix source \"" <> source.name <> "\" problem-state reconcile failed: " <> err)
            Right states -> do
                let latest = latestProblemByTrigger states
                forM_ tracked \(triggerId, alert) ->
                    case resolveDecision now source alert (Map.lookup triggerId latest) of
                        Nothing -> pure ()
                        Just resolvedAt -> do
                            logInfo ("zabbix source \"" <> source.name <> "\": resolving alert " <> tshow (get #id alert) <> " (no open problem on " <> triggerId <> ")")
                            resolveFromProblem source alert resolvedAt

-- | Resolve via the normal ingest path (state machine, events, WS fan-out),
-- then back-date resolved_at to the zabbix-side resolution time when known.
resolveFromProblem :: (?modelContext :: ModelContext) => Source -> Alert -> UTCTime -> IO ()
resolveFromProblem source alert resolvedAt = do
    mAlertId <- ingest source NormalizedEvent
        { fingerprint = alert.fingerprint
        , externalId = alert.externalId
        , status = Resolved
        , severity = alert.severity
        , title = alert.title
        , description = alert.description
        , env = alert.env
        , host = alert.host
        , service = alert.service
        , checkName = alert.checkName
        , labels = alert.labels
        , annotations = alert.annotations
        , startedAt = alert.startedAt
        , sourceUrl = alert.sourceUrl
        }
    forM_ mAlertId \alertId ->
        void (sqlExecTyped [typedSql|
            UPDATE alerts SET resolved_at = ${resolvedAt}
            WHERE id = ${alertId} AND status = 'resolved' AND resolved_at > ${resolvedAt}
        |])

-- | Just resolvedAt when the alert should be locally resolved; Nothing when
-- the source still reports an open problem or local activity is too fresh to
-- trust source state (grace window covers the ingest/reconcile race).
resolveDecision :: UTCTime -> Source -> Alert -> Maybe Zabbix.ZabbixProblemState -> Maybe UTCTime
resolveDecision now source alert mLatest
    | alert.lastSeenAt >= graceCutoff = Nothing
    | otherwise = case mLatest of
        Just state
            | state.problemREventId == "0" -> Nothing
            | state.problemRClock > 0 -> Just (min now (posixSecondsToUTCTime (fromIntegral state.problemRClock)))
            | otherwise -> Just now
        Nothing
            | addUTCTime (negate absentMinAge) now >= fromMaybe now alert.startedAt -> Just now
            | otherwise -> Nothing
  where
    graceCutoff = addUTCTime (negate (fromIntegral (reconcileGraceSeconds source))) now
    absentMinAge = fromIntegral (absentResolveMinAgeSeconds source)

-- | The row that tells the CURRENT state of a trigger: its newest problem.
latestProblemByTrigger :: [Zabbix.ZabbixProblemState] -> Map Text Zabbix.ZabbixProblemState
latestProblemByTrigger = Map.fromListWith newer . map (\state -> (state.problemTriggerId, state))
  where
    newer a b = if (a.problemClock, a.problemEventId) >= (b.problemClock, b.problemEventId) then a else b

triggerIdOf :: Alert -> Maybe Text
triggerIdOf alert = Text.stripPrefix "zabbix:trigger:" alert.fingerprint

reconcileDue :: UTCTime -> Source -> Bool
reconcileDue now source = case source.lastReconcileAt of
    Nothing -> True
    Just lastAt -> addUTCTime (fromIntegral (reconcileIntervalSeconds source)) lastAt <= now

reconcileResolvedEnabled :: Source -> Bool
reconcileResolvedEnabled = configBool True "reconcileResolved"

reconcileGraceSeconds :: Source -> Int
reconcileGraceSeconds = configInt 60 "reconcileGraceSeconds"

reconcileIntervalSeconds :: Source -> Int
reconcileIntervalSeconds = configInt 0 "reconcileIntervalSeconds"

absentResolveMinAgeSeconds :: Source -> Int
absentResolveMinAgeSeconds = configInt 86400 "absentResolveMinAgeSeconds"

eventPageLimit :: Source -> Int
eventPageLimit source = max 1 (configInt 1000 "eventPageLimit" source)

configInt :: Int -> Text -> Source -> Int
configInt def key source = fromMaybe def (parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: Key.fromText key)) source.config)

configBool :: Bool -> Text -> Source -> Bool
configBool def key source = fromMaybe def (parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: Key.fromText key)) source.config)
