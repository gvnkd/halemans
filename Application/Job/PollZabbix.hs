module Application.Job.PollZabbix where

import qualified Application.Connector.Zabbix as Zabbix
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), fetchActiveBlackouts, ingestEvents, transitionAlert)
import Application.Pipeline.Actions (addComment)
import Application.Pipeline.Blackouts (alertSubject, blackoutApplies)
import Application.Service.HostGroups (HostGroupScope (..), hostGroupScope, teamHostGroupNames)
import Application.Service.Log (logDebug, logInfo, logWarn)
import Application.Service.Reconcile (mirrorExternalAck, mirrorExternalSuppress, mirrorExternalUnack, mirrorExternalUnsuppress, shouldMirror)
import Application.Service.SourceHealth (pollDue, recordFailure, recordReconcileFailure, recordReconcileSuccess, recordSuccess)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseMaybe)
import Data.Bits ((.&.))
import Data.Either (fromRight)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.FrameworkConfig (FrameworkConfig (..))
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
import System.Environment (lookupEnv)

-- Self-rescheduling zabbix poller (milestone 0). Seeded by EnqueuePollers
-- (via `seed`); drops duplicate pending siblings before rescheduling so
-- re-running seed never spawns a second loop. Stops rescheduling when no
-- enabled zabbix sources exist; creating/enabling one re-arms the loop via
-- Application.Service.PollerControl.
instance Job PollZabbixJob where
    perform job = do
        -- Single-loop guard: EnqueuePollers inserts unconditionally on every
        -- deploy and the pending-sibling cleanup can't stop two loops from
        -- RUNNING at once — concurrent performs double-ingest the same event
        -- batch and create duplicate alert rows (the ambiguous fingerprint
        -- dedupe then feeds one and starves the other). The older-created
        -- running job wins; this one stops without rescheduling.
        let createdAt = job.createdAt
        olderRunning <-
            sqlQueryTyped
                [typedSql|
            SELECT count(*) FROM poll_zabbix_jobs
            WHERE status = 'job_status_running' AND created_at < ${createdAt}
        |]
        case olderRunning of
            (count_ : _)
                | count_ > 0 ->
                    logWarn "duplicate PollZabbixJob loop detected (an older poll job is running); stopping this one"
            _ -> do
                now <- getCurrentTime
                sources <-
                    query @Source
                        |> filterWhere (#type_, "zabbix" :: Text)
                        |> filterWhere (#enabled, True)
                        |> fetch
                forM_ (filter (pollDue now) sources) pollSource

                if null sources
                    then do
                        logInfo "no enabled zabbix sources; poll loop stopped (re-arms on source create/enable)"
                        void $
                            sqlExecTyped
                                [typedSql|
                            DELETE FROM poll_zabbix_jobs
                            WHERE status = 'job_status_not_started'
                        |]
                    else reschedule

    -- Pick up future-run_at reschedules quickly (default is 60s).
    queuePollInterval = 3 * 1000000
    maxAttempts = 3

-- Guarded reschedule: insert only when nothing is pending, so a duplicate
-- loop's reschedule no-ops and the loop dies instead of fighting over (or
-- deleting) the surviving loop's successor row.
reschedule :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => IO ()
reschedule = do
    now <- getCurrentTime
    let runAt = addUTCTime 5 now
    inserted <-
        sqlQueryTyped
            [typedSql|
        INSERT INTO poll_zabbix_jobs (run_at)
        SELECT ${runAt}
        WHERE NOT EXISTS (SELECT 1 FROM poll_zabbix_jobs WHERE status = 'job_status_not_started')
        RETURNING id
    |]
    case inserted of
        (nextId : _) ->
            void $
                sqlExecTyped
                    [typedSql|
            DELETE FROM poll_zabbix_jobs
            WHERE status = 'job_status_not_started' AND id <> ${nextId}
        |]
        [] -> logInfo "another PollZabbixJob is already pending; stopping this loop"

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
                    outcome <- try (Zabbix.eventGetFold source.baseUrl token cursor groupIds (eventPageLimit source) (ingestPage source token) emptyPageAcc)
                    result <- pure case outcome of
                        Left err -> Left (tshow (err :: SomeException))
                        Right result -> result
                    case result of
                        Left err -> do
                            -- Cursor NOT advanced: the fold aborted on a page
                            -- fetch or a host-group lookup, so the next cycle
                            -- refetches from the last good cursor and the
                            -- already-ingested pages dedupe by fingerprint —
                            -- rather than skipping unprocessed events.
                            recordFailure source err
                            logWarn ("zabbix source \"" <> source.name <> "\" poll failed: " <> err)
                        Right acc -> do
                            logDebug ("zabbix source \"" <> source.name <> "\": event.get returned " <> tshow (paCount acc) <> " events")
                            recordSuccess source
                            syncUngroupedHostAlerts source (Set.toList (paUngrouped acc)) [host | (host, groups) <- Map.toList (paGroups acc), not (null groups)]
                            reconcileAcks source token
                            when (reconcileDue now source) do
                                when (reconcileResolvedEnabled source) do
                                    reconcileProblemStates source token now
                                when (scanMissingProblemsEnabled source) do
                                    scanMissingProblems source token groupIds now
                                void (source |> set #lastReconcileAt (Just now) |> updateRecord)
                            -- +1s: event.get's time_from is INCLUSIVE, so a
                            -- cursor at maxClock re-ingests the boundary event
                            -- every cycle (resolved→resolved no-op flood, and
                            -- occurrences inflation when the boundary event is
                            -- a problem). Same-second events are all in this
                            -- paged fetch, so nothing is skipped. Written only
                            -- after the whole fold succeeded, so the cursor
                            -- never passes an event that failed to ingest.
                            case paMaxClock acc of
                                Just maxClock -> do
                                    _ <-
                                        source
                                            |> set #lastSyncCursor (Just (posixSecondsToUTCTime (fromIntegral (maxClock + 1))))
                                            |> updateRecord
                                    pure ()
                                Nothing -> pure ()

-- | Per-cycle state threaded through the paged event fold: a running count,
-- the max clock seen (the cursor frontier), a host -> groups cache (a []
-- value is a negative entry: looked up and ungrouped/invisible to the
-- token), and the hosts seen ungrouped this cycle. Everything is bounded by
-- the page size and the host inventory — never by the event backlog, which
-- is what OOMed the worker on sources with the host group filter removed.
data PageAcc = PageAcc
    { paCount :: Int
    , paMaxClock :: Maybe Integer
    , paGroups :: Map.Map Text [Text]
    , paUngrouped :: Set.Set Text
    }

emptyPageAcc :: PageAcc
emptyPageAcc = PageAcc 0 Nothing Map.empty Set.empty

-- | One page of the event fold: resolve host groups for the page's hosts
-- that are not in the cycle cache yet (one batched host.get per page), drop
-- ungrouped-host events, ingest the rest, and thread the accumulator. Left
-- aborts the whole fold (e.g. a host-group lookup failure); the caller does
-- not advance the cursor, so the next cycle refetches from the last good
-- cursor and the already-ingested pages dedupe by fingerprint.
ingestPage :: (?modelContext :: ModelContext) => Source -> Text -> PageAcc -> [Zabbix.ZabbixEvent] -> IO (Either Text PageAcc)
ingestPage source token acc page = do
    let pageHosts = nub (mapMaybe (.host) page)
        unknownHosts = [host | host <- pageHosts, not (Map.member host (paGroups acc))]
    fetched <-
        if null unknownHosts
            then pure (Right [])
            else Zabbix.hostsGroupsGet source.baseUrl token unknownHosts
    case fetched of
        Left err -> pure (Left err)
        Right pairs -> do
            let resolved = Map.fromList [(host, fromMaybe [] (lookup host pairs)) | host <- unknownHosts]
                hostGroups = Map.union resolved (paGroups acc)
                (groupedEvents, ungroupedHosts) = partitionUngrouped hostGroups page
            ingestEvents source (map (normalizeWithGroups source hostGroups) groupedEvents)
            pure
                ( Right
                    acc
                        { paCount = paCount acc + length page
                        , paMaxClock = max (paMaxClock acc) (maximumMaybe (map (.clock) page))
                        , paGroups = hostGroups
                        , paUngrouped = Set.union (paUngrouped acc) (Set.fromList ungroupedHosts)
                        }
                )

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
                rows <-
                    query @ZabbixHostGroup
                        |> filterWhere (#sourceId, get #id source)
                        |> filterWhereIn (#name, names)
                        |> fetch
                pure case rows of
                    [] -> Right Nothing
                    _ -> Right (Just (map (.groupId) rows))

maximumMaybe :: (Ord a) => [a] -> Maybe a
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
-- Zabbix suppress/unsuppress (ack action bits 32/64) mirrors into the
-- suppressed overlay as suppressed_by = 'source'; the newest of the two
-- actions wins.
reconcileAcks :: (?modelContext :: ModelContext) => Source -> Text -> IO ()
reconcileAcks source token = do
    -- Stalled alerts are included: a stalled alert can still be
    -- suppressed/unsuppressed on the zabbix side, and the mirror is the only
    -- path that learns about it (the event cursor already passed). The cost
    -- is a few more ids in the single batched event.get, no extra calls.
    alerts <-
        query @Alert
            |> filterWhere (#sourceId, Just (get #id source))
            |> filterWhereIn (#status, ["firing", "ack", "stalled"] :: [Text])
            |> fetch
    let eventIds = mapMaybe (.externalId) alerts
    unless (null eventIds) do
        result <- Zabbix.ackStateGet source.baseUrl token eventIds
        case result of
            Left _err -> pure ()
            Right states -> do
                let userIds = nub [row.ackUserId | state <- states, row <- state.ackRows, row.ackUserId /= ""]
                usersResult <-
                    if null userIds
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
                latestSuppress = lastMaybe [row | row <- rows, row.ackAction .&. 32 /= 0]
                latestUnsuppress = lastMaybe [row | row <- rows, row.ackAction .&. 64 /= 0]
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
            case (latestSuppress, latestUnsuppress) of
                (Just sup, Just unsup)
                    | unsup.ackClock > sup.ackClock -> mirrorUnsup alert unsup
                    | otherwise -> mirrorSup alert sup
                (Just sup, Nothing) -> mirrorSup alert sup
                (Nothing, Just unsup) -> mirrorUnsup alert unsup
                (Nothing, Nothing) -> pure ()
  where
    mirrorSup alert row = do
        let sourceAt = posixSecondsToUTCTime (fromIntegral row.ackClock)
        void (mirrorExternalSuppress alert "zabbix" (actorOf' row) sourceAt)
    mirrorUnsup alert row = do
        let sourceAt = posixSecondsToUTCTime (fromIntegral row.ackClock)
        void (mirrorExternalUnsuppress alert "zabbix" (actorOf' row) sourceAt)
    actorOf' row = fromMaybe row.ackUserId (lookup row.ackUserId userNames)

lastMaybe :: [a] -> Maybe a
lastMaybe = last

-- Resolved-state reconciliation: cursor-based event.get only returns NEW
-- events, so an OK event missed during an outage (truncated catch-up page,
-- housekeeper-purged history) leaves the local alert firing forever. Each
-- due cycle re-fetches the CURRENT value of every trigger behind a tracked
-- (firing/ack) alert via ONE batched trigger.get and locally resolves
-- whatever zabbix no longer reports as a problem. Keyed on the trigger id
-- from the fingerprint, not alerts.external_id: refires don't rotate
-- external_id, so the stored event id can point at an already-resolved older
-- problem. The resolve is applied to the tracked row BY ID via
-- transitionAlert — rediscovering it by fingerprint would let a duplicate
-- non-closed row eat the transition as an illegal no-op (endless loop).
--
-- Source config keys (all optional, defaults in the accessors below):
--   reconcileResolved           bool  master switch (default true)
--   reconcileGraceSeconds       int   min age of last local activity before
--                                     trusting source state (default 60)
--   reconcileIntervalSeconds    int   min seconds between reconciles,
--                                     0 = every poll cycle (default 0)
--   absentResolveMinAgeSeconds  int   min alert age before a trigger MISSING
--                                     from trigger.get (deleted, or invisible
--                                     to the token) is treated as resolved
--                                     (default 86400)
--   eventPageLimit              int   event.get page size (default 1000)
--   scanMissingProblems         bool  master switch for the missing-problem
--                                     scan below (default true)
--   scanWindowSeconds           int   how far back lastchange may be for an
--                                     untracked problem to be scanned (default
--                                     86400)
reconcileProblemStates :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Source -> Text -> UTCTime -> IO ()
reconcileProblemStates source token now = do
    alerts <-
        query @Alert
            |> filterWhere (#sourceId, Just (get #id source))
            |> filterWhereIn (#status, ["firing", "ack", "stalled"] :: [Text])
            |> fetch
    let tracked = [(triggerId, alert) | alert <- alerts, Just triggerId <- [triggerIdOf alert]]
    unless (null tracked) do
        result <- Zabbix.triggerStateGet source.baseUrl token (nub (map fst tracked))
        case result of
            Left err -> do
                logWarn ("zabbix source \"" <> source.name <> "\" trigger-state reconcile failed: " <> err)
                recordReconcileFailure source err
            Right states -> do
                recordReconcileSuccess source
                let stateByTrigger = Map.fromList (map (\state -> (state.triggerStateId, state)) states)
                forM_ tracked \(triggerId, alert) ->
                    case resolveDecision now source alert (Map.lookup triggerId stateByTrigger) of
                        Nothing -> pure ()
                        Just (resolvedAt, disabled) -> do
                            updated <-
                                if disabled
                                    then resolveDisabledOnZabbix alert resolvedAt
                                    else resolveFromProblem alert resolvedAt
                            when (updated.status == "resolved") do
                                let alertId = get #id alert
                                logInfo ("zabbix source \"" <> source.name <> "\": resolved alert " <> tshow alertId <> " (trigger " <> triggerId <> (if disabled then " disabled" :: Text else " not in problem state") <> ")")

-- Missing-problem scan: cursor-based event.get only sees NEW events, so a
-- trigger that went to problem before the cursor (standing problem at attach
-- time, older than initialHistoryDays on the first poll, or skipped by the
-- same-second page-truncation guard) never produces a local alert even though
-- trigger.get reports it in problem state. Each due reconcile cycle, fetch
-- triggers currently in problem state (restricted to the source's host group
-- scope), keep those whose lastchange is inside the scan window AND have no
-- tracked (firing/ack/stalled) local alert, then fetch their events in one
-- batched event.get (objectids) and ingest them through the normal pipeline.
-- The event cursor is untouched; overlap with the cursor path dedupes by
-- fingerprint in ingest, and an OK event we also missed is healed by the
-- resolved-state reconcile on a later cycle. Triggers with only
-- resolved/closed local rows are rescanned on purpose: a refire we never saw
-- (skipped tail) looks exactly like that.
scanMissingProblems :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Source -> Text -> [Text] -> UTCTime -> IO ()
scanMissingProblems source token groupIds now = do
    result <- Zabbix.problemTriggersGet source.baseUrl token groupIds
    case result of
        Left err -> do
            logWarn ("zabbix source \"" <> source.name <> "\" missing-problem scan failed: " <> err)
            recordReconcileFailure source err
        Right triggers -> do
            alerts <-
                query @Alert
                    |> filterWhere (#sourceId, Just (get #id source))
                    |> filterWhereIn (#status, ["firing", "ack", "stalled"] :: [Text])
                    |> fetch
            let tracked = Set.fromList (map (.fingerprint) alerts)
                candidates = missingProblemCandidates now (scanWindowSeconds source) tracked triggers
            unless (null candidates) do
                let triggerIds = map (.triggerStateId) candidates
                    windowStart = floor (utcTimeToPOSIXSeconds (addUTCTime (negate (fromIntegral (scanWindowSeconds source))) now))
                eventsResult <- Zabbix.eventsByTriggersFold source.baseUrl token windowStart triggerIds groupIds (eventPageLimit source) (ingestPage source token) emptyPageAcc
                case eventsResult of
                    Left err ->
                        logWarn ("zabbix source \"" <> source.name <> "\" missed-problem event fetch failed: " <> err)
                    Right acc -> do
                        recordReconcileSuccess source
                        logInfo ("zabbix source \"" <> source.name <> "\": ingesting " <> tshow (paCount acc) <> " missed events for " <> tshow (length candidates) <> " untracked problem trigger(s)")
                        -- Unlike the cursor path there is nothing to retry
                        -- (the cursor doesn't move), but the trigger stays in
                        -- problem state so the next due cycle rescans it.
                        syncUngroupedHostAlerts source (Set.toList (paUngrouped acc)) [host | (host, groups) <- Map.toList (paGroups acc), not (null groups)]

-- | Problem-state triggers worth fetching events for: lastchange inside the
-- window and no tracked local alert with the trigger's fingerprint.
missingProblemCandidates :: UTCTime -> Int -> Set.Set Text -> [Zabbix.ZabbixTriggerState] -> [Zabbix.ZabbixTriggerState]
missingProblemCandidates now windowSeconds tracked triggers =
    [ trigger
    | trigger <- triggers
    , trigger.triggerStateLastChange >= windowStart
    , not (Set.member ("zabbix:trigger:" <> trigger.triggerStateId) tracked)
    ]
  where
    windowStart = floor (utcTimeToPOSIXSeconds (addUTCTime (negate (fromIntegral windowSeconds)) now))

scanMissingProblemsEnabled :: Source -> Bool
scanMissingProblemsEnabled = configBool True "scanMissingProblems"

scanWindowSeconds :: Source -> Int
scanWindowSeconds = configInt 86400 "scanWindowSeconds"

-- | Split events by the host group map (one batched host.get per cycle):
-- events whose host positively has groups are ingested (with the groups
-- attached); events from a host with NO groups — or missing from host.get,
-- e.g. invisible to the token — are DROPPED from ingest entirely and the host
-- is reported (Sergey 2026-10-01: such hosts get no alerts, no
-- notifications, no processing at all). Hostless events (template/calculated
-- triggers) pass through with no groups, which makes them invisible under
-- per-team visibility.
partitionUngrouped :: Map Text [Text] -> [Zabbix.ZabbixEvent] -> ([Zabbix.ZabbixEvent], [Text])
partitionUngrouped hostGroups = foldr step ([], [])
  where
    step event (grouped, ungrouped) = case event.host of
        Nothing -> (event : grouped, ungrouped)
        Just host -> case Map.lookup host hostGroups of
            Just groups | not (null groups) -> (event : grouped, ungrouped)
            _ -> (grouped, host : ungrouped)

normalizeWithGroups :: Source -> Map Text [Text] -> Zabbix.ZabbixEvent -> Application.Helper.Ingest.NormalizedEvent
normalizeWithGroups source hostGroups event =
    Zabbix.toNormalizedEvent source.baseUrl source.env (Map.findWithDefault [] (fromMaybe "" event.host) hostGroups) event

-- | One info-severity Halemans alert per ungrouped host (raised via the
-- normal ingest pipeline, so dedupe/occurrences/grouping all behave), and
-- resolved as soon as a poll sees the same host WITH groups. The "halemans:"
-- fingerprint prefix keeps these visible to every user regardless of host
-- group scope.
ungroupedHostFingerprint :: Text -> Text
ungroupedHostFingerprint host = "halemans:ungrouped-host:" <> host

syncUngroupedHostAlerts :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Source -> [Text] -> [Text] -> IO ()
syncUngroupedHostAlerts source ungroupedHosts groupedHosts = do
    now <- getCurrentTime
    ingestEvents source [ungroupedEvent now host | host <- ungroupedHosts]
    unless (null groupedHosts) do
        rows <-
            query @Alert
                |> filterWhere (#sourceId, Just (get #id source))
                |> filterWhereIn (#status, ["firing", "ack", "stalled"] :: [Text])
                |> fetch
        blackouts <- fetchActiveBlackouts now
        forM_ rows \alert -> case alert.host of
            Just host
                | host `elem` groupedHosts
                , isJust (Text.stripPrefix "halemans:ungrouped-host:" alert.fingerprint) -> do
                    let suppressedNow = any (blackoutApplies now (alertSubject alert)) blackouts
                    void (transitionAlert now Resolved (if Text.null source.env then Nothing else Just source.env) alert.environmentId alert.hostId alert.serviceId suppressedNow alert)
            _ -> pure ()

ungroupedEvent :: UTCTime -> Text -> NormalizedEvent
ungroupedEvent now host =
    NormalizedEvent
        { fingerprint = ungroupedHostFingerprint host
        , externalId = Nothing
        , status = Firing
        , severity = "info"
        , title = "Zabbix host has no host groups: " <> host
        , description = "Halemans dropped alerts from this host: it has no zabbix host groups (or is invisible to the source token), and hosts without groups are not processed. Add the host to a zabbix host group; this alert resolves itself on the next poll cycle once the host has groups."
        , env = Nothing
        , host = Just host
        , service = Nothing
        , checkName = Nothing
        , labels = Aeson.object ["source" Aeson..= ("halemans" :: Text)]
        , annotations = Aeson.object []
        , hostGroups = []
        , startedAt = Just now
        , sourceUrl = Nothing
        }

-- | Resolve the tracked row itself through the normal transition path (state
-- machine, audit events, notifications, WS fan-out), then back-date
-- resolved_at to the zabbix-side state-change time when known.
resolveFromProblem :: (?modelContext :: ModelContext) => Alert -> UTCTime -> IO Alert
resolveFromProblem alert resolvedAt = do
    now <- getCurrentTime
    blackouts <- fetchActiveBlackouts now
    let suppressedNow = any (blackoutApplies now (alertSubject alert)) blackouts
    updated <- transitionAlert now Resolved alert.env alert.environmentId alert.hostId alert.serviceId suppressedNow alert
    when (updated.status == "resolved") do
        let alertId = get #id alert
        void
            ( sqlExecTyped
                [typedSql|
            UPDATE alerts SET resolved_at = ${resolvedAt}
            WHERE id = ${alertId} AND status = 'resolved' AND resolved_at > ${resolvedAt}
        |]
            )
    pure updated

-- | Resolve an alert whose trigger was DISABLED on the zabbix side, with an
-- audit comment. Disabled triggers keep their last value (often "1" =
-- problem), so value-based checks alone would leave the alert firing (and
-- the stalled reconcile would refire it forever).
resolveDisabledOnZabbix :: (?modelContext :: ModelContext) => Alert -> UTCTime -> IO Alert
resolveDisabledOnZabbix alert resolvedAt = do
    updated <- resolveFromProblem alert resolvedAt
    when (updated.status == "resolved") do
        user <- zabbixServiceUser
        void (addComment user updated "disabled on zabbix side")
    pure updated

zabbixServiceUser :: (?modelContext :: ModelContext) => IO User
zabbixServiceUser = do
    existing <- query @User |> filterWhere (#email, "zabbix@localhost") |> fetchOneOrNothing
    case existing of
        Just user -> pure user
        Nothing ->
            newRecord @User
                |> set #email "zabbix@localhost"
                |> set #displayName "Zabbix"
                |> set #passwordHash "!"
                |> createRecord

-- | Just (resolvedAt, wasDisabled) when the alert should be locally
-- resolved; Nothing when the trigger is still in problem state or local
-- activity is too fresh to trust source state (grace window covers the
-- ingest/reconcile race). A DISABLED trigger resolves even when its last
-- value was "1" — zabbix stops evaluating it, so no recovery will ever
-- arrive. zabbix exposes no disable timestamp, so resolvedAt = now.
resolveDecision :: UTCTime -> Source -> Alert -> Maybe Zabbix.ZabbixTriggerState -> Maybe (UTCTime, Bool)
resolveDecision now source alert mState
    | alert.lastSeenAt >= graceCutoff = Nothing
    | otherwise = case mState of
        Just state
            | state.triggerStateStatus == "1" -> Just (now, True)
            | state.triggerStateValue == "1" -> Nothing
            | state.triggerStateLastChange > 0 -> Just (min now (posixSecondsToUTCTime (fromIntegral state.triggerStateLastChange)), False)
            | otherwise -> Just (now, False)
        Nothing
            | addUTCTime (negate absentMinAge) now >= fromMaybe now alert.startedAt -> Just (now, False)
            | otherwise -> Nothing
  where
    graceCutoff = addUTCTime (negate (fromIntegral (reconcileGraceSeconds source))) now
    absentMinAge = fromIntegral (absentResolveMinAgeSeconds source)

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

configVal :: (Aeson.FromJSON a) => a -> Text -> Source -> a
configVal def key source = fromMaybe def (parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: Key.fromText key)) source.config)

configInt :: Int -> Text -> Source -> Int
configInt = configVal

configBool :: Bool -> Text -> Source -> Bool
configBool = configVal
