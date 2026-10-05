module Application.Helper.Ingest (
    NormalizedEvent (..),
    SourceStatus (..),
    IngestError (..),
    ingestEvents,
    ingest,
    transitionAlert,
    fetchActiveBlackouts,
    reevaluateBlackoutOverlay,
    publishAlertUpdate,
) where

import Application.Job.Mattermost (enqueueSyncIfPosted)
import Application.Pipeline.Blackouts (alertSubject, blackoutApplies)
import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)
import Application.Pipeline.StateMachine (AlertState, Transition (..), Trigger (..))
import qualified Application.Pipeline.StateMachine as SM
import Application.Service.AlertScope (hostGroupNamesOf)
import Application.Service.Escalation (cancelTrackersFor)
import qualified Application.Service.Facets as Facets
import Application.Service.Groups (assignGroup, recomputeGroupRollup)
import Application.Service.I18n (defaultLanguage, languageCode)
import qualified Application.Service.Llm.AutoAnalyze as AutoAnalyze
import Application.Service.Notify (dispatchNotification)
import Control.Monad (void)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as Text
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)

data SourceStatus = Firing | Resolved deriving (Eq, Show)

-- Minimal normalized form (milestone 0). Phase 1 extends this per
-- design_docs/01_highlevel.md §4.3.
data NormalizedEvent = NormalizedEvent
    { fingerprint :: Text
    , externalId :: Maybe Text
    , status :: SourceStatus
    , severity :: Text
    , title :: Text
    , description :: Text
    , env :: Maybe Text
    , host :: Maybe Text
    , service :: Maybe Text
    , checkName :: Maybe Text
    , labels :: Value
    , annotations :: Value
    , hostGroups :: [Text]
    -- ^ Zabbix host group names for the event's host (empty for non-zabbix
    -- sources); drives per-team alert visibility.
    , startedAt :: Maybe UTCTime
    , sourceUrl :: Maybe Text
    }

ingestEvents :: (?modelContext :: ModelContext) => Source -> [NormalizedEvent] -> IO ()
ingestEvents source = mapM_ (void . ingest source)

-- | Pipeline per design_docs/milestone_1.md §3. RawEvent persistence happens
-- in the connector before this is called.
ingest :: (?modelContext :: ModelContext) => Source -> NormalizedEvent -> IO (Maybe (Id Alert))
ingest source event = do
    now <- getCurrentTime
    environmentRef <- forM event.env upsertEnvironment
    hostRef <- forM event.host \fqdn -> upsertHost fqdn environmentRef
    serviceRef <- forM event.service \name -> upsertService name environmentRef

    existing <-
        query @Alert
            |> filterWhere (#fingerprint, event.fingerprint)
            |> filterWhereNot (#status, "closed" :: Text)
            |> fetchOneOrNothing

    case (existing, event.status) of
        (Nothing, Resolved) -> pure Nothing
        (Nothing, Firing) -> do
            let built =
                    newRecord @Alert
                        |> set #fingerprint event.fingerprint
                        |> set #sourceId (Just (get #id source))
                        |> set #externalId event.externalId
                        |> set #title event.title
                        |> set #description event.description
                        |> set #severity event.severity
                        |> set #status "firing"
                        |> set #env event.env
                        |> set #host event.host
                        |> set #service event.service
                        |> set #checkName event.checkName
                        |> set #labels event.labels
                        |> set #annotations event.annotations
                        |> set #hostGroups (Aeson.toJSON event.hostGroups)
                        |> set #sourceUrl event.sourceUrl
                        |> set #startedAt event.startedAt
                        |> set #environmentId environmentRef
                        |> set #hostId hostRef
                        |> set #serviceId serviceRef
            -- Facet materialization at ingest (milestone_9.md §3): field/label
            -- mappings resolve immediately; attr facets land via EnrichAlertJob.
            -- Blackouts scope by the EFFECTIVE values (facet override wins over
            -- the raw ingest name), so the coverage check runs only after the
            -- field/label facets are resolved; attr overrides are re-evaluated
            -- after enrichment (reevaluateBlackoutOverlay), before Expose.
            facets <- Facets.computeFacetsValue [] built
            let enriched = built |> set #facets facets
            blackouts <- fetchActiveBlackouts now
            let suppressedNow = any (blackoutApplies now (alertSubject enriched)) blackouts
            alert <-
                enriched
                    |> set #suppressed suppressedNow
                    |> set #suppressedBy (if suppressedNow then Just "blackout" else Nothing)
                    |> createRecord
            recordEvent (get #id alert) "created" (object ["source" .= get #name source])
            when suppressedNow do
                recordEvent (get #id alert) "suppressed" (object ["note" .= ("covered by active blackout" :: Text)])
            -- Step 5 grouping (milestone_2.md §3): first matching rule wins;
            -- no match leaves the alert standalone.
            grouped <- assignGroup alert
            -- Staged pipeline: the Expose stage (notification dispatch + the
            -- "created" WS fan-out) is DEFERRED until the Enrich stage
            -- completes (EnrichAlertJob triggers it) so every channel
            -- renders the fully resolved alert. The ExposeAlertJob below is
            -- the deadline: if enrichment hard-fails, exposition still runs
            -- — an alert is never lost. See Application.Service.Expose.
            void do
                newRecord @EnrichAlertJob
                    |> set #alertId (get #id grouped)
                    |> createRecord
            void do
                newRecord @ExposeAlertJob
                    |> set #alertId (get #id grouped)
                    |> set #runAt (addUTCTime exposeDeadlineSeconds now)
                    |> createRecord
            -- Step 8 extension (milestone_4.md §4): queue an LLM analysis on
            -- new alerts only (never on dedupe hits), gated by the
            -- auto-analysis rules (milestone 10 §5: status + severity). The
            -- row gets its prompt hash when the job builds the prompt.
            autoAnalyze <- AutoAnalyze.autoAnalyzeAllowed grouped
            when autoAnalyze do
                systemLanguage <- defaultLanguage
                void do
                    analysis <-
                        newRecord @LlmAnalysis
                            |> set #alertId (get #id grouped)
                            |> set #language (Just (languageCode systemLanguage))
                            |> createRecord
                    void do
                        newRecord @LlmAnalysisJob
                            |> set #analysisId (get #id analysis)
                            |> createRecord
            pure (Just (get #id grouped))
        (Just alert, sourceStatus) -> do
            -- The source's current text is the truth: a refired alert follows
            -- title changes (e.g. grafana rule rename); an empty event
            -- description keeps whatever we have (older sources send none).
            let refreshed =
                    alert
                        |> set #title event.title
                        |> (if Text.null event.description then (\x -> x) else set #description event.description)
                        |> set #hostGroups (Aeson.toJSON (nub (event.hostGroups ++ hostGroupNamesOf alert)))
            -- Coverage from the row's EFFECTIVE names (facets win over the raw
            -- ingest names) — see alertSubject.
            blackouts <- fetchActiveBlackouts now
            let suppressedNow = any (blackoutApplies now (alertSubject refreshed)) blackouts
            updated <- transitionAlert now sourceStatus event.env environmentRef hostRef serviceRef suppressedNow refreshed
            pure (Just (get #id updated))

-- | State-machine transition + side effects (escalation cancel, notification,
-- WS fan-out) for a source status applied to a KNOWN alert row. Split from
-- ingest so reconcile paths that already hold the row don't rediscover it by
-- fingerprint: a duplicate non-closed row with the same fingerprint would
-- otherwise eat the transition as an illegal no-op.
transitionAlert ::
    (?modelContext :: ModelContext) =>
    UTCTime ->
    SourceStatus ->
    Maybe Text ->
    Maybe (Id Environment) ->
    Maybe (Id Host) ->
    Maybe (Id Service) ->
    Bool ->
    Alert ->
    IO Alert
transitionAlert now sourceStatus eventEnv environmentRef hostRef serviceRef suppressedNow alert = do
    let currentState = fromMaybe SM.Firing (SM.alertStateFromText alert.status)
        trigger = case sourceStatus of
            Firing -> Refire
            Resolved -> SourceResolved
        transition = SM.step currentState trigger
    updated <- applyTransition now eventEnv environmentRef hostRef serviceRef suppressedNow alert transition
    when (transition.applied && transition.to == SM.Resolved) do
        cancelTrackersFor (get #id alert)
        unless suppressedNow do
            void (dispatchNotification updated)
    publishAlertUpdate updated transition.eventKind
    pure updated

-- | Apply a state-machine transition to the alert row and append the audit
-- event. Illegal transitions are no-ops with an AlertEvent note (§4).
applyTransition ::
    (?modelContext :: ModelContext) =>
    UTCTime ->
    Maybe Text ->
    Maybe (Id Environment) ->
    Maybe (Id Host) ->
    Maybe (Id Service) ->
    Bool ->
    Alert ->
    Transition ->
    IO Alert
applyTransition now eventEnv environmentRef hostRef serviceRef suppressedNow alert transition = do
    -- Source-muted alerts (suppressed_by = 'source') are owned by the
    -- source's suppress/unsuppress actions: the blackout overlay recomputed
    -- on every event must neither clear nor re-flag them.
    let ownedBySource = alert.suppressedBy == Just "source"
        effectiveSuppressed = ownedBySource || suppressedNow
        effectiveSuppressedBy
            | ownedBySource = alert.suppressedBy
            | suppressedNow = Just "blackout"
            | otherwise = Nothing
        base =
            alert
                |> set #lastSeenAt now
                |> set #env (eventEnv <|> alert.env)
                |> set #environmentId (environmentRef <|> alert.environmentId)
                |> set #hostId (hostRef <|> alert.hostId)
                |> set #serviceId (serviceRef <|> alert.serviceId)
                |> set #suppressed effectiveSuppressed
                |> set #suppressedBy effectiveSuppressedBy
                |> set #updatedAt now
    let transitioned = case (transition.applied, transition.trigger) of
            (True, Refire) ->
                base
                    |> set #occurrences (alert.occurrences + 1)
                    |> set #status (SM.alertStateToText transition.to)
                    |> set #resolvedAt (if transition.to == SM.Firing then Nothing else alert.resolvedAt)
            (True, SourceResolved) ->
                base
                    |> set #status "resolved"
                    |> set #resolvedAt (Just now)
            (True, _) ->
                base
                    |> set #status (SM.alertStateToText transition.to)
            (False, Refire) ->
                base
                    |> set #occurrences (alert.occurrences + 1)
            (False, _) -> base
    updated <- updateRecord transitioned
    forM_ alert.groupId (void . recomputeGroupRollup)
    let payload =
            if transition.applied
                then
                    object
                        [ "from" .= SM.alertStateToText transition.from
                        , "to" .= SM.alertStateToText transition.to
                        ]
                else
                    object
                        [ "note" .= ("illegal transition ignored" :: Text)
                        , "state" .= SM.alertStateToText transition.from
                        , "trigger" .= show transition.trigger
                        ]
    recordEvent (get #id alert) transition.eventKind payload
    when (effectiveSuppressed && not alert.suppressed && not ownedBySource) do
        recordEvent (get #id alert) "suppressed" (object ["note" .= ("covered by active blackout" :: Text)])
    when (alert.suppressed && not effectiveSuppressed) do
        recordEvent (get #id alert) "unsuppressed" (object ["note" .= ("blackout expired or removed" :: Text)])
    pure updated

-- | Re-evaluate the blackout overlay after enrichment materialized attr
-- facets: the effective env/host/service may now differ from the raw
-- ingest names (field/asset mappings), so a blackout scoped by an
-- effective value only starts matching here. Runs BEFORE the Expose stage
-- so notification dispatch sees the final overlay. Source-owned
-- suppression (suppressed_by = 'source') is never touched. Publishes the
-- flip so dashboards refresh.
reevaluateBlackoutOverlay :: (?modelContext :: ModelContext) => Alert -> IO Alert
reevaluateBlackoutOverlay alert
    | alert.suppressedBy == Just "source" = pure alert
    | otherwise = do
        now <- getCurrentTime
        blackouts <- fetchActiveBlackouts now
        let covered = any (blackoutApplies now (alertSubject alert)) blackouts
            wasCovered = alert.suppressed && alert.suppressedBy == Just "blackout"
        if covered == wasCovered
            then pure alert
            else do
                let kind = if covered then "suppressed" else "unsuppressed"
                updated <-
                    alert
                        |> set #suppressed covered
                        |> set #suppressedBy (if covered then Just "blackout" else Nothing)
                        |> set #updatedAt now
                        |> updateRecord
                recordEvent (get #id alert) kind (object ["note" .= ("blackout coverage re-evaluated after enrichment" :: Text)])
                publishAlertUpdate updated kind
                pure updated

recordEvent :: (?modelContext :: ModelContext) => Id Alert -> Text -> Value -> IO ()
recordEvent alertId kind payload = do
    _ <-
        newRecord @AlertEvent
            |> set #alertId alertId
            |> set #userId Nothing
            |> set #kind kind
            |> set #payload payload
            |> createRecord
    pure ()

-- | Ingestion failures (milestone 12 §8): thrown as a typed exception
-- instead of 'error' so callers can match on them.
data IngestError = EnvironmentUpsertFailed Text
    deriving (Show)

instance Exception IngestError

-- | Subject resolution (milestone_1.md §3 step 3): upsert inventory rows by
-- name; unknown host/service become auto_created stubs.
upsertEnvironment :: (?modelContext :: ModelContext) => Text -> IO (Id Environment)
upsertEnvironment name = do
    rows <-
        sqlQueryTyped
            [typedSql|
        INSERT INTO environments (name) VALUES (${name})
        ON CONFLICT (name) DO UPDATE SET name = EXCLUDED.name
        RETURNING id
    |]
    case rows of
        (rawId : _) -> pure rawId
        [] -> throwIO (EnvironmentUpsertFailed name)

upsertHost :: (?modelContext :: ModelContext) => Text -> Maybe (Id Environment) -> IO (Id Host)
upsertHost fqdn environmentRef = do
    existing <-
        query @Host
            |> filterWhere (#fqdn, fqdn)
            |> fetchOneOrNothing
    case existing of
        Just host -> pure (get #id host)
        Nothing -> do
            host <-
                newRecord @Host
                    |> set #fqdn fqdn
                    |> set #environmentId environmentRef
                    |> set #autoCreated True
                    |> createRecord
            pure (get #id host)

upsertService :: (?modelContext :: ModelContext) => Text -> Maybe (Id Environment) -> IO (Id Service)
upsertService name environmentRef = do
    existing <-
        query @Service
            |> filterWhere (#name, name)
            |> fetchOneOrNothing
    case existing of
        Just service -> pure (get #id service)
        Nothing -> do
            service <-
                newRecord @Service
                    |> set #name name
                    |> set #environmentId environmentRef
                    |> set #autoCreated True
                    |> createRecord
            pure (get #id service)

fetchActiveBlackouts :: (?modelContext :: ModelContext) => UTCTime -> IO [Blackout]
fetchActiveBlackouts now =
    query @Blackout
        |> filterWhereSql (#startsAt, "<= NOW()")
        |> filterWhereSql (#endsAt, "> NOW()")
        |> fetch

-- | Seconds after ingest at which the Expose-stage deadline job fires
-- regardless of enrichment state (Application.Service.Expose).
exposeDeadlineSeconds :: NominalDiffTime
exposeDeadlineSeconds = 120

-- | Websocket fan-out (milestone_1.md §7): the web process LISTENs on
-- halemans_events and re-renders fragments for connected clients.
publishAlertUpdate :: (?modelContext :: ModelContext) => Alert -> Text -> IO ()
publishAlertUpdate alert kind = do
    let payload :: Text
        payload =
            cs
                ( Aeson.encode
                    ( object
                        [ "alertId" .= get #id alert
                        , "env" .= effectiveFieldText FieldEnv alert
                        , "host" .= effectiveFieldText FieldHost alert
                        , "kind" .= kind
                        , "title" .= alert.title
                        , "severity" .= alert.severity
                        , "status" .= alert.status
                        ]
                    )
                )
    -- pg_notify returns void, which typedSql cannot decode; the IS NULL
    -- predicate on void is always true, so this yields a plain int row.
    _ <- sqlQueryTyped [typedSql| SELECT 1 WHERE pg_notify('halemans_events', ${payload}) IS NULL |] :: IO [Int]
    -- Mattermost root-post sync rides the same fan-out: state-changing kinds
    -- patch the alert's root posts (the service no-ops when unconfigured).
    enqueueSyncIfPosted alert kind
    pure ()
