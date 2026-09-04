module Application.Helper.Ingest
( NormalizedEvent (..)
, SourceStatus (..)
, ingestEvents
, ingest
, publishAlertUpdate
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch, fetchOneOrNothing)
import Control.Monad (void)
import IHP.TypedSql (sqlQueryTyped, sqlExecTyped, typedSql)
import Generated.Types
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Application.Pipeline.StateMachine (AlertState, Trigger (..), Transition (..))
import qualified Application.Pipeline.StateMachine as SM
import Application.Pipeline.Blackouts (blackoutApplies)
import Application.Service.Notify (dispatchNotification)
import Application.Service.Groups (assignGroup, recomputeGroupRollup)
import Application.Service.Escalation (cancelTrackersFor)

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

    blackouts <- fetchActiveBlackouts now
    let suppressedNow = any (blackoutApplies now environmentRef hostRef serviceRef) blackouts

    existing <- query @Alert
        |> filterWhere (#fingerprint, event.fingerprint)
        |> filterWhereNot (#status, "closed" :: Text)
        |> fetchOneOrNothing

    case (existing, event.status) of
        (Nothing, Resolved) -> pure Nothing
        (Nothing, Firing) -> do
            alert <- newRecord @Alert
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
                |> set #sourceUrl event.sourceUrl
                |> set #startedAt event.startedAt
                |> set #environmentId environmentRef
                |> set #hostId hostRef
                |> set #serviceId serviceRef
                |> set #suppressed suppressedNow
                |> createRecord
            recordEvent (get #id alert) "created" (object ["source" .= get #name source])
            when suppressedNow do
                recordEvent (get #id alert) "suppressed" (object ["note" .= ("covered by active blackout" :: Text)])
            -- Step 5 grouping (milestone_2.md §3): first matching rule wins;
            -- no match leaves the alert standalone.
            grouped <- assignGroup alert
            unless suppressedNow do
                void (dispatchNotification grouped)
            publishAlertUpdate grouped "created"
            pure (Just (get #id grouped))
        (Just alert, sourceStatus) -> do
            let currentState = fromMaybe SM.Firing (SM.alertStateFromText alert.status)
            let trigger = case sourceStatus of
                    Firing -> Refire
                    Resolved -> SourceResolved
            let transition = SM.step currentState trigger
            updated <- applyTransition now environmentRef hostRef serviceRef suppressedNow alert transition
            when (transition.applied && transition.to == SM.Resolved) do
                cancelTrackersFor (get #id alert)
                unless suppressedNow do
                    void (dispatchNotification updated)
            publishAlertUpdate updated transition.eventKind
            pure (Just (get #id alert))

-- | Apply a state-machine transition to the alert row and append the audit
-- event. Illegal transitions are no-ops with an AlertEvent note (§4).
applyTransition
    :: (?modelContext :: ModelContext)
    => UTCTime
    -> Maybe (Id Environment)
    -> Maybe (Id Host)
    -> Maybe (Id Service)
    -> Bool
    -> Alert
    -> Transition
    -> IO Alert
applyTransition now environmentRef hostRef serviceRef suppressedNow alert transition = do
    let base = alert
            |> set #lastSeenAt now
            |> set #environmentId (environmentRef <|> alert.environmentId)
            |> set #hostId (hostRef <|> alert.hostId)
            |> set #serviceId (serviceRef <|> alert.serviceId)
            |> set #suppressed suppressedNow
            |> set #updatedAt now
    let transitioned = case (transition.applied, transition.trigger) of
            (True, Refire) -> base
                |> set #occurrences (alert.occurrences + 1)
                |> set #status (SM.alertStateToText transition.to)
                |> set #resolvedAt (if transition.to == SM.Firing then Nothing else alert.resolvedAt)
            (True, SourceResolved) -> base
                |> set #status "resolved"
                |> set #resolvedAt (Just now)
            (True, _) -> base
                |> set #status (SM.alertStateToText transition.to)
            (False, Refire) -> base
                |> set #occurrences (alert.occurrences + 1)
            (False, _) -> base
    updated <- updateRecord transitioned
    forM_ alert.groupId (void . recomputeGroupRollup)
    let payload = if transition.applied
            then object
                [ "from" .= SM.alertStateToText transition.from
                , "to" .= SM.alertStateToText transition.to
                ]
            else object
                [ "note" .= ("illegal transition ignored" :: Text)
                , "state" .= SM.alertStateToText transition.from
                , "trigger" .= show transition.trigger
                ]
    recordEvent (get #id alert) transition.eventKind payload
    when (suppressedNow && not alert.suppressed) do
        recordEvent (get #id alert) "suppressed" (object ["note" .= ("covered by active blackout" :: Text)])
    when (alert.suppressed && not suppressedNow) do
        recordEvent (get #id alert) "unsuppressed" (object ["note" .= ("blackout expired or removed" :: Text)])
    pure updated

recordEvent :: (?modelContext :: ModelContext) => Id Alert -> Text -> Value -> IO ()
recordEvent alertId kind payload = do
    _ <- newRecord @AlertEvent
        |> set #alertId alertId
        |> set #userId Nothing
        |> set #kind kind
        |> set #payload payload
        |> createRecord
    pure ()

-- | Subject resolution (milestone_1.md §3 step 3): upsert inventory rows by
-- name; unknown host/service become auto_created stubs.
upsertEnvironment :: (?modelContext :: ModelContext) => Text -> IO (Id Environment)
upsertEnvironment name = do
    rows <- sqlQueryTyped [typedSql|
        INSERT INTO environments (name) VALUES (${name})
        ON CONFLICT (name) DO UPDATE SET name = EXCLUDED.name
        RETURNING id
    |]
    case rows of
        (rawId:_) -> pure rawId
        [] -> error "upsertEnvironment: INSERT RETURNING gave no row"

upsertHost :: (?modelContext :: ModelContext) => Text -> Maybe (Id Environment) -> IO (Id Host)
upsertHost fqdn environmentRef = do
    existing <- query @Host
        |> filterWhere (#fqdn, fqdn)
        |> fetchOneOrNothing
    case existing of
        Just host -> pure (get #id host)
        Nothing -> do
            host <- newRecord @Host
                |> set #fqdn fqdn
                |> set #environmentId environmentRef
                |> set #autoCreated True
                |> createRecord
            pure (get #id host)

upsertService :: (?modelContext :: ModelContext) => Text -> Maybe (Id Environment) -> IO (Id Service)
upsertService name environmentRef = do
    existing <- query @Service
        |> filterWhere (#name, name)
        |> fetchOneOrNothing
    case existing of
        Just service -> pure (get #id service)
        Nothing -> do
            service <- newRecord @Service
                |> set #name name
                |> set #environmentId environmentRef
                |> set #autoCreated True
                |> createRecord
            pure (get #id service)

fetchActiveBlackouts :: (?modelContext :: ModelContext) => UTCTime -> IO [Blackout]
fetchActiveBlackouts now = query @Blackout
    |> filterWhereSql (#startsAt, "<= NOW()")
    |> filterWhereSql (#endsAt, "> NOW()")
    |> fetch

-- | Websocket fan-out (milestone_1.md §7): the web process LISTENs on
-- halemans_events and re-renders fragments for connected clients.
publishAlertUpdate :: (?modelContext :: ModelContext) => Alert -> Text -> IO ()
publishAlertUpdate alert kind = do
    let payload :: Text
        payload = cs (Aeson.encode (object
            [ "alertId" .= get #id alert
            , "env" .= alert.env
            , "kind" .= kind
            , "title" .= alert.title
            , "severity" .= alert.severity
            , "status" .= alert.status
            ]))
    -- pg_notify returns void, which typedSql cannot decode; the IS NULL
    -- predicate on void is always true, so this yields a plain int row.
    _ <- sqlQueryTyped [typedSql| SELECT 1 WHERE pg_notify('halemans_events', ${payload}) IS NULL |] :: IO [Int]
    pure ()
