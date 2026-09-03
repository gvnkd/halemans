module Application.Helper.Ingest
( NormalizedEvent (..)
, SourceStatus (..)
, ingestEvents
, ingest
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetchOneOrNothing)
import Generated.Types
import Data.Aeson (Value)

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
ingestEvents source = mapM_ (ingest source)

ingest :: (?modelContext :: ModelContext) => Source -> NormalizedEvent -> IO ()
ingest source event = do
    existing <- query @Alert
        |> filterWhere (#fingerprint, event.fingerprint)
        |> filterWhereNot (#status, "closed" :: Text)
        |> fetchOneOrNothing
    now <- getCurrentTime
    case (existing, event.status) of
        (Just alert, Firing) -> do
            _ <- alert
                |> set #occurrences (alert.occurrences + 1)
                |> set #lastSeenAt now
                |> set #status "firing"
                |> set #resolvedAt Nothing
                |> set #updatedAt now
                |> updateRecord
            pure ()
        (Just alert, Resolved) -> do
            _ <- alert
                |> set #status "resolved"
                |> set #resolvedAt (Just now)
                |> set #lastSeenAt now
                |> set #updatedAt now
                |> updateRecord
            pure ()
        (Nothing, Firing) -> do
            _ <- newRecord @Alert
                |> set #fingerprint event.fingerprint
                |> set #sourceId (Just source.id)
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
                |> createRecord
            pure ()
        (Nothing, Resolved) -> pure ()
