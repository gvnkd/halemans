module Application.Service.Reconcile (
    shouldMirror,
    mirrorExternalAck,
    mirrorExternalUnack,
    lastAckWasExternal,
) where

import Application.Helper.Ingest (publishAlertUpdate)
import Application.Service.Escalation (cancelTrackersFor, restartTrackersFor)
import Application.Service.Groups (recomputeGroupRollup)
import Control.Monad (void)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Generated.Types
import IHP.Fetch (fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

-- Reverse reconciliation (design_docs/milestone_3.md §6): source-side
-- ack/close mirrors into Halemans. Last-writer-wins by timestamp: source
-- state newer than the last local action mirrors in; older source state
-- never clobbers a newer local action. Both actions land in the history.
-- Mirrors never trigger write-back (only user actions enqueue, see
-- Application.Service.WriteBack), so the two directions cannot loop.

-- | Mirror only when the source action is newer than the last local action
-- (Nothing = no local action yet → always mirror).
shouldMirror :: Maybe UTCTime -> UTCTime -> Bool
shouldMirror lastLocalAt sourceAt = maybe True (< sourceAt) lastLocalAt

mirrorExternalAck :: (?modelContext :: ModelContext) => Alert -> Text -> Text -> UTCTime -> IO Alert
mirrorExternalAck alert source actor sourceAt
    | alert.status /= "firing" = pure alert
    | otherwise = do
        now <- getCurrentTime
        updated <-
            alert
                |> set #status "ack"
                |> set #acknowledgedBy Nothing
                |> set #acknowledgedAt (Just now)
                |> set #updatedAt now
                |> updateRecord
        recordExternal alert "ack" source actor sourceAt
        cancelTrackersFor (get #id alert)
        forM_ alert.groupId (void . recomputeGroupRollup)
        publishAlertUpdate updated "ack"
        pure updated

mirrorExternalUnack :: (?modelContext :: ModelContext) => Alert -> Text -> Text -> UTCTime -> IO Alert
mirrorExternalUnack alert source actor sourceAt
    | alert.status /= "ack" = pure alert
    | otherwise = do
        now <- getCurrentTime
        updated <-
            alert
                |> set #status "firing"
                |> set #ackExpiresAt Nothing
                |> set #updatedAt now
                |> updateRecord
        recordExternal alert "unack" source actor sourceAt
        restartTrackersFor (get #id alert)
        forM_ alert.groupId (void . recomputeGroupRollup)
        publishAlertUpdate updated "unack"
        pure updated

recordExternal :: (?modelContext :: ModelContext) => Alert -> Text -> Text -> Text -> UTCTime -> IO ()
recordExternal alert action source actor sourceAt = void do
    newRecord @AlertEvent
        |> set #alertId (get #id alert)
        |> set #userId Nothing
        |> set #kind "external"
        |> set
            #payload
            ( object
                [ "source" .= source
                , "action" .= action
                , "actor" .= actor
                , "sourceAt" .= sourceAt
                ]
            )
        |> createRecord

-- | True when the alert's most recent ack/unack-relevant event was an
-- external ack mirror (used by silence-expiry revert: only external acks
-- are reverted by the poller).
lastAckWasExternal :: (?modelContext :: ModelContext) => Alert -> IO Bool
lastAckWasExternal alert = do
    latest <-
        query @AlertEvent
            |> filterWhere (#alertId, get #id alert)
            |> filterWhereIn (#kind, ["ack", "unack", "external"])
            |> orderByDesc #createdAt
            |> fetchOneOrNothing
    pure case latest of
        Just event
            | event.kind == "external" ->
                parseMaybe (Aeson.withObject "payload" (\o -> o Aeson..: "action")) event.payload == Just ("ack" :: Text)
        _ -> False
