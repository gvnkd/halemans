module Application.Pipeline.Actions
( ackAlert
, unackAlert
, closeAlert
, stallAlert
, autoCloseAlert
, addComment
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import Generated.Types
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Application.Pipeline.StateMachine (AlertState, Trigger (..))
import qualified Application.Pipeline.StateMachine as SM
import Application.Helper.Ingest (publishAlertUpdate)
import Application.Service.Escalation (cancelTrackersFor, restartTrackersFor)
import Application.Service.Groups (recomputeGroupRollup)
import qualified Application.Service.WriteBack as WriteBack
import Control.Monad (void)

-- User-initiated alert actions (milestone_1.md §4/§6). Every attempt writes
-- an AlertEvent; illegal transitions are no-ops with a note.

ackAlert :: (?modelContext :: ModelContext) => User -> Alert -> Maybe Text -> Maybe Int -> IO Alert
ackAlert user alert comment timeoutMinutes = do
    now <- getCurrentTime
    let transition = SM.step (currentState alert) AckTrigger
    if not transition.applied
        then do
            illegalNote user alert transition
            pure alert
        else do
            let ackExpiresAt = (\minutes -> addUTCTime (fromIntegral minutes * 60) now) <$> timeoutMinutes
            updated <- alert
                |> set #status "ack"
                |> set #acknowledgedBy (Just (get #id user))
                |> set #acknowledgedAt (Just now)
                |> set #ackComment comment
                |> set #ackExpiresAt ackExpiresAt
                |> set #updatedAt now
                |> updateRecord
            recordUserEvent user alert "ack" (object
                [ "comment" .= comment
                , "expiresAt" .= ackExpiresAt
                ])
            cancelTrackersFor (get #id alert)
            forM_ alert.groupId (void . recomputeGroupRollup)
            publishAlertUpdate updated "ack"
            WriteBack.enqueueForAction updated "ack"
            pure updated

unackAlert :: (?modelContext :: ModelContext) => Maybe User -> Alert -> Text -> IO Alert
unackAlert actor alert note = do
    now <- getCurrentTime
    let transition = SM.step (currentState alert) Unack
    if not transition.applied
        then do
            case actor of
                Just user -> illegalNote user alert transition
                Nothing -> recordSystemEvent alert "external" (illegalPayload transition)
            pure alert
        else do
            updated <- alert
                |> set #status "firing"
                |> set #ackExpiresAt Nothing
                |> set #updatedAt now
                |> updateRecord
            let payload = object ["note" .= note]
            case actor of
                Just user -> recordUserEvent user alert "unack" payload
                Nothing -> recordSystemEvent alert "unack" payload
            restartTrackersFor (get #id alert)
            forM_ alert.groupId (void . recomputeGroupRollup)
            publishAlertUpdate updated "unack"
            when (isJust actor) do
                WriteBack.enqueueForAction updated "unack"
            pure updated

closeAlert :: (?modelContext :: ModelContext) => Maybe User -> Alert -> Maybe Text -> IO Alert
closeAlert actor alert reason = do
    now <- getCurrentTime
    let transition = SM.step (currentState alert) CloseTrigger
    if not transition.applied
        then do
            case actor of
                Just user -> illegalNote user alert transition
                Nothing -> recordSystemEvent alert "external" (illegalPayload transition)
            pure alert
        else do
            updated <- alert
                |> set #status "closed"
                |> set #closedBy (get #id <$> actor)
                |> set #closedAt (Just now)
                |> set #closeReason reason
                |> set #updatedAt now
                |> updateRecord
            let payload = object ["reason" .= reason]
            case actor of
                Just user -> recordUserEvent user alert "closed" payload
                Nothing -> recordSystemEvent alert "closed" payload
            cancelTrackersFor (get #id alert)
            forM_ alert.groupId (void . recomputeGroupRollup)
            publishAlertUpdate updated "closed"
            when (isJust actor) do
                WriteBack.enqueueForAction updated "close"
            pure updated

-- | Stall an alert that stopped receiving source updates (deleted trigger,
-- dead webhook). System-initiated like the auto-close path below.
stallAlert :: (?modelContext :: ModelContext) => Alert -> Text -> IO Alert
stallAlert alert note = do
    now <- getCurrentTime
    let transition = SM.step (currentState alert) StallTimeout
    if not transition.applied
        then do
            recordSystemEvent alert "external" (illegalPayload transition)
            pure alert
        else do
            updated <- alert
                |> set #status "stalled"
                |> set #updatedAt now
                |> updateRecord
            recordSystemEvent alert "stalled" (object
                [ "from" .= SM.alertStateToText transition.from
                , "note" .= note
                ])
            cancelTrackersFor (get #id alert)
            forM_ alert.groupId (void . recomputeGroupRollup)
            publishAlertUpdate updated "stalled"
            pure updated

-- | TTL-driven close (resolved TTL, stalled TTL). Steps with AutoClose —
-- closeAlert uses CloseTrigger, which is illegal from resolved/stalled.
autoCloseAlert :: (?modelContext :: ModelContext) => Alert -> Text -> IO Alert
autoCloseAlert alert reason = do
    now <- getCurrentTime
    let transition = SM.step (currentState alert) AutoClose
    if not transition.applied
        then do
            recordSystemEvent alert "external" (illegalPayload transition)
            pure alert
        else do
            updated <- alert
                |> set #status "closed"
                |> set #closedBy Nothing
                |> set #closedAt (Just now)
                |> set #closeReason (Just reason)
                |> set #updatedAt now
                |> updateRecord
            recordSystemEvent alert "closed" (object ["reason" .= Just reason])
            cancelTrackersFor (get #id alert)
            forM_ alert.groupId (void . recomputeGroupRollup)
            publishAlertUpdate updated "closed"
            pure updated

addComment :: (?modelContext :: ModelContext) => User -> Alert -> Text -> IO Comment
addComment user alert body = do
    comment <- newRecord @Comment
        |> set #alertId (get #id alert)
        |> set #userId (get #id user)
        |> set #body body
        |> createRecord
    recordUserEvent user alert "comment" (object ["body" .= body])
    publishAlertUpdate alert "comment"
    pure comment

currentState :: Alert -> AlertState
currentState alert = fromMaybe SM.Firing (SM.alertStateFromText alert.status)

illegalPayload :: SM.Transition -> Value
illegalPayload transition = object
    [ "note" .= ("illegal transition ignored" :: Text)
    , "state" .= SM.alertStateToText transition.from
    , "trigger" .= show transition.trigger
    ]

illegalNote :: (?modelContext :: ModelContext) => User -> Alert -> SM.Transition -> IO ()
illegalNote user alert transition = recordUserEvent user alert "external" (illegalPayload transition)

recordUserEvent :: (?modelContext :: ModelContext) => User -> Alert -> Text -> Value -> IO ()
recordUserEvent user alert kind payload = do
    _ <- newRecord @AlertEvent
        |> set #alertId (get #id alert)
        |> set #userId (Just (get #id user))
        |> set #kind kind
        |> set #payload payload
        |> createRecord
    pure ()

recordSystemEvent :: (?modelContext :: ModelContext) => Alert -> Text -> Value -> IO ()
recordSystemEvent alert kind payload = do
    _ <- newRecord @AlertEvent
        |> set #alertId (get #id alert)
        |> set #userId Nothing
        |> set #kind kind
        |> set #payload payload
        |> createRecord
    pure ()
