{-# LANGUAGE OverloadedStrings #-}
module Application.Pipeline.StateMachine
( AlertState (..)
, Trigger (..)
, Transition (..)
, step
, runSequence
, alertStateFromText
, alertStateToText
, isActive
) where

import IHP.Prelude

-- Core alert states per design_docs/01_highlevel.md §5.2.
-- @suppressed@ is intentionally NOT a state: it is an overlay flag on the
-- alert row (design_docs/milestone_1.md §12).
data AlertState = Firing | Acked | Resolved | Closed
    deriving (Eq, Show, Enum, Bounded)

-- External triggers that can move an alert between states.
data Trigger
    = Refire          -- firing event for an existing fingerprint
    | SourceResolved  -- resolved event from the source
    | AckTrigger      -- manual ack
    | Unack           -- manual unack or ack-timeout expiry
    | CloseTrigger    -- manual close
    | AutoClose       -- resolved TTL expired
    deriving (Eq, Show, Enum, Bounded)

data Transition = Transition
    { from :: AlertState
    , to :: AlertState
    , trigger :: Trigger
    -- | AlertEvent.kind to append. Illegal transitions emit @external@ with a
    -- payload note and leave the state unchanged (milestone_1.md §4).
    , eventKind :: Text
    , applied :: Bool
    } deriving (Eq, Show)

alertStateToText :: AlertState -> Text
alertStateToText = \case
    Firing -> "firing"
    Acked -> "ack"
    Resolved -> "resolved"
    Closed -> "closed"

alertStateFromText :: Text -> Maybe AlertState
alertStateFromText = \case
    "firing" -> Just Firing
    "ack" -> Just Acked
    "resolved" -> Just Resolved
    "closed" -> Just Closed
    _ -> Nothing

-- | Active alerts participate in dedupe (fingerprint lookup).
isActive :: AlertState -> Bool
isActive Closed = False
isActive _ = True

step :: AlertState -> Trigger -> Transition
step current trigger = case (current, trigger) of
    (Firing, Refire) -> ok Firing "repeated"
    (Firing, SourceResolved) -> ok Resolved "resolved"
    (Firing, AckTrigger) -> ok Acked "ack"
    (Acked, Refire) -> ok Acked "repeated"
    (Acked, SourceResolved) -> ok Resolved "resolved"
    (Acked, Unack) -> ok Firing "unack"
    (Acked, CloseTrigger) -> ok Closed "closed"
    (Resolved, Refire) -> ok Firing "repeated"
    (Resolved, AutoClose) -> ok Closed "closed"
    _ -> illegal
    where
        ok to kind = Transition { from = current, to, trigger, eventKind = kind, applied = True }
        illegal = Transition { from = current, to = current, trigger, eventKind = "external", applied = False }

-- | Fold a trigger sequence, collecting every transition (applied or not) so
-- the audit log preserves trigger order.
runSequence :: AlertState -> [Trigger] -> [Transition]
runSequence = go
    where
        go _ [] = []
        go state (t:ts) =
            let transition = step state t
            in transition : go (to transition) ts
