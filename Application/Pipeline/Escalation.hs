module Application.Pipeline.Escalation
( EscalationStep (..)
, stepsFromJSON
, stepDeadline
, DueDecision (..)
, TrackerAdvance (..)
, decideDueTracker
) where

import IHP.Prelude
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import Data.Aeson.Types (parseMaybe)

-- Pure escalation-step arithmetic (design_docs/milestone_2.md §6). DB rows
-- are decoded upstream; this module never touches the DB.

data EscalationStep = EscalationStep
    { esAfterSeconds :: Int
    , esTargetTeamId :: Maybe Text
    , esTargetUserId :: Maybe Text
    , esUnlessStatus :: Maybe Text
    }
    deriving (Eq, Show)

-- | Policy `steps` jsonb shape:
-- [{"after_seconds": 600, "target_team_id": "<uuid>", "unless_status": "ack"}, ...]
-- Malformed entries are dropped.
stepsFromJSON :: Value -> [EscalationStep]
stepsFromJSON (Array steps) = mapMaybe (parseMaybe parseStep) (Vector.toList steps)
    where
        parseStep = Aeson.withObject "escalation step" \o -> do
            afterSeconds <- o Aeson..: "after_seconds"
            targetTeamId <- o Aeson..:? "target_team_id"
            targetUserId <- o Aeson..:? "target_user_id"
            unlessStatus <- o Aeson..:? "unless_status"
            pure EscalationStep
                { esAfterSeconds = afterSeconds
                , esTargetTeamId = targetTeamId
                , esTargetUserId = targetUserId
                , esUnlessStatus = unlessStatus
                }
stepsFromJSON _ = []

-- | Deadline for a step relative to the previous step's fire time (or the
-- tracker creation time for step 0).
stepDeadline :: UTCTime -> EscalationStep -> UTCTime
stepDeadline base step = addUTCTime (fromIntegral step.esAfterSeconds) base

data TrackerAdvance
    = AdvanceTo Int UTCTime
    | MarkDone
    deriving (Eq, Show)

data DueDecision
    = EscalateNotify EscalationStep TrackerAdvance
    | EscalateCancel
    deriving (Eq, Show)

-- | What to do with an `active` tracker whose deadline passed. Alert status
-- and suppression are re-checked here so a missed cancel can't page anyone.
decideDueTracker :: UTCTime -> [EscalationStep] -> Int -> Text -> Bool -> DueDecision
decideDueTracker now steps currentStep alertStatus suppressed
    | suppressed = EscalateCancel
    | alertStatus /= "firing" = EscalateCancel
    | otherwise = case drop currentStep steps of
        [] -> EscalateCancel
        (step:_)
            | Just alertStatus == step.esUnlessStatus -> EscalateCancel
            | otherwise -> EscalateNotify step (advance currentStep)
    where
        advance index = case drop (index + 1) steps of
            (nextStep:_) -> AdvanceTo (index + 1) (stepDeadline now nextStep)
            [] -> MarkDone
