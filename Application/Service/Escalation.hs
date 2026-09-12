module Application.Service.Escalation (
    createTracker,
    cancelTrackersFor,
    restartTrackersFor,
) where

import Application.Pipeline.Escalation (stepDeadline, stepsFromJSON)
import Control.Monad (void)
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

-- Escalation tracker lifecycle (design_docs/milestone_2.md §6). Stepping
-- lives in Application.Job.Escalation; the deadline arithmetic in
-- Application.Pipeline.Escalation (pure).

-- | Start tracking an alert for a rule's attached policy. No-op when an
-- active tracker already exists for the alert or the policy has no steps.
createTracker :: (?modelContext :: ModelContext) => Alert -> NotificationRule -> Id EscalationPolicy -> IO (Maybe (Id EscalationTracker))
createTracker alert rule policyId = do
    existing <-
        query @EscalationTracker
            |> filterWhere (#alertId, get #id alert)
            |> filterWhere (#status, "active" :: Text)
            |> fetchOneOrNothing
    case existing of
        Just tracker -> pure (Just (get #id tracker))
        Nothing -> do
            policy <- fetch policyId
            case stepsFromJSON policy.steps of
                [] -> pure Nothing
                (firstStep : _) -> do
                    now <- getCurrentTime
                    tracker <-
                        newRecord @EscalationTracker
                            |> set #alertId (get #id alert)
                            |> set #policyId policyId
                            |> set #ruleId (Just (get #id rule))
                            |> set #currentStep 0
                            |> set #nextDeadline (Just (stepDeadline now firstStep))
                            |> set #status "active"
                            |> createRecord
                    pure (Just (get #id tracker))

-- | Ack/close/resolve cancel the tracker (§6).
cancelTrackersFor :: (?modelContext :: ModelContext) => Id Alert -> IO ()
cancelTrackersFor alertId = do
    now <- getCurrentTime
    active <-
        query @EscalationTracker
            |> filterWhere (#alertId, alertId)
            |> filterWhere (#status, "active" :: Text)
            |> fetch
    forM_ active \tracker ->
        void $
            tracker
                |> set #status "cancelled"
                |> set #updatedAt now
                |> updateRecord

-- | Unack re-activates from step 0 (§6): the most recently cancelled tracker
-- for the alert goes active again with a fresh first-step deadline.
restartTrackersFor :: (?modelContext :: ModelContext) => Id Alert -> IO ()
restartTrackersFor alertId = do
    cancelled <-
        query @EscalationTracker
            |> filterWhere (#alertId, alertId)
            |> filterWhere (#status, "cancelled" :: Text)
            |> orderByDesc #updatedAt
            |> fetchOneOrNothing
    forM_ cancelled \tracker -> do
        policy <- fetch tracker.policyId
        case stepsFromJSON policy.steps of
            [] -> pure ()
            (firstStep : _) -> do
                now <- getCurrentTime
                void $
                    tracker
                        |> set #status "active"
                        |> set #currentStep 0
                        |> set #nextDeadline (Just (stepDeadline now firstStep))
                        |> set #updatedAt now
                        |> updateRecord
