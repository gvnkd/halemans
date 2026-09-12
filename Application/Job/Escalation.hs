module Application.Job.Escalation (runDueTrackers) where

import Application.Pipeline.Escalation (DueDecision (..), EscalationStep (..), TrackerAdvance (..), decideDueTracker, stepsFromJSON)
import Application.Service.Notify (currentOnCall, notifyUsers, teamMemberIds)
import Control.Monad (void)
import Data.Aeson (Value, object, (.=))
import Generated.Types
import IHP.Fetch (fetch)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlExecTyped, typedSql)

-- Drives due escalation steps (design_docs/milestone_2.md §6): every 30s,
-- scan active trackers past deadline; notify the step target, append
-- AlertEvent(escalated), advance or finish. Self-rescheduling like
-- PollZabbixJob.
instance Job EscalationJob where
    perform _job = do
        runDueTrackers

        now <- getCurrentTime
        next <-
            newRecord @EscalationJob
                |> set #runAt (addUTCTime 30 now)
                |> createRecord
        let nextId = get #id next
        _ <-
            sqlExecTyped
                [typedSql|
            DELETE FROM escalation_jobs
            WHERE status = 'job_status_not_started' AND id <> ${nextId}
        |]
        pure ()

    queuePollInterval = 5 * 1000000
    maxAttempts = 3

-- | Fire every active tracker whose deadline passed. Exported for the
-- integration suite.
runDueTrackers :: (?modelContext :: ModelContext) => IO ()
runDueTrackers = do
    due <-
        query @EscalationTracker
            |> filterWhere (#status, "active" :: Text)
            |> filterWhereSql (#nextDeadline, "<= NOW()")
            |> fetch
    forM_ due fireTracker

fireTracker :: (?modelContext :: ModelContext) => EscalationTracker -> IO ()
fireTracker tracker = do
    now <- getCurrentTime
    alert <- fetch tracker.alertId
    policy <- fetch tracker.policyId
    let steps = stepsFromJSON policy.steps
    case decideDueTracker now steps tracker.currentStep alert.status alert.suppressed of
        EscalateCancel ->
            void $
                tracker
                    |> set #status "cancelled"
                    |> set #updatedAt now
                    |> updateRecord
        EscalateNotify step advance -> do
            targets <- resolveStepTargets step
            notifyUsers alert targets
            recordEscalatedEvent tracker step targets
            case advance of
                MarkDone ->
                    void $
                        tracker
                            |> set #status "done"
                            |> set #updatedAt now
                            |> updateRecord
                AdvanceTo nextStep deadline ->
                    void $
                        tracker
                            |> set #currentStep nextStep
                            |> set #nextDeadline (Just deadline)
                            |> set #updatedAt now
                            |> updateRecord

resolveStepTargets :: (?modelContext :: ModelContext) => EscalationStep -> IO [Id User]
resolveStepTargets step = case (step.esTargetTeamId, step.esTargetUserId) of
    (_, Just userIdText) -> pure [textToId userIdText]
    (Just teamIdText, Nothing) -> do
        onCall <- currentOnCall (textToId teamIdText)
        case onCall of
            Just userId -> pure [userId]
            Nothing -> teamMemberIds (textToId teamIdText)
    (Nothing, Nothing) -> pure []

recordEscalatedEvent :: (?modelContext :: ModelContext) => EscalationTracker -> EscalationStep -> [Id User] -> IO ()
recordEscalatedEvent tracker step targets = do
    let payload :: Value =
            object
                [ "step" .= tracker.currentStep
                , "policyId" .= tshow tracker.policyId
                , "targets" .= map (tshow :: Id User -> Text) targets
                ]
    _ <-
        newRecord @AlertEvent
            |> set #alertId tracker.alertId
            |> set #userId Nothing
            |> set #kind "escalated"
            |> set #payload payload
            |> createRecord
    pure ()
