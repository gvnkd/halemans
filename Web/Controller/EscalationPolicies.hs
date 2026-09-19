module Web.Controller.EscalationPolicies where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Web.Controller.Prelude
import Web.View.EscalationPolicies.Edit
import Web.View.EscalationPolicies.Index
import Web.View.EscalationPolicies.New

instance Controller EscalationPoliciesController where
    beforeAction = ensureIsUser

    action EscalationPoliciesAction = do
        requirePrivilege "manage_rules"
        policies <- query @EscalationPolicy |> orderByAsc #name |> fetch
        render IndexView{policies}
    action NewEscalationPolicyAction = do
        requirePrivilege "manage_rules"
        (teams, users) <- formChoices
        render NewView{teams, users}
    action CreateEscalationPolicyAction = do
        requirePrivilege "manage_rules"
        case stepsFromForm of
            [] -> setErrorMessage (tr "at least one step is required")
            steps -> do
                _ <-
                    newRecord @EscalationPolicy
                        |> set #name (param @Text "name")
                        |> set #steps (Aeson.toJSON steps)
                        |> createRecord
                setSuccessMessage (tr "Escalation policy created")
        redirectTo EscalationPoliciesAction
    action EditEscalationPolicyAction{escalationPolicyId} = do
        requirePrivilege "manage_rules"
        policy <- fetch escalationPolicyId
        ensureNotProtected policy.name (get #protected policy)
        (teams, users) <- formChoices
        render EditView{policy, teams, users}
    action UpdateEscalationPolicyAction{escalationPolicyId} = do
        requirePrivilege "manage_rules"
        policy <- fetch escalationPolicyId
        ensureNotProtected policy.name (get #protected policy)
        case stepsFromForm of
            [] -> setErrorMessage (tr "at least one step is required")
            steps -> do
                _ <-
                    policy
                        |> set #name (param @Text "name")
                        |> set #steps (Aeson.toJSON steps)
                        |> updateRecord
                setSuccessMessage (tr "Escalation policy updated")
        redirectTo EscalationPoliciesAction
    action DeleteEscalationPolicyAction{escalationPolicyId} = do
        requirePrivilege "manage_rules"
        policy <- fetch escalationPolicyId
        ensureNotProtected policy.name (get #protected policy)
        deleteRecord policy
        setSuccessMessage (tr "Escalation policy deleted")
        redirectTo EscalationPoliciesAction

formChoices :: (?modelContext :: ModelContext) => IO ([Team], [User])
formChoices = do
    teams <- query @Team |> orderByAsc #name |> fetch
    users <- query @User |> orderByAsc #email |> fetch
    pure (teams, users)

-- | Parallel form arrays: stepAfter[], stepTarget[] ("team:<uuid>" |
-- "user:<uuid>"), stepUnless[] ("" | status). Rows without a positive
-- after_seconds are dropped (§6 step shape).
stepsFromForm :: (?request :: Request, ?respond :: Respond) => [Value]
stepsFromForm =
    let afters = paramList @Text "stepAfter"
        targets = paramList @Text "stepTarget"
        unlesses = paramList @Text "stepUnless"
        row after target unlessStatus = do
            seconds <- readMaybe (cs after) :: Maybe Int
            if seconds <= 0
                then Nothing
                else
                    Just $
                        object
                            [ "after_seconds" .= seconds
                            , "target_team_id" .= targetPiece "team" target
                            , "target_user_id" .= targetPiece "user" target
                            , "unless_status" .= (if Text.null unlessStatus then Nothing else Just unlessStatus)
                            ]
        targetPiece kind raw =
            let (prefix, rest) = Text.break (== ':') raw
             in if prefix == kind && not (Text.null rest) then Just (Text.drop 1 rest) else Nothing
     in catMaybes (zipWith3 row afters targets unlesses)
