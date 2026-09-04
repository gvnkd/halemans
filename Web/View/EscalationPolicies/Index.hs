module Web.View.EscalationPolicies.Index where
import Web.View.Prelude
import Application.Pipeline.Escalation (stepsFromJSON, EscalationStep (..))
import qualified Data.Text as Text

data IndexView = IndexView { policies :: [EscalationPolicy] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex justify-content-between align-items-center">
            <h1>Escalation policies</h1>
            <a href={NewEscalationPolicyAction} class="btn btn-sm btn-primary" data-testid="new-escalation-policy">New policy</a>
        </div>
        <table class="table" data-testid="escalation-policies-table">
            <thead>
                <tr>
                    <th>Name</th>
                    <th>Steps</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach policies renderPolicy}
            </tbody>
        </table>
    |]

renderPolicy :: EscalationPolicy -> Html
renderPolicy policy = [hsx|
    <tr data-testid="escalation-policy-row">
        <td>{policy.name}</td>
        <td>{forEach (stepsFromJSON policy.steps) renderStep}</td>
        <td>
            <a href={EditEscalationPolicyAction policy.id} class="btn btn-sm btn-outline-secondary" data-testid="edit-escalation-policy">Edit</a>
            <form method="POST" action={DeleteEscalationPolicyAction policy.id} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-danger">Delete</button>
            </form>
        </td>
    </tr>
|]

renderStep :: EscalationStep -> Html
renderStep step = [hsx|
    <span class="badge bg-secondary">
        after {step.esAfterSeconds}s → {targetLabel}{unlessLabel}
    </span>
|]
    where
        targetLabel :: Text
        targetLabel = case (step.esTargetTeamId, step.esTargetUserId) of
            (Just teamId, _) -> "team " <> shortId teamId
            (_, Just userId) -> "user " <> shortId userId
            _ -> "—"
        unlessLabel = maybe "" (\status -> " unless " <> status) step.esUnlessStatus
        shortId = Text.take 8
