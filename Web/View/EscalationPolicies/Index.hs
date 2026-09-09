module Web.View.EscalationPolicies.Index where
import Web.View.Prelude
import Web.View.Fragments (pageHeaderHtml, editDeleteActionsHtml)
import Application.Pipeline.Escalation (stepsFromJSON, EscalationStep (..))
import qualified Data.Text as Text

data IndexView = IndexView { policies :: [EscalationPolicy] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {pageHeaderHtml "Escalation policies" newButton}
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
        where
            newButton = [hsx|<a href={NewEscalationPolicyAction} class="btn btn-sm btn-primary" data-testid="new-escalation-policy">New policy</a>|]

renderPolicy :: EscalationPolicy -> Html
renderPolicy policy = [hsx|
    <tr data-testid="escalation-policy-row">
        <td>{policy.name}</td>
        <td>{forEach (stepsFromJSON policy.steps) renderStep}</td>
        <td>
            {editDeleteActionsHtml (pathTo (EditEscalationPolicyAction policy.id)) (pathTo (DeleteEscalationPolicyAction policy.id)) "edit-escalation-policy"}
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
