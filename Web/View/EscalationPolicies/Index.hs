module Web.View.EscalationPolicies.Index where

import Application.Pipeline.Escalation (EscalationStep (..), stepsFromJSON)
import qualified Data.Text as Text
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Fragments (editDeleteActionsHtml, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView {policies :: [EscalationPolicy]}

instance View IndexView where
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml (tr "Escalation policies") newButton}
        <table class="table" data-testid="escalation-policies-table">
            <thead>
                <tr>
                    <th>{tr "Name"}</th>
                    <th>{tr "Steps"}</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach policies renderPolicy}
            </tbody>
        </table>
    |]
      where
        newButton = [hsx|<a href={NewEscalationPolicyAction} class="btn btn-sm btn-primary" data-testid="new-escalation-policy">{tr "New policy"}</a>|]

renderPolicy :: (CurrentUserRecord ~ User, ?request :: Request) => EscalationPolicy -> Html
renderPolicy policy =
    [hsx|
    <tr data-testid="escalation-policy-row">
        <td>{policy.name} {protectedBadgeHtml (get #protected policy)}</td>
        <td>{forEach (stepsFromJSON policy.steps) renderStep}</td>
        <td>
            {editDeleteActionsHtml (pathTo (EditEscalationPolicyAction policy.id)) (pathTo (DeleteEscalationPolicyAction policy.id)) "edit-escalation-policy"}
        </td>
    </tr>
|]

renderStep :: (CurrentUserRecord ~ User, ?request :: Request) => EscalationStep -> Html
renderStep step =
    [hsx|
    <span class="badge bg-secondary">
        {trp "after {seconds}s → {target}{unless}" [("seconds", tshow step.esAfterSeconds), ("target", targetLabel), ("unless", unlessLabel)]}
    </span>
|]
  where
    targetLabel :: Text
    targetLabel = case (step.esTargetTeamId, step.esTargetUserId) of
        (Just teamId, _) -> trp "team {id}" [("id", shortId teamId)]
        (_, Just userId) -> trp "user {id}" [("id", shortId userId)]
        _ -> "—"
    unlessLabel = maybe "" (\status -> trp " unless {status}" [("status", status)]) step.esUnlessStatus
    shortId = Text.take 8
