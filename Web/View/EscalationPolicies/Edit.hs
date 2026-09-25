module Web.View.EscalationPolicies.Edit where

import Application.Pipeline.Escalation (stepsFromJSON)
import Web.View.EscalationPolicies.New (stepEditor)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data EditView = EditView
    { policy :: EscalationPolicy
    , teams :: [Team]
    , users :: [User]
    }

instance View EditView where
    beforeRender _ = setPageTitle (tr "Escalation policies")
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit escalation policy") mempty}
        <div class="card maxw-700"><div class="card-body">
        <form method="POST" action={UpdateEscalationPolicyAction policy.id} data-testid="escalation-policy-edit-form">
            <div class="mb-3">
                <label class="form-label">{tr "Name"}</label>
                <input name="name" type="text" class="form-control" value={policy.name} data-testid="policy-name" required="required"/>
            </div>
            {stepEditor teams users (stepsFromJSON policy.steps)}
            <button type="submit" class="btn btn-brand" data-testid="policy-submit">{tr "Save"}</button>
        </form>
        </div></div>|]
