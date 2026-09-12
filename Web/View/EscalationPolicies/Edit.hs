module Web.View.EscalationPolicies.Edit where

import Application.Pipeline.Escalation (stepsFromJSON)
import Web.View.EscalationPolicies.New (stepEditor)
import Web.View.Prelude

data EditView = EditView
    { policy :: EscalationPolicy
    , teams :: [Team]
    , users :: [User]
    }

instance View EditView where
    html EditView{..} =
        [hsx|
        <h1>Edit escalation policy</h1>
        <form method="POST" action={UpdateEscalationPolicyAction policy.id} data-testid="escalation-policy-edit-form" class="maxw-700">
            <div class="mb-3">
                <label class="form-label">Name</label>
                <input name="name" type="text" class="form-control" value={policy.name} data-testid="policy-name" required="required"/>
            </div>
            {stepEditor teams users (stepsFromJSON policy.steps)}
            <button type="submit" class="btn btn-primary" data-testid="policy-submit">Save</button>
        </form>
    |]
