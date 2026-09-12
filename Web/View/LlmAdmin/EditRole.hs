module Web.View.LlmAdmin.EditRole where

import Web.View.LlmAdmin.RoleForm (roleFormFields)
import Web.View.Prelude

data EditRoleView = EditRoleView
    { role :: LlmAgentRole
    , toolNames :: [Text]
    }

instance View EditRoleView where
    html EditRoleView{..} =
        [hsx|
        <h1>Edit agent role</h1>
        <p class="text-muted">{role.name} is {stateText}. Enable/default actions live on the roles list.</p>
        <form method="POST" action={UpdateLlmRoleAction (get #id role)} data-testid="llm-role-form">
            {roleFormFields (Just role) toolNames}
            <button type="submit" class="btn btn-primary" data-testid="llm-role-save">Save role</button>
            <a href={LlmAdminAction} class="btn btn-outline-secondary">Cancel</a>
        </form>
    |]
      where
        stateText :: Text
        stateText = if role.enabled then "enabled" else "disabled"
