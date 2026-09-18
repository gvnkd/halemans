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
        <h1>{tr "Edit agent role"}</h1>
        <p class="text-muted">{trp "{name} is {state}. Enable/default actions live on the roles list." [("name", role.name), ("state", stateText)]}</p>
        <form method="POST" action={UpdateLlmRoleAction (get #id role)} data-testid="llm-role-form">
            {roleFormFields (Just role) toolNames}
            <button type="submit" class="btn btn-primary" data-testid="llm-role-save">{tr "Save role"}</button>
            <a href={LlmAdminAction} class="btn btn-outline-secondary">{tr "Cancel"}</a>
        </form>
    |]
      where
        stateText :: Text
        stateText = if role.enabled then tr "enabled" else tr "disabled"
