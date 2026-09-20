module Web.View.LlmAdmin.EditRole where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.LlmAdmin.RoleForm (roleFormFields)
import Web.View.Prelude

data EditRoleView = EditRoleView
    { role :: LlmAgentRole
    , toolNames :: [Text]
    }

instance View EditRoleView where
    html EditRoleView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit agent role") mempty}
        <p class="text-muted">{trp "{name} is {state}. Enable/default actions live on the roles list." [("name", role.name), ("state", stateText)]}</p>
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={UpdateLlmRoleAction (get #id role)} data-testid="llm-role-form">
            {roleFormFields (Just role) toolNames}
            <button type="submit" class="btn btn-brand" data-testid="llm-role-save">{tr "Save role"}</button>
            <a href={LlmAdminAction} class="btn btn-ghost">{tr "Cancel"}</a>
        </form>
        </div></div>
    |]
      where
        stateText :: Text
        stateText = if role.enabled then tr "enabled" else tr "disabled"
