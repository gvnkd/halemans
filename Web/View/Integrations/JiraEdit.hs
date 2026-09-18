module Web.View.Integrations.JiraEdit where

import Web.View.Integrations.Form (jiraConfigFormFields)
import Web.View.Prelude

data JiraEditView = JiraEditView {config :: JiraConfig}

instance View JiraEditView where
    html JiraEditView{..} =
        [hsx|
        <h1>{tr "Edit Jira connection"}</h1>
        <form method="POST" action={UpdateJiraConfigAction config.id} data-testid="jira-config-edit-form" class="maxw-500">
            {jiraConfigFormFields (Just config)}
            <button type="submit" class="btn btn-primary" data-testid="jira-config-submit">{tr "Save"}</button>
        </form>
    |]
