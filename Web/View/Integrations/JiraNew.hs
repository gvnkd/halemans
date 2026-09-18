module Web.View.Integrations.JiraNew where

import Web.View.Integrations.Form (jiraConfigFormFields)
import Web.View.Prelude

data JiraNewView = JiraNewView

instance View JiraNewView where
    html JiraNewView =
        [hsx|
        <h1>{tr "New Jira connection"}</h1>
        <form method="POST" action={CreateJiraConfigAction} data-testid="jira-config-form" class="maxw-500">
            {jiraConfigFormFields Nothing}
            <button type="submit" class="btn btn-primary" data-testid="jira-config-submit">{tr "Create"}</button>
        </form>
    |]
