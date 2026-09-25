module Web.View.Integrations.JiraNew where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Integrations.Form (jiraConfigFormFields)
import Web.View.Prelude

data JiraNewView = JiraNewView

instance View JiraNewView where
    beforeRender _ = setPageTitle (tr "Integrations")
    html JiraNewView =
        [hsx|
        {pageHeaderHtml (tr "New Jira connection") mempty}
        <div class="card maxw-500"><div class="card-body">
        <form method="POST" action={CreateJiraConfigAction} data-testid="jira-config-form">
            {jiraConfigFormFields Nothing}
            <button type="submit" class="btn btn-brand" data-testid="jira-config-submit">{tr "Create"}</button>
        </form>
        </div></div>|]
