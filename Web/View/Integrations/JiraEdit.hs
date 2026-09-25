module Web.View.Integrations.JiraEdit where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Integrations.Form (jiraConfigFormFields)
import Web.View.Prelude

data JiraEditView = JiraEditView {config :: JiraConfig}

instance View JiraEditView where
    beforeRender _ = setPageTitle (tr "Integrations")
    html JiraEditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit Jira connection") mempty}
        <div class="card maxw-500"><div class="card-body">
        <form method="POST" action={UpdateJiraConfigAction config.id} data-testid="jira-config-edit-form">
            {jiraConfigFormFields (Just config)}
            <button type="submit" class="btn btn-brand" data-testid="jira-config-submit">{tr "Save"}</button>
        </form>
        </div></div>|]
