module Web.View.LlmAdmin.NewRole where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.LlmAdmin.RoleForm (roleFormFields)
import Web.View.Prelude

data NewRoleView = NewRoleView

instance View NewRoleView where
    beforeRender _ = setPageTitle (tr "New agent role")
    html NewRoleView =
        [hsx|
        {pageHeaderHtml (tr "New agent role") mempty}
        <p class="text-muted">
            {tr "A role bundles a prompt template name with a tool whitelist for LLM enrichment. Tools: comma-separated subset of cmdb_lookup, jira_search, jira_issue_details, assets_lookup (empty = no tools)."}
        </p>
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={CreateLlmRoleAction} data-testid="llm-role-new-form">
            {roleFormFields Nothing []}
            <button type="submit" class="btn btn-brand" data-testid="llm-role-save">{tr "Create role"}</button>
            <a href={LlmAdminAction} class="btn btn-ghost">{tr "Cancel"}</a>
        </form>
        </div></div>
    |]
