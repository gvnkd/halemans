module Web.View.LlmAdmin.NewRole where

import Web.View.LlmAdmin.RoleForm (roleFormFields)
import Web.View.Prelude

data NewRoleView = NewRoleView

instance View NewRoleView where
    html NewRoleView =
        [hsx|
        <h1>New agent role</h1>
        <p class="text-muted">
            A role bundles a prompt template name with a tool whitelist for LLM enrichment.
            Tools: comma-separated subset of cmdb_lookup, jira_search, jira_issue_details, assets_lookup (empty = no tools).
        </p>
        <form method="POST" action={CreateLlmRoleAction} data-testid="llm-role-new-form">
            {roleFormFields Nothing []}
            <button type="submit" class="btn btn-primary" data-testid="llm-role-save">Create role</button>
            <a href={LlmAdminAction} class="btn btn-outline-secondary">Cancel</a>
        </form>
    |]
