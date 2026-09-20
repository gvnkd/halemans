module Web.View.LlmAdmin.Index where

import Application.Service.Llm.AutoAnalyze (AutoAnalyzeRules (..), allSeverities, allStatuses)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Web.View.Fragments (emptyStateHtml, inlinePostFormHtml, pageHeaderHtml, sectionHeaderHtml, stateBadgeHtml)
import Web.View.Prelude

data TemplateRow = TemplateRow
    { templateId :: Id LlmPromptTemplate
    , name :: Text
    , version :: Int
    , active :: Bool
    , notes :: Maybe Text
    , feedbackScore :: Int64
    , feedbackCount :: Int64
    , rowProtected :: Bool
    }

data CounterRow = CounterRow
    { provider :: Text
    , day :: Day
    , tokensIn :: Int64
    , tokensOut :: Int64
    , requests :: Int
    }

data IndexView = IndexView
    { templates :: [TemplateRow]
    , counters :: [CounterRow]
    , providers :: [LlmConfig]
    , roles :: [LlmAgentRole]
    , endpoint :: Maybe Text
    , model :: Maybe Text
    , toolsEnabled :: Bool
    , dailyBudget :: Int
    , rateLimit :: Int
    , autoAnalyze :: AutoAnalyzeRules
    , toolCacheTtl :: Maybe Int
    , toolCacheSize :: Int
    }

instance View IndexView where
    html IndexView{..} =
        [hsx|
    <div>
        {pageHeaderHtml "LLM" mempty}

        <div class="row g-4 mb-2">
            <div class="col-lg-6 d-flex flex-column">
                {sectionHeaderHtml (tr "Effective configuration") mempty}
                <div class="card flex-fill"><div class="card-body">
                    <table class="table" data-testid="llm-config">
                        <tbody>
                            <tr><td>{tr "Endpoint"}</td><td>{fromMaybe "-" endpoint}</td></tr>
                            <tr><td>{tr "Model"}</td><td>{fromMaybe "-" model}</td></tr>
                            <tr><td>{tr "Tools (read-only)"}</td><td>{if toolsEnabled then tr "enabled" else tr "disabled"}</td></tr>
                            <tr><td>{tr "Daily token budget"}</td><td data-testid="llm-daily-budget">{dailyBudget}</td></tr>
                            <tr><td>{tr "Rate limit"}</td><td data-testid="llm-rate-limit">{trp "{count} requests/min" [("count", tshow rateLimit)]}</td></tr>
                        </tbody>
                    </table>
                    {inlinePostFormHtml (pathTo TestLlmConnectionAction) (tr "Test connection") "btn btn-ghost" (Just "test-llm") False}
                </div></div>
            </div>
            <div class="col-lg-6 d-flex flex-column">
                {sectionHeaderHtml (tr "Tool cache") mempty}
                <div class="card flex-fill"><div class="card-body">
                    <p class="text-muted">{tr "Short-lived memoization of agent tool calls (cmdb_lookup, jira_search, jira_issue_details, assets_lookup), keyed by tool + arguments. Failures are never cached."} {trp "Entries cached: {count}" [("count", tshow toolCacheSize)]}</p>
                    <form method="POST" action={UpdateToolCacheAction} data-testid="tool-cache-form">
                        <div class="form-check mb-2">
                            <input name="enabled" type="checkbox" class="form-check-input" checked={isJust toolCacheTtl} data-testid="tool-cache-enabled"/>
                            <label class="form-check-label">{tr "Enabled"}</label>
                        </div>
                        <div class="mb-2">
                            <label class="form-label">{tr "TTL (seconds)"}</label>
                            <input name="ttlSeconds" type="number" class="form-control" value={ttlValue} data-testid="tool-cache-ttl"/>
                        </div>
                        <button type="submit" class="btn btn-ghost" data-testid="tool-cache-submit">{tr "Save"}</button>
                    </form>
                </div></div>
            </div>
        </div>

        {sectionHeaderHtml (tr "Auto-analysis") mempty}
        <div class="card mb-4"><div class="card-body">
            <p class="text-muted">{tr "New alerts (and enrichment re-triggers) get an LLM analysis only when the alert matches the selected statuses and severities. Manual re-analyze from the alert card is never gated."}</p>
            <form method="POST" action={UpdateAutoAnalyzeAction} data-testid="auto-analyze-form">
                <div class="row mb-2">
                    <div class="col-md-3">
                        <div class="form-check mb-2">
                            <input name="enabled" type="checkbox" class="form-check-input" checked={autoAnalyze.aaEnabled} data-testid="auto-analyze-enabled"/>
                            <label class="form-check-label">{tr "Enabled"}</label>
                        </div>
                    </div>
                    <div class="col-md-3">
                        <div class="form-label">{tr "Statuses"}</div>
                        {forEach allStatuses (flagCheckbox "statuses" autoAnalyze.aaStatuses "auto-analyze-status")}
                    </div>
                    <div class="col-md-3">
                        <div class="form-label">{tr "Severities"}</div>
                        {forEach allSeverities (flagCheckbox "severities" autoAnalyze.aaSeverities "auto-analyze-severity")}
                    </div>
                    <div class="col-md-3">
                        <label class="form-label">{tr "Environments (comma-separated, empty = all; matches the effective env)"}</label>
                        <input name="environments" type="text" class="form-control" value={Text.intercalate ", " autoAnalyze.aaEnvironments} placeholder="dev, prod" data-testid="auto-analyze-envs"/>
                    </div>
                </div>
                <button type="submit" class="btn btn-ghost" data-testid="auto-analyze-submit">{tr "Save"}</button>
            </form>
        </div></div>

        {sectionHeaderHtml (tr "Providers") newProviderButton}
        {providersTable}

        {sectionHeaderHtml (tr "Agent roles") newRoleButton}
        {rolesTable}

        {sectionHeaderHtml (tr "Prompt templates") templateActions}
        {templatesTable}

        {sectionHeaderHtml (tr "Budget counters") mempty}
        {countersTable}
    </div>
    |]
      where
        newProviderButton = [hsx|<a href={NewLlmProviderAction} class="btn btn-brand" data-testid="new-llm-provider">{tr "New provider"}</a>|]
        ttlValue :: Text
        ttlValue = maybe "300" tshow toolCacheTtl
        newRoleButton = [hsx|<a href={NewLlmRoleAction} class="btn btn-brand" data-testid="new-llm-role">{tr "New role"}</a>|]
        templateActions =
            [hsx|
                <div>
                    <a href={LlmQueueAction} class="btn btn-sm btn-ghost" data-testid="llm-queue-link">{tr "Queue"}</a>
                    <a href={NewLlmTemplateAction} class="btn btn-brand" data-testid="new-llm-template">{tr "New template"}</a>
                </div>
            |]
        providersTable =
            if null providers
                then emptyStateHtml "llm-providers-empty" (tr "No providers yet — create one to enable LLM analyses.")
                else
                    [hsx|
                    <table class="table" data-testid="llm-providers">
                        <thead>
                            <tr><th>{tr "Name"}</th><th>{tr "Endpoint"}</th><th>{tr "Model"}</th><th>{tr "API key env"}</th><th>{tr "Tools"}</th><th>{tr "Enabled"}</th><th></th></tr>
                        </thead>
                        <tbody>
                            {forEach providers providerRowHtml}
                        </tbody>
                    </table>
                    |]
        rolesTable =
            if null roles
                then emptyStateHtml "llm-roles-empty" (tr "No agent roles yet.")
                else
                    [hsx|
                    <table class="table" data-testid="llm-roles">
                        <thead>
                            <tr><th>{tr "Name"}</th><th>{tr "Template"}</th><th>{tr "Tools"}</th><th>{tr "Enabled"}</th><th>{tr "Default"}</th><th></th></tr>
                        </thead>
                        <tbody>
                            {forEach roles roleRowHtml}
                        </tbody>
                    </table>
                    |]
        templatesTable =
            if null templates
                then emptyStateHtml "llm-templates-empty" (tr "No prompt templates yet.")
                else
                    [hsx|
                    <table class="table" data-testid="llm-templates">
                        <thead>
                            <tr><th>{tr "Name"}</th><th>{tr "Version"}</th><th>{tr "Active"}</th><th>{tr "Feedback"}</th><th>{tr "Notes"}</th><th></th></tr>
                        </thead>
                        <tbody>
                            {forEach templates templateRowHtml}
                        </tbody>
                    </table>
                    |]
        countersTable =
            if null counters
                then emptyStateHtml "llm-counters-empty" (tr "No LLM usage recorded yet.")
                else
                    [hsx|
                    <table class="table" data-testid="llm-counters">
                        <thead>
                            <tr><th>{tr "Provider"}</th><th>{tr "Day"}</th><th>{tr "Tokens in"}</th><th>{tr "Tokens out"}</th><th>{tr "Requests"}</th></tr>
                        </thead>
                        <tbody>
                            {forEach counters counterRowHtml}
                        </tbody>
                    </table>
                    |]

providerRowHtml :: (?request :: Request) => LlmConfig -> Html
providerRowHtml provider =
    [hsx|
    <tr data-testid="llm-provider">
        <td>{provider.providerName} {protectedBadgeHtml (get #protected provider)}</td>
        <td>{provider.endpoint}</td>
        <td>{provider.model}</td>
        <td>{fromMaybe "-" provider.apiKeyEnv}</td>
        <td>{if provider.toolsEnabled then tr "enabled" else tr "disabled"}</td>
        <td>{enabledBadge}</td>
        <td>
            <a href={EditLlmProviderAction providerId} class="btn btn-sm btn-ghost" data-testid="llm-provider-edit">{tr "Edit"}</a>
            {toggleForm}
            {deleteForm}
        </td>
    </tr>
|]
  where
    providerId = get #id provider
    enabledBadge =
        if provider.enabled
            then [hsx|<span class="badge status-resolved" data-testid="llm-provider-enabled">{tr "enabled"}</span>|]
            else mempty
    toggleForm =
        if provider.enabled
            then inlinePostFormHtml (pathTo (DisableLlmProviderAction providerId)) (tr "Disable") "btn btn-sm btn-ghost" (Just "llm-provider-disable") False
            else inlinePostFormHtml (pathTo (EnableLlmProviderAction providerId)) (tr "Enable") "btn btn-sm btn-ghost" (Just "llm-provider-enable") False
    deleteForm = inlinePostFormHtml (pathTo (DeleteLlmProviderAction providerId)) (tr "Delete") "btn btn-sm btn-ghost btn-ghost-critical" (Just "llm-provider-delete") True

roleRowHtml :: (?request :: Request) => LlmAgentRole -> Html
roleRowHtml role =
    [hsx|
    <tr data-testid="llm-role">
        <td>{role.name} {protectedBadgeHtml (get #protected role)}</td>
        <td>{role.promptTemplateName}</td>
        <td data-testid="llm-role-tools">{toolsText}</td>
        <td>{enabledBadge}</td>
        <td>{defaultBadge}</td>
        <td>
            <a href={EditLlmRoleAction roleId} class="btn btn-sm btn-ghost" data-testid="llm-role-edit">{tr "Edit"}</a>
            {toggleForm}
            {defaultForm}
            {deleteForm}
        </td>
    </tr>
|]
  where
    roleId = get #id role
    toolsText :: Text
    toolsText = case decodeTools role.tools of
        [] -> tr "(none)"
        names -> Text.intercalate ", " names
    enabledBadge = stateBadgeHtml role.enabled "llm-role"
    defaultBadge =
        if role.isDefault
            then [hsx|<span class="badge status-ack" data-testid="llm-role-default">{tr "default"}</span>|]
            else mempty
    toggleForm = inlinePostFormHtml (pathTo (ToggleLlmRoleAction roleId)) toggleLabel "btn btn-sm btn-ghost" (Just "llm-role-toggle") False
    toggleLabel :: Text
    toggleLabel = if role.enabled then tr "Disable" else tr "Enable"
    defaultForm =
        if role.isDefault || not role.enabled
            then mempty
            else inlinePostFormHtml (pathTo (SetDefaultLlmRoleAction roleId)) (tr "Set default") "btn btn-sm btn-ghost" (Just "llm-role-set-default") False
    deleteForm =
        if role.isDefault
            then mempty
            else inlinePostFormHtml (pathTo (DeleteLlmRoleAction roleId)) (tr "Delete") "btn btn-sm btn-ghost btn-ghost-critical" (Just "llm-role-delete") True

decodeTools :: Aeson.Value -> [Text]
decodeTools value = fromMaybe [] (Aeson.decode (Aeson.encode value))

templateRowHtml :: (?request :: Request) => TemplateRow -> Html
templateRowHtml row =
    [hsx|
    <tr data-testid="llm-template">
        <td>{row.name} {protectedBadgeHtml row.rowProtected}</td>
        <td data-testid="llm-template-version">v{row.version}</td>
        <td>{activeBadge}</td>
        <td data-testid="llm-template-feedback">{row.feedbackScore} ({trp "votes: {count}" [("count", tshow row.feedbackCount)]})</td>
        <td>{fromMaybe "" row.notes}</td>
        <td>
            <a href={EditLlmTemplateAction row.templateId} class="btn btn-sm btn-ghost" data-testid="llm-template-edit">{tr "Edit (new version)"}</a>
            {activateForm}
            {deleteForm}
        </td>
    </tr>
|]
  where
    activeBadge =
        if row.active
            then [hsx|<span class="badge status-resolved" data-testid="llm-template-active">{tr "active"}</span>|]
            else mempty
    activateForm =
        if row.active
            then mempty
            else inlinePostFormHtml (pathTo (ActivateLlmTemplateAction row.templateId)) (tr "Activate") "btn btn-sm btn-ghost" (Just "llm-template-activate") False
    deleteForm =
        if row.active
            then mempty
            else inlinePostFormHtml (pathTo (DeleteLlmTemplateAction row.templateId)) (tr "Delete") "btn btn-sm btn-ghost btn-ghost-critical" (Just "llm-template-delete") True

counterRowHtml :: CounterRow -> Html
counterRowHtml row =
    [hsx|
    <tr data-testid="llm-counter">
        <td>{row.provider}</td>
        <td>{show row.day}</td>
        <td>{row.tokensIn}</td>
        <td>{row.tokensOut}</td>
        <td>{row.requests}</td>
    </tr>
|]

-- One labelled checkbox per status/severity flag in the auto-analysis form.
flagCheckbox :: Text -> [Text] -> Text -> Text -> Html
flagCheckbox fieldName selected testIdBase value =
    [hsx|
    <div class="form-check">
        <input name={fieldName} value={value} type="checkbox" class="form-check-input" checked={value `elem` selected} data-testid={testIdBase <> "-" <> value}/>
        <label class="form-check-label">{value}</label>
    </div>
|]
