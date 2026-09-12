module Web.View.LlmAdmin.Index where

import Application.Service.Llm.AutoAnalyze (AutoAnalyzeRules (..), allSeverities, allStatuses)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Web.View.Fragments (inlinePostFormHtml, sectionHeaderHtml, stateBadgeHtml)
import Web.View.Prelude

data TemplateRow = TemplateRow
    { templateId :: Id LlmPromptTemplate
    , name :: Text
    , version :: Int
    , active :: Bool
    , notes :: Maybe Text
    , feedbackScore :: Int64
    , feedbackCount :: Int64
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
        <h1>LLM</h1>

        <h2>Effective configuration</h2>
        <table class="table maxw-700" data-testid="llm-config">
            <tbody>
                <tr><td>Endpoint</td><td>{fromMaybe "-" endpoint}</td></tr>
                <tr><td>Model</td><td>{fromMaybe "-" model}</td></tr>
                <tr><td>Tools (read-only)</td><td>{if toolsEnabled then "enabled" else "disabled" :: Text}</td></tr>
                <tr><td>Daily token budget</td><td data-testid="llm-daily-budget">{dailyBudget}</td></tr>
                <tr><td>Rate limit</td><td data-testid="llm-rate-limit">{rateLimit} requests/min</td></tr>
            </tbody>
        </table>
        {inlinePostFormHtml (pathTo TestLlmConnectionAction) "Test connection" "btn btn-sm btn-outline-primary" (Just "test-llm") False}

        {sectionHeaderHtml "Auto-analysis" mempty}
        <p class="text-muted">New alerts (and enrichment re-triggers) get an LLM analysis only when the alert matches the selected statuses and severities. Manual re-analyze from the alert card is never gated.</p>
        <form method="POST" action={UpdateAutoAnalyzeAction} class="maxw-700" data-testid="auto-analyze-form">
            <div class="form-check mb-2">
                <input name="enabled" type="checkbox" class="form-check-input" checked={autoAnalyze.aaEnabled} data-testid="auto-analyze-enabled"/>
                <label class="form-check-label">Enabled</label>
            </div>
            <div class="row mb-2">
                <div class="col">
                    <div class="form-label">Statuses</div>
                    {forEach allStatuses (flagCheckbox "statuses" autoAnalyze.aaStatuses "auto-analyze-status")}
                </div>
                <div class="col">
                    <div class="form-label">Severities</div>
                    {forEach allSeverities (flagCheckbox "severities" autoAnalyze.aaSeverities "auto-analyze-severity")}
                </div>
            </div>
            <div class="mb-2">
                <label class="form-label">Environments (comma-separated, empty = all; matches the effective env)</label>
                <input name="environments" type="text" class="form-control" value={Text.intercalate ", " autoAnalyze.aaEnvironments} placeholder="dev, prod" data-testid="auto-analyze-envs"/>
            </div>
            <button type="submit" class="btn btn-sm btn-primary" data-testid="auto-analyze-submit">Save</button>
        </form>

        {sectionHeaderHtml "Tool cache" mempty}
        <p class="text-muted">Short-lived memoization of agent tool calls (cmdb_lookup, jira_search, jira_issue_details, assets_lookup), keyed by tool + arguments. Failures are never cached. {toolCacheSize} entries cached.</p>
        <form method="POST" action={UpdateToolCacheAction} class="maxw-700" data-testid="tool-cache-form">
            <div class="form-check mb-2">
                <input name="enabled" type="checkbox" class="form-check-input" checked={isJust toolCacheTtl} data-testid="tool-cache-enabled"/>
                <label class="form-check-label">Enabled</label>
            </div>
            <div class="mb-2">
                <label class="form-label">TTL (seconds)</label>
                <input name="ttlSeconds" type="number" class="form-control" value={ttlValue} data-testid="tool-cache-ttl"/>
            </div>
            <button type="submit" class="btn btn-sm btn-primary" data-testid="tool-cache-submit">Save</button>
        </form>

        {sectionHeaderHtml "Providers" newProviderButton}
        <table class="table" data-testid="llm-providers">
            <thead>
                <tr><th>Name</th><th>Endpoint</th><th>Model</th><th>API key env</th><th>Tools</th><th>Enabled</th><th></th></tr>
            </thead>
            <tbody>
                {forEach providers providerRowHtml}
            </tbody>
        </table>

        {sectionHeaderHtml "Agent roles" newRoleButton}
        <table class="table" data-testid="llm-roles">
            <thead>
                <tr><th>Name</th><th>Template</th><th>Tools</th><th>Enabled</th><th>Default</th><th></th></tr>
            </thead>
            <tbody>
                {forEach roles roleRowHtml}
            </tbody>
        </table>

        {sectionHeaderHtml "Prompt templates" templateActions}
        <table class="table" data-testid="llm-templates">
            <thead>
                <tr><th>Name</th><th>Version</th><th>Active</th><th>Feedback</th><th>Notes</th><th></th></tr>
            </thead>
            <tbody>
                {forEach templates templateRowHtml}
            </tbody>
        </table>

        <h2 class="mt-4">Budget counters</h2>
        <table class="table" data-testid="llm-counters">
            <thead>
                <tr><th>Provider</th><th>Day</th><th>Tokens in</th><th>Tokens out</th><th>Requests</th></tr>
            </thead>
            <tbody>
                {forEach counters counterRowHtml}
            </tbody>
        </table>
    |]
      where
        newProviderButton = [hsx|<a href={NewLlmProviderAction} class="btn btn-sm btn-primary" data-testid="new-llm-provider">New provider</a>|]
        ttlValue :: Text
        ttlValue = maybe "300" tshow toolCacheTtl
        newRoleButton = [hsx|<a href={NewLlmRoleAction} class="btn btn-sm btn-primary" data-testid="new-llm-role">New role</a>|]
        templateActions =
            [hsx|
                <div>
                    <a href={LlmQueueAction} class="btn btn-sm btn-outline-secondary" data-testid="llm-queue-link">Queue</a>
                    <a href={NewLlmTemplateAction} class="btn btn-sm btn-primary" data-testid="new-llm-template">New template</a>
                </div>
            |]

providerRowHtml :: LlmConfig -> Html
providerRowHtml provider =
    [hsx|
    <tr data-testid="llm-provider">
        <td>{provider.providerName}</td>
        <td>{provider.endpoint}</td>
        <td>{provider.model}</td>
        <td>{fromMaybe "-" provider.apiKeyEnv}</td>
        <td>{if provider.toolsEnabled then "enabled" else "disabled" :: Text}</td>
        <td>{enabledBadge}</td>
        <td>
            <a href={EditLlmProviderAction providerId} class="btn btn-sm btn-outline-secondary" data-testid="llm-provider-edit">Edit</a>
            {toggleForm}
            {deleteForm}
        </td>
    </tr>
|]
  where
    providerId = get #id provider
    enabledBadge =
        if provider.enabled
            then [hsx|<span class="badge status-resolved" data-testid="llm-provider-enabled">enabled</span>|]
            else mempty
    toggleForm =
        if provider.enabled
            then inlinePostFormHtml (pathTo (DisableLlmProviderAction providerId)) "Disable" "btn btn-sm btn-outline-warning" (Just "llm-provider-disable") False
            else inlinePostFormHtml (pathTo (EnableLlmProviderAction providerId)) "Enable" "btn btn-sm btn-outline-primary" (Just "llm-provider-enable") False
    deleteForm = inlinePostFormHtml (pathTo (DeleteLlmProviderAction providerId)) "Delete" "btn btn-sm btn-outline-danger" (Just "llm-provider-delete") False

roleRowHtml :: LlmAgentRole -> Html
roleRowHtml role =
    [hsx|
    <tr data-testid="llm-role">
        <td>{role.name}</td>
        <td>{role.promptTemplateName}</td>
        <td data-testid="llm-role-tools">{toolsText}</td>
        <td>{enabledBadge}</td>
        <td>{defaultBadge}</td>
        <td>
            <a href={EditLlmRoleAction roleId} class="btn btn-sm btn-outline-secondary" data-testid="llm-role-edit">Edit</a>
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
        [] -> "(none)"
        names -> Text.intercalate ", " names
    enabledBadge = stateBadgeHtml role.enabled "llm-role"
    defaultBadge =
        if role.isDefault
            then [hsx|<span class="badge status-ack" data-testid="llm-role-default">default</span>|]
            else mempty
    toggleForm = inlinePostFormHtml (pathTo (ToggleLlmRoleAction roleId)) toggleLabel "btn btn-sm btn-outline-warning" (Just "llm-role-toggle") False
    toggleLabel :: Text
    toggleLabel = if role.enabled then "Disable" else "Enable"
    defaultForm =
        if role.isDefault || not role.enabled
            then mempty
            else inlinePostFormHtml (pathTo (SetDefaultLlmRoleAction roleId)) "Set default" "btn btn-sm btn-outline-primary" (Just "llm-role-set-default") False
    deleteForm =
        if role.isDefault
            then mempty
            else inlinePostFormHtml (pathTo (DeleteLlmRoleAction roleId)) "Delete" "btn btn-sm btn-outline-danger" (Just "llm-role-delete") True

decodeTools :: Aeson.Value -> [Text]
decodeTools value = fromMaybe [] (Aeson.decode (Aeson.encode value))

templateRowHtml :: TemplateRow -> Html
templateRowHtml row =
    [hsx|
    <tr data-testid="llm-template">
        <td>{row.name}</td>
        <td data-testid="llm-template-version">v{row.version}</td>
        <td>{activeBadge}</td>
        <td data-testid="llm-template-feedback">{row.feedbackScore} ({row.feedbackCount} votes)</td>
        <td>{fromMaybe "" row.notes}</td>
        <td>
            <a href={EditLlmTemplateAction row.templateId} class="btn btn-sm btn-outline-secondary" data-testid="llm-template-edit">Edit (new version)</a>
            {activateForm}
            {deleteForm}
        </td>
    </tr>
|]
  where
    activeBadge =
        if row.active
            then [hsx|<span class="badge status-resolved" data-testid="llm-template-active">active</span>|]
            else mempty
    activateForm =
        if row.active
            then mempty
            else inlinePostFormHtml (pathTo (ActivateLlmTemplateAction row.templateId)) "Activate" "btn btn-sm btn-outline-primary" (Just "llm-template-activate") False
    deleteForm =
        if row.active
            then mempty
            else inlinePostFormHtml (pathTo (DeleteLlmTemplateAction row.templateId)) "Delete" "btn btn-sm btn-outline-danger" (Just "llm-template-delete") False
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
