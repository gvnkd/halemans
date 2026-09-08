module Web.View.LlmAdmin.Index where
import Web.View.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

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
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>LLM</h1>

        <h2>Effective configuration</h2>
        <table class="table" style="max-width: 700px" data-testid="llm-config">
            <tbody>
                <tr><td>Endpoint</td><td>{fromMaybe "-" endpoint}</td></tr>
                <tr><td>Model</td><td>{fromMaybe "-" model}</td></tr>
                <tr><td>Tools (read-only)</td><td>{if toolsEnabled then "enabled" else "disabled" :: Text}</td></tr>
                <tr><td>Daily token budget</td><td data-testid="llm-daily-budget">{dailyBudget}</td></tr>
                <tr><td>Rate limit</td><td data-testid="llm-rate-limit">{rateLimit} requests/min</td></tr>
            </tbody>
        </table>
        <form method="POST" action={TestLlmConnectionAction} class="d-inline">
            <button type="submit" class="btn btn-sm btn-outline-primary" data-testid="test-llm">Test connection</button>
        </form>

        <div class="d-flex justify-content-between align-items-center mt-4">
            <h2>Providers</h2>
            <a href={NewLlmProviderAction} class="btn btn-sm btn-primary" data-testid="new-llm-provider">New provider</a>
        </div>
        <table class="table" data-testid="llm-providers">
            <thead>
                <tr><th>Name</th><th>Endpoint</th><th>Model</th><th>API key env</th><th>Tools</th><th>Enabled</th><th></th></tr>
            </thead>
            <tbody>
                {forEach providers providerRowHtml}
            </tbody>
        </table>

        <div class="d-flex justify-content-between align-items-center mt-4">
            <h2>Agent roles</h2>
            <a href={NewLlmRoleAction} class="btn btn-sm btn-primary" data-testid="new-llm-role">New role</a>
        </div>
        <table class="table" data-testid="llm-roles">
            <thead>
                <tr><th>Name</th><th>Template</th><th>Tools</th><th>Enabled</th><th>Default</th><th></th></tr>
            </thead>
            <tbody>
                {forEach roles roleRowHtml}
            </tbody>
        </table>

        <div class="d-flex justify-content-between align-items-center mt-4">
            <h2>Prompt templates</h2>
            <div>
                <a href={LlmQueueAction} class="btn btn-sm btn-outline-secondary" data-testid="llm-queue-link">Queue</a>
                <a href={NewLlmTemplateAction} class="btn btn-sm btn-primary" data-testid="new-llm-template">New template</a>
            </div>
        </div>
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

providerRowHtml :: LlmConfig -> Html
providerRowHtml provider = [hsx|
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
        enabledBadge = if provider.enabled
            then [hsx|<span class="badge status-resolved" data-testid="llm-provider-enabled">enabled</span>|]
            else mempty
        toggleForm = if provider.enabled
            then [hsx|
                <form method="POST" action={DisableLlmProviderAction providerId} class="d-inline">
                    <button type="submit" class="btn btn-sm btn-outline-warning" data-testid="llm-provider-disable">Disable</button>
                </form>
            |]
            else [hsx|
                <form method="POST" action={EnableLlmProviderAction providerId} class="d-inline">
                    <button type="submit" class="btn btn-sm btn-outline-primary" data-testid="llm-provider-enable">Enable</button>
                </form>
            |]
        deleteForm = [hsx|
            <form method="POST" action={DeleteLlmProviderAction providerId} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-danger" data-testid="llm-provider-delete">Delete</button>
            </form>
        |]

roleRowHtml :: LlmAgentRole -> Html
roleRowHtml role = [hsx|
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
        enabledBadge = if role.enabled
            then [hsx|<span class="badge status-resolved" data-testid="llm-role-enabled">enabled</span>|]
            else [hsx|<span class="badge" data-testid="llm-role-disabled">disabled</span>|]
        defaultBadge = if role.isDefault
            then [hsx|<span class="badge status-ack" data-testid="llm-role-default">default</span>|]
            else mempty
        toggleForm = [hsx|
            <form method="POST" action={ToggleLlmRoleAction roleId} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-warning" data-testid="llm-role-toggle">{toggleLabel}</button>
            </form>
        |]
        toggleLabel :: Text
        toggleLabel = if role.enabled then "Disable" else "Enable"
        defaultForm = if role.isDefault || not role.enabled
            then mempty
            else [hsx|
                <form method="POST" action={SetDefaultLlmRoleAction roleId} class="d-inline">
                    <button type="submit" class="btn btn-sm btn-outline-primary" data-testid="llm-role-set-default">Set default</button>
                </form>
            |]
        deleteForm = if role.isDefault
            then mempty
            else [hsx|
                <form method="POST" action={DeleteLlmRoleAction roleId} class="d-inline js-delete">
                    <button type="submit" class="btn btn-sm btn-outline-danger" data-testid="llm-role-delete">Delete</button>
                </form>
            |]

decodeTools :: Aeson.Value -> [Text]
decodeTools value = fromMaybe [] (Aeson.decode (Aeson.encode value))

templateRowHtml :: TemplateRow -> Html
templateRowHtml row = [hsx|
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
        activeBadge = if row.active
            then [hsx|<span class="badge status-resolved" data-testid="llm-template-active">active</span>|]
            else mempty
        activateForm = if row.active
            then mempty
            else [hsx|
                <form method="POST" action={ActivateLlmTemplateAction row.templateId} class="d-inline">
                    <button type="submit" class="btn btn-sm btn-outline-primary" data-testid="llm-template-activate">Activate</button>
                </form>
            |]
        deleteForm = if row.active
            then mempty
            else [hsx|
                <form method="POST" action={DeleteLlmTemplateAction row.templateId} class="d-inline">
                    <button type="submit" class="btn btn-sm btn-outline-danger" data-testid="llm-template-delete">Delete</button>
                </form>
            |]

counterRowHtml :: CounterRow -> Html
counterRowHtml row = [hsx|
    <tr data-testid="llm-counter">
        <td>{row.provider}</td>
        <td>{show row.day}</td>
        <td>{row.tokensIn}</td>
        <td>{row.tokensOut}</td>
        <td>{row.requests}</td>
    </tr>
|]
