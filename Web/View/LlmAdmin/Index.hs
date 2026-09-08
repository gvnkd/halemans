module Web.View.LlmAdmin.Index where
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
    , endpoint :: Maybe Text
    , model :: Maybe Text
    , toolsEnabled :: Bool
    , dailyBudget :: Int
    , rateLimit :: Int
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>LLM</h1>

        <h2>Provider</h2>
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
            <h2>Prompt templates</h2>
            <a href={NewLlmTemplateAction} class="btn btn-sm btn-primary" data-testid="new-llm-template">New template</a>
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
