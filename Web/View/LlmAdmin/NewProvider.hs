module Web.View.LlmAdmin.NewProvider where
import Web.View.Prelude

data NewProviderView = NewProviderView

instance View NewProviderView where
    html NewProviderView = [hsx|
        <h1>New LLM provider</h1>
        <p class="text-muted">
            API key env holds the NAME of the environment variable containing the key, not the key itself.
            Providers are created disabled; enable one from the list (enabling disables the others).
        </p>
        <form method="POST" action={CreateLlmProviderAction} data-testid="llm-provider-new-form">
            <div class="mb-3">
                <label class="form-label">Provider name</label>
                <input name="providerName" type="text" class="form-control" data-testid="llm-provider-name" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Endpoint</label>
                <input name="endpoint" type="text" class="form-control" placeholder="http://127.0.0.1:1234" data-testid="llm-provider-endpoint" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Model</label>
                <input name="model" type="text" class="form-control" data-testid="llm-provider-model" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">API key env var (optional)</label>
                <input name="apiKeyEnv" type="text" class="form-control" data-testid="llm-provider-api-key-env"/>
            </div>
            <div class="mb-3 form-check">
                <input name="toolsEnabled" type="checkbox" class="form-check-input" data-testid="llm-provider-tools"/>
                <label class="form-check-label">Tools (read-only)</label>
            </div>
            <button type="submit" class="btn btn-primary" data-testid="llm-provider-create">Create provider</button>
            <a href={LlmAdminAction} class="btn btn-outline-secondary">Cancel</a>
        </form>
    |]
