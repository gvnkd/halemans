module Web.View.LlmAdmin.NewProvider where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data NewProviderView = NewProviderView

instance View NewProviderView where
    beforeRender _ = setPageTitle "LLM"
    html NewProviderView =
        [hsx|
        {pageHeaderHtml (tr "New LLM provider") mempty}
        <p class="text-muted">
            {tr "API key env holds the NAME of the environment variable containing the key, not the key itself. Providers are created disabled; enable one from the list (enabling disables the others)."}
        </p>
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={CreateLlmProviderAction} data-testid="llm-provider-new-form">
            <div class="mb-3">
                <label class="form-label">{tr "Provider name"}</label>
                <input name="providerName" type="text" class="form-control" data-testid="llm-provider-name" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Endpoint"}</label>
                <input name="endpoint" type="text" class="form-control" placeholder="http://127.0.0.1:1234" data-testid="llm-provider-endpoint" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Model"}</label>
                <input name="model" type="text" class="form-control" data-testid="llm-provider-model" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "API key env var (optional)"}</label>
                <input name="apiKeyEnv" type="text" class="form-control" data-testid="llm-provider-api-key-env"/>
            </div>
            <div class="mb-3 form-check">
                <input name="toolsEnabled" type="checkbox" class="form-check-input" data-testid="llm-provider-tools"/>
                <label class="form-check-label">{tr "Tools (read-only)"}</label>
            </div>
            <button type="submit" class="btn btn-brand" data-testid="llm-provider-create">{tr "Create provider"}</button>
            <a href={LlmAdminAction} class="btn btn-ghost">{tr "Cancel"}</a>
        </form>
        </div></div>
    |]
