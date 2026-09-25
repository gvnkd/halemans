module Web.View.LlmAdmin.EditProvider where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data EditProviderView = EditProviderView
    { provider :: LlmConfig
    }

instance View EditProviderView where
    beforeRender _ = setPageTitle (tr "Edit LLM provider")
    html EditProviderView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit LLM provider") mempty}
        <p class="text-muted">
            {trp "{name} is {state}. Enabling/disabling happens from the provider list." [("name", provider.providerName), ("state", stateText)]}
        </p>
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={UpdateLlmProviderAction (get #id provider)} data-testid="llm-provider-form">
            <div class="mb-3">
                <label class="form-label">{tr "Provider name"}</label>
                <input name="providerName" type="text" class="form-control" value={provider.providerName} data-testid="llm-provider-name" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Endpoint"}</label>
                <input name="endpoint" type="text" class="form-control" value={provider.endpoint} data-testid="llm-provider-endpoint" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Model"}</label>
                <input name="model" type="text" class="form-control" value={provider.model} data-testid="llm-provider-model" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "API key env var (optional)"}</label>
                <input name="apiKeyEnv" type="text" class="form-control" value={fromMaybe "" provider.apiKeyEnv} data-testid="llm-provider-api-key-env"/>
            </div>
            <div class="mb-3 form-check">
                <input name="toolsEnabled" type="checkbox" class="form-check-input" checked={provider.toolsEnabled} data-testid="llm-provider-tools"/>
                <label class="form-check-label">{tr "Tools (read-only)"}</label>
            </div>
            <button type="submit" class="btn btn-brand" data-testid="llm-provider-save">{tr "Save provider"}</button>
            <a href={LlmAdminAction} class="btn btn-ghost">{tr "Cancel"}</a>
        </form>
        </div></div>
    |]
      where
        stateText :: Text
        stateText = if provider.enabled then tr "enabled" else tr "disabled"
