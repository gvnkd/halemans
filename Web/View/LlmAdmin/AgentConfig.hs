module Web.View.LlmAdmin.AgentConfig where

import Application.Service.Agent.Core (internalAgentTemplateName)
import Application.Service.Llm.AgentConfig (AgentBudgetConfig (..))
import Application.Service.Llm.GlobalConfig (GlobalBudgetConfig (..))
import Web.View.Fragments (emptyStateHtml, inlinePostFormHtml, pageHeaderHtml, sectionHeaderHtml)
import Web.View.Prelude

data AgentConfigView = AgentConfigView
    { agentBudget :: AgentBudgetConfig
    , globalBudget :: GlobalBudgetConfig
    , globalFromEnv :: Bool
    , template :: Maybe LlmPromptTemplate
    , agentUsage :: (Int64, Int64, Int64)
    , globalUsage :: (Int64, Int64, Int64)
    }

instance View AgentConfigView where
    html AgentConfigView{..} =
        [hsx|
    <div>
        {pageHeaderHtml (tr "Agent configuration") backLink}

        <div class="row g-4 mb-2">
            <div class="col-lg-6 d-flex flex-column">
                {sectionHeaderHtml (tr "Agent prompt") mempty}
                <div class="card flex-fill"><div class="card-body">
                    {templateBody}
                </div></div>
            </div>
            <div class="col-lg-6 d-flex flex-column">
                {sectionHeaderHtml (tr "Agent limits") mempty}
                <div class="card flex-fill"><div class="card-body">
                    <p class="text-muted">{trp "Used today by the embedded agent: {tokens} tokens ({requests} requests), budget {budget} tokens/day." [("tokens", tshow (agentTokensIn + agentTokensOut)), ("requests", tshow agentRequests), ("budget", tshow agentBudget.abcDailyTokenBudget)]}</p>
                    <form method="POST" action={UpdateAgentConfigAction} data-testid="agent-config-form">
                        <div class="row">
                            <div class="col-md-6 mb-2">
                                <label class="form-label">{tr "Daily token budget"}</label>
                                <input name="dailyTokenBudget" type="number" class="form-control" value={agentBudget.abcDailyTokenBudget} min="0" data-testid="agent-config-daily-budget"/>
                            </div>
                            <div class="col-md-6 mb-2">
                                <label class="form-label">{tr "Rate limit (requests/min)"}</label>
                                <input name="ratePerMinute" type="number" class="form-control" value={agentBudget.abcRatePerMinute} min="1" data-testid="agent-config-rate"/>
                            </div>
                        </div>
                        <button type="submit" class="btn btn-ghost" data-testid="agent-config-submit">{tr "Save"}</button>
                    </form>
                </div></div>
            </div>
        </div>

        {sectionHeaderHtml (tr "Global limits") mempty}
        <div class="card mb-4"><div class="card-body">
            <p class="text-muted">
                {trp "Used today across all LLM consumers: {tokens} tokens ({requests} requests), budget {budget} tokens/day." [("tokens", tshow (globalTokensIn + globalTokensOut)), ("requests", tshow globalRequests), ("budget", tshow globalBudget.gbcDailyTokenBudget)]}
                {if globalFromEnv then tr " (no saved row — the env defaults are in effect; saving pins them)" else mempty}
            </p>
            <form method="POST" action={UpdateGlobalConfigAction} data-testid="global-config-form">
                <div class="row">
                    <div class="col-md-4 mb-2">
                        <label class="form-label">{tr "Daily token budget (all consumers)"}</label>
                        <input name="dailyTokenBudget" type="number" class="form-control" value={globalBudget.gbcDailyTokenBudget} min="0" data-testid="global-config-daily-budget"/>
                    </div>
                    <div class="col-md-4 mb-2">
                        <label class="form-label">{tr "Rate limit (analysis requests/min)"}</label>
                        <input name="ratePerMinute" type="number" class="form-control" value={globalBudget.gbcRatePerMinute} min="1" data-testid="global-config-rate"/>
                    </div>
                </div>
                <button type="submit" class="btn btn-ghost" data-testid="global-config-submit">{tr "Save"}</button>
            </form>
        </div></div>
    </div>
    |]
      where
        backLink =
            [hsx|<a href={LlmAdminAction} class="btn btn-ghost" data-testid="agent-config-back">{tr "Back to LLM"}</a>|]
        (agentTokensIn, agentTokensOut, agentRequests) = agentUsage
        (globalTokensIn, globalTokensOut, globalRequests) = globalUsage
        templateBody = case template of
            Nothing ->
                [hsx|
                    {emptyStateHtml "agent-template-empty" (tr "No active internal_agent template — the built-in default prompt is in use.")}
                    {inlinePostFormHtml (pathTo SeedAgentTemplateAction) (tr "Create template from default") "btn btn-ghost" (Just "agent-template-seed") False}
                |]
            Just template ->
                [hsx|
                    <p class="text-muted">
                        {trp "Active template {name} v{version} (updated {updated}). Slots: {{user_name}}, {{user_email}}, {{language}}, {{current_page_url}}, {{current_page_title}}." [("name", template.name), ("version", tshow template.version), ("updated", tshow template.updatedAt)]}
                    </p>
                    <a href={EditLlmTemplateAction (get #id template)} class="btn btn-ghost" data-testid="agent-template-edit">{tr "Edit (new version)"}</a>
                |]
