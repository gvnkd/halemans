module Web.Controller.LlmAdmin where

import Web.Controller.Prelude
import Web.View.LlmAdmin.Index
import Web.View.LlmAdmin.Edit
import Application.Service.Llm (LlmProviderConfig (..), connectionOk)
import Application.Service.Llm.DbConfig (currentLlmConfig)
import qualified Application.Service.Llm.Budget as Budget
import IHP.TypedSql (sqlQueryTyped, sqlExecTyped, typedSql)
import Data.Functor ((<&>))
import Control.Monad (void)

-- Admin → LLM page (design_docs/milestone_4.md §7): prompt template
-- list/edit/new-version with transactional active-flip, aggregate feedback
-- per template, budget counters, config display and a connection test.
instance Controller LlmAdminController where
    beforeAction = ensureIsUser

    action LlmAdminAction = do
        requirePrivilege "manage_rules"
        templateRows <- sqlQueryTyped [typedSql|
            SELECT t.id, t.name, t.version, t.active, t.notes,
                COALESCE(SUM(f.score), 0) AS feedback_score,
                COUNT(f.id) AS feedback_count
            FROM llm_prompt_templates t
            LEFT JOIN llm_analyses a ON a.prompt_template_id = t.id
            LEFT JOIN llm_feedback f ON f.analysis_id = a.id
            GROUP BY t.id, t.name, t.version, t.active, t.notes
            ORDER BY t.name, t.version DESC
        |]
        let templates = templateRows <&> \row -> TemplateRow
                { templateId = get #id row
                , name = get #name row
                , version = get #version row
                , active = get #active row
                , notes = get #notes row
                , feedbackScore = get #feedback_score row
                , feedbackCount = get #feedback_count row
                }
        counterRows <- sqlQueryTyped [typedSql|
            SELECT provider, day, tokens_in, tokens_out, requests
            FROM llm_budget_counters
            ORDER BY day DESC, provider
            LIMIT 14
        |]
        let counters = counterRows <&> \row -> CounterRow
                { provider = get #provider row
                , day = get #day row
                , tokensIn = get #tokens_in row
                , tokensOut = get #tokens_out row
                , requests = get #requests row
                }
        maybeConfig <- currentLlmConfig
        dailyBudget <- Budget.dailyTokenBudget
        rateLimit <- Budget.rateLimitPerMinute
        render IndexView { endpoint = (.endpoint) <$> maybeConfig, model = (.model) <$> maybeConfig
                         , toolsEnabled = maybe False (.toolsEnabled) maybeConfig, .. }

    action EditLlmTemplateAction { templateId } = do
        requirePrivilege "manage_rules"
        template <- fetch templateId
        render EditView { .. }

    -- Editing a template always creates version+1 (append-only lineage,
    -- milestone_4.md §12); activation is a separate explicit action.
    action UpdateLlmTemplateAction { templateId } = do
        requirePrivilege "manage_rules"
        template <- fetch templateId
        let body = param @Text "body"
            notes = paramOrNothing @Text "notes"
        _ <- newRecord @LlmPromptTemplate
            |> set #name (get #name template)
            |> set #version (template.version + 1)
            |> set #body body
            |> set #active False
            |> set #notes notes
            |> createRecord
        setSuccessMessage ("Created " <> get #name template <> " v" <> tshow (template.version + 1) <> " (inactive — activate it from the list)")
        redirectTo LlmAdminAction

    -- Activate flips the partial-unique active row for this template name
    -- transactionally (milestone_4.md §7).
    action ActivateLlmTemplateAction { templateId } = do
        requirePrivilege "manage_rules"
        template <- fetch templateId
        let templateName = get #name template
            templateRef = get #id template
        withTransaction do
            void do
                sqlExecTyped [typedSql|
                    UPDATE llm_prompt_templates SET active = false, updated_at = NOW()
                    WHERE name = ${templateName}
                |]
            void do
                sqlExecTyped [typedSql|
                    UPDATE llm_prompt_templates SET active = true, updated_at = NOW()
                    WHERE id = ${templateRef}
                |]
        setSuccessMessage ("Activated " <> get #name template <> " v" <> tshow template.version)
        redirectTo LlmAdminAction

    action TestLlmConnectionAction = do
        requirePrivilege "manage_rules"
        maybeConfig <- currentLlmConfig
        case maybeConfig of
            Nothing -> setErrorMessage "LLM not configured (no enabled llm_configs row, LLM_ENDPOINT/LLM_MODEL missing)"
            Just config -> do
                ok <- connectionOk config
                if ok
                    then setSuccessMessage ("LLM reachable at " <> config.endpoint <> " (model " <> config.model <> ")")
                    else setErrorMessage ("LLM endpoint " <> config.endpoint <> " did not answer /v1/models")
        redirectTo LlmAdminAction
