module Web.Controller.LlmAdmin where

import Web.Controller.Prelude
import Web.View.LlmAdmin.Index
import Web.View.LlmAdmin.Queue
import Web.View.LlmAdmin.New
import Web.View.LlmAdmin.Edit
import Web.View.LlmAdmin.NewProvider
import Web.View.LlmAdmin.EditProvider
import Web.View.LlmAdmin.NewRole
import Web.View.LlmAdmin.EditRole
import Application.Job.LlmAnalysis (failAnalysis)
import Application.Service.Llm (LlmProviderConfig (..), connectionOk, apiUrl)
import Application.Service.Llm.DbConfig (currentLlmConfig)
import Application.Service.Llm.Roles (roleToolNames)
import qualified Application.Service.Llm.Budget as Budget
import qualified Application.Service.Log as Log
import IHP.TypedSql (sqlQueryTyped, sqlExecTyped, typedSql)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Functor ((<&>))
import Control.Monad (void)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)

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
        providers <- query @LlmConfig
            |> orderByAsc #providerName
            |> fetch
        roles <- query @LlmAgentRole
            |> orderByAsc #name
            |> fetch
        render IndexView { endpoint = (.endpoint) <$> maybeConfig, model = (.model) <$> maybeConfig
                         , toolsEnabled = maybe False (.toolsEnabled) maybeConfig, .. }

    action LlmQueueAction = do
        requirePrivilege "manage_rules"
        analyses <- query @LlmAnalysis
            |> filterWhereIn (#status, ["queued", "running"] :: [Text])
            |> orderByAsc #createdAt
            |> fetch
        queue <- forM analyses \analysis -> do
            alert <- fetch analysis.alertId
            let analysisRef = get #id analysis
            jobRows <- sqlQueryTyped [typedSql|
                SELECT id, status::text AS status, last_error, attempts_count,
                    created_at, updated_at, run_at, locked_at
                FROM llm_analysis_jobs
                WHERE analysis_id = ${analysisRef}
                ORDER BY created_at DESC
                LIMIT 1
            |]
            let maybeJob = case jobRows of
                    (row:_) -> Just row
                    [] -> Nothing
            pure QueueRow
                { analysisId = get #id analysis
                , alertId = get #id alert
                , alertTitle = alert.title
                , alertFingerprint = alert.fingerprint
                , analysisStatus = analysis.status
                , analysisError = analysis.errorMessage
                , analysisCreatedAt = analysis.createdAt
                , analysisUpdatedAt = analysis.updatedAt
                , jobId = fmap (get #id) maybeJob
                , jobStatus = maybe Nothing (get #status) maybeJob
                , jobLastError = maybe Nothing (get #last_error) maybeJob
                , jobAttempts = fmap (get #attempts_count) maybeJob
                , jobCreatedAt = fmap (get #created_at) maybeJob
                , jobUpdatedAt = fmap (get #updated_at) maybeJob
                , jobRunAt = fmap (get #run_at) maybeJob
                , jobLockedAt = maybe Nothing (get #locked_at) maybeJob
                }
        render QueueView { queue }

    action DropLlmAnalysisAction { analysisId } = do
        requirePrivilege "manage_rules"
        analysis <- fetch analysisId
        if analysis.status /= "queued"
            then setErrorMessage "Only queued LLM analyses can be dropped"
            else do
                alert <- fetch analysis.alertId
                withTransaction do
                    void do
                        sqlExecTyped [typedSql|
                            DELETE FROM llm_analysis_jobs
                            WHERE analysis_id = ${analysisId}
                                AND status IN ('job_status_not_started', 'job_status_retry')
                        |]
                    failAnalysis analysis alert "dropped by admin" "llm_failed"
                setSuccessMessage "LLM analysis dropped"
        redirectTo LlmQueueAction

    action NewLlmTemplateAction = do
        requirePrivilege "manage_rules"
        render NewView

    action CreateLlmTemplateAction = do
        requirePrivilege "manage_rules"
        let name = param @Text "name"
            version = param @Int "version"
            body = param @Text "body"
            notes = paramOrNothing @Text "notes"
            activate = paramOrNothing @Text "active" == Just "on"
        if Text.null name || version < 1 || Text.null body
            then do
                setErrorMessage "Name, positive version and body are required"
                redirectTo NewLlmTemplateAction
            else do
                existing <- query @LlmPromptTemplate
                    |> filterWhere (#name, name)
                    |> filterWhere (#version, version)
                    |> fetchOneOrNothing
                case existing of
                    Just _ -> do
                        setErrorMessage ("Prompt template " <> name <> " v" <> tshow version <> " already exists")
                        redirectTo NewLlmTemplateAction
                    Nothing -> do
                        _ <- if activate
                            then withTransaction do
                                void do
                                    sqlExecTyped [typedSql|
                                        UPDATE llm_prompt_templates SET active = false, updated_at = NOW()
                                        WHERE name = ${name}
                                    |]
                                newRecord @LlmPromptTemplate
                                    |> set #name name
                                    |> set #version version
                                    |> set #body body
                                    |> set #active True
                                    |> set #notes notes
                                    |> createRecord
                            else (newRecord @LlmPromptTemplate
                                |> set #name name
                                |> set #version version
                                |> set #body body
                                |> set #active False
                                |> set #notes notes
                                |> createRecord)
                        let state = if activate then " (active)" else " (inactive)"
                        setSuccessMessage ("Created " <> name <> " v" <> tshow version <> state)
                        redirectTo LlmAdminAction

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

    action DeleteLlmTemplateAction { templateId } = do
        requirePrivilege "manage_rules"
        template <- fetch templateId
        references <- query @LlmAnalysis
            |> filterWhere (#promptTemplateId, Just templateId)
            |> fetchCount
        if get #active template
            then setErrorMessage "Cannot delete the active prompt template version"
            else if references > 0
                then setErrorMessage "Cannot delete prompt template: analyses reference this version"
                else do
                    deleteRecord template
                    setSuccessMessage ("Deleted " <> get #name template <> " v" <> tshow template.version)
        redirectTo LlmAdminAction

    action TestLlmConnectionAction = do
        requirePrivilege "manage_rules"
        maybeConfig <- currentLlmConfig
        case maybeConfig of
            Nothing -> setErrorMessage "LLM not configured (no enabled llm_configs row, LLM_ENDPOINT/LLM_MODEL missing)"
            Just config -> do
                let url = apiUrl config "/v1/models"
                result <- connectionOk config
                case result of
                    Right () -> setSuccessMessage ("LLM reachable at " <> url <> " (model " <> config.model <> ")")
                    Left err -> do
                        let ?context = ?context.frameworkConfig
                        Log.logWarn ("llm connection test failed: " <> err)
                        setErrorMessage ("LLM endpoint " <> url <> " did not answer: " <> err)
        redirectTo LlmAdminAction

    action NewLlmProviderAction = do
        requirePrivilege "manage_rules"
        render NewProviderView

    action CreateLlmProviderAction = do
        requirePrivilege "manage_rules"
        let name = param @Text "providerName"
            endpoint = param @Text "endpoint"
            model = param @Text "model"
            apiKeyEnv = nonEmptyParam "apiKeyEnv"
            toolsEnabled = paramOrNothing @Text "toolsEnabled" == Just "on"
        if Text.null name || Text.null endpoint || Text.null model
            then do
                setErrorMessage "Provider name, endpoint and model are required"
                redirectTo NewLlmProviderAction
            else do
                existing <- query @LlmConfig
                    |> filterWhere (#providerName, name)
                    |> fetchOneOrNothing
                case existing of
                    Just _ -> do
                        setErrorMessage ("Provider " <> name <> " already exists")
                        redirectTo NewLlmProviderAction
                    Nothing -> do
                        _ <- newRecord @LlmConfig
                            |> set #providerName name
                            |> set #endpoint endpoint
                            |> set #model model
                            |> set #apiKeyEnv apiKeyEnv
                            |> set #toolsEnabled toolsEnabled
                            |> set #enabled False
                            |> createRecord
                        setSuccessMessage ("Created provider " <> name <> " (disabled — enable it from the list)")
                        redirectTo LlmAdminAction

    action EditLlmProviderAction { providerId } = do
        requirePrivilege "manage_rules"
        provider <- fetch providerId
        render EditProviderView { .. }

    action UpdateLlmProviderAction { providerId } = do
        requirePrivilege "manage_rules"
        provider <- fetch providerId
        let name = param @Text "providerName"
            endpoint = param @Text "endpoint"
            model = param @Text "model"
            apiKeyEnv = nonEmptyParam "apiKeyEnv"
            toolsEnabled = paramOrNothing @Text "toolsEnabled" == Just "on"
        if Text.null name || Text.null endpoint || Text.null model
            then do
                setErrorMessage "Provider name, endpoint and model are required"
                redirectTo (EditLlmProviderAction providerId)
            else do
                clash <- query @LlmConfig
                    |> filterWhere (#providerName, name)
                    |> fetchOneOrNothing
                case clash of
                    Just other | get #id other /= providerId -> do
                        setErrorMessage ("Provider " <> name <> " already exists")
                        redirectTo (EditLlmProviderAction providerId)
                    _ -> do
                        now <- getCurrentTime
                        _ <- provider
                            |> set #providerName name
                            |> set #endpoint endpoint
                            |> set #model model
                            |> set #apiKeyEnv apiKeyEnv
                            |> set #toolsEnabled toolsEnabled
                            |> set #updatedAt now
                            |> updateRecord
                        setSuccessMessage ("Updated provider " <> name)
                        redirectTo LlmAdminAction

    -- Enabled row is unique (llm_configs_enabled_idx): flip others off in the
    -- same transaction, mirroring template activation above.
    action EnableLlmProviderAction { providerId } = do
        requirePrivilege "manage_rules"
        provider <- fetch providerId
        withTransaction do
            void do
                sqlExecTyped [typedSql|
                    UPDATE llm_configs SET enabled = false, updated_at = NOW()
                |]
            void do
                sqlExecTyped [typedSql|
                    UPDATE llm_configs SET enabled = true, updated_at = NOW()
                    WHERE id = ${providerId}
                |]
        setSuccessMessage ("Enabled provider " <> get #providerName provider)
        redirectTo LlmAdminAction

    action DisableLlmProviderAction { providerId } = do
        requirePrivilege "manage_rules"
        provider <- fetch providerId
        void do
            sqlExecTyped [typedSql|
                UPDATE llm_configs SET enabled = false, updated_at = NOW()
                WHERE id = ${providerId}
            |]
        setSuccessMessage ("Disabled provider " <> get #providerName provider <> " (env config applies when no provider is enabled)")
        redirectTo LlmAdminAction

    -- Nothing references llm_configs (milestone_7.md §7): deletes are safe.
    action DeleteLlmProviderAction { providerId } = do
        requirePrivilege "manage_rules"
        provider <- fetch providerId
        deleteRecord provider
        setSuccessMessage ("Deleted provider " <> get #providerName provider)
        redirectTo LlmAdminAction

    -- Agent roles (milestone_8.md §7): name + prompt template + tool
    -- whitelist; set-default flips is_default transactionally (same
    -- unique-WHERE index trick as llm_configs_enabled_idx).
    action NewLlmRoleAction = do
        requirePrivilege "manage_rules"
        render NewRoleView

    action CreateLlmRoleAction = do
        requirePrivilege "manage_rules"
        let name = param @Text "name"
            description = param @Text "description"
            templateName = param @Text "promptTemplateName"
            tools = parseTools (param @Text "tools")
        if Text.null name || Text.null templateName
            then do
                setErrorMessage "Role name and prompt template name are required"
                redirectTo NewLlmRoleAction
            else do
                existing <- query @LlmAgentRole
                    |> filterWhere (#name, name)
                    |> fetchOneOrNothing
                case existing of
                    Just _ -> do
                        setErrorMessage ("Role " <> name <> " already exists")
                        redirectTo NewLlmRoleAction
                    Nothing -> do
                        _ <- newRecord @LlmAgentRole
                            |> set #name name
                            |> set #description description
                            |> set #promptTemplateName templateName
                            |> set #tools tools
                            |> set #enabled True
                            |> set #isDefault False
                            |> createRecord
                        setSuccessMessage ("Created role " <> name)
                        redirectTo LlmAdminAction

    action EditLlmRoleAction { roleId } = do
        requirePrivilege "manage_rules"
        role <- fetch roleId
        render EditRoleView { role, toolNames = roleToolNames role }

    action UpdateLlmRoleAction { roleId } = do
        requirePrivilege "manage_rules"
        role <- fetch roleId
        let name = param @Text "name"
            description = param @Text "description"
            templateName = param @Text "promptTemplateName"
            tools = parseTools (param @Text "tools")
        if Text.null name || Text.null templateName
            then do
                setErrorMessage "Role name and prompt template name are required"
                redirectTo (EditLlmRoleAction roleId)
            else do
                clash <- query @LlmAgentRole
                    |> filterWhere (#name, name)
                    |> fetchOneOrNothing
                case clash of
                    Just other | get #id other /= roleId -> do
                        setErrorMessage ("Role " <> name <> " already exists")
                        redirectTo (EditLlmRoleAction roleId)
                    _ -> do
                        now <- getCurrentTime
                        _ <- role
                            |> set #name name
                            |> set #description description
                            |> set #promptTemplateName templateName
                            |> set #tools tools
                            |> set #updatedAt now
                            |> updateRecord
                        setSuccessMessage ("Updated role " <> name)
                        redirectTo LlmAdminAction

    action ToggleLlmRoleAction { roleId } = do
        requirePrivilege "manage_rules"
        role <- fetch roleId
        now <- getCurrentTime
        -- Disabling the default role also drops the default flag, so
        -- resolution never lands on a disabled role.
        _ <- role
            |> set #enabled (not role.enabled)
            |> set #isDefault (role.isDefault && not role.enabled)
            |> set #updatedAt now
            |> updateRecord
        setSuccessMessage ((if role.enabled then "Disabled " else "Enabled ") <> "role " <> role.name)
        redirectTo LlmAdminAction

    action SetDefaultLlmRoleAction { roleId } = do
        requirePrivilege "manage_rules"
        role <- fetch roleId
        if not role.enabled
            then setErrorMessage "Enable the role before making it the default"
            else do
                withTransaction do
                    void do
                        sqlExecTyped [typedSql|
                            UPDATE llm_agent_roles SET is_default = false, updated_at = NOW()
                        |]
                    void do
                        sqlExecTyped [typedSql|
                            UPDATE llm_agent_roles SET is_default = true, updated_at = NOW()
                            WHERE id = ${roleId}
                        |]
                setSuccessMessage ("Default role: " <> role.name)
        redirectTo LlmAdminAction

    action DeleteLlmRoleAction { roleId } = do
        requirePrivilege "manage_rules"
        role <- fetch roleId
        references <- query @LlmAnalysis
            |> filterWhere (#agentRoleId, Just roleId)
            |> fetchCount
        if role.isDefault
            then setErrorMessage "Cannot delete the default role (set another default first)"
            else if references > 0
                then setErrorMessage ("Cannot delete role " <> role.name <> ": analyses reference it")
                else do
                    deleteRecord role
                    setSuccessMessage ("Deleted role " <> role.name)
        redirectTo LlmAdminAction

-- Tools field: comma-separated whitelist of known tool names; empty = no
-- tools for the role (milestone_8.md §2).
parseTools :: Text -> Value
parseTools raw = Aeson.toJSON
    [ name
    | name <- map Text.strip (Text.splitOn "," raw)
    , name `elem` knownToolNames
    ]

knownToolNames :: [Text]
knownToolNames = ["cmdb_lookup", "jira_search", "assets_lookup"]
