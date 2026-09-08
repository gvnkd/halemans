module Web.View.LlmAdmin.RoleForm (roleFormFields) where
import Web.View.Prelude
import qualified Data.Text as Text

roleFormFields :: Maybe LlmAgentRole -> [Text] -> Html
roleFormFields role toolNames = [hsx|
    <div class="mb-3">
        <label class="form-label">Name</label>
        <input name="name" type="text" class="form-control" value={field (.name)} data-testid="llm-role-name" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Description</label>
        <input name="description" type="text" class="form-control" value={field (.description)} data-testid="llm-role-description"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Prompt template name</label>
        <input name="promptTemplateName" type="text" class="form-control" value={templateName} data-testid="llm-role-template" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Tools (comma-separated whitelist)</label>
        <input name="tools" type="text" class="form-control" value={toolsText} data-testid="llm-role-tools"/>
    </div>
|]
    where
        field :: (LlmAgentRole -> Text) -> Text
        field getter = maybe "" getter role
        templateName :: Text
        templateName = maybe "alert_enrichment" (.promptTemplateName) role
        toolsText :: Text
        toolsText = Text.intercalate ", " toolNames
