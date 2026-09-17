module Web.View.LlmAdmin.Edit where

import Application.Service.Llm.Prompt (templateSlotNames)
import Web.View.Prelude

data EditView = EditView
    { template :: LlmPromptTemplate
    }

instance View EditView where
    html EditView{..} =
        [hsx|
        <h1>{tr "Edit prompt template"}</h1>
        <p class="text-muted">
            {trp "{name} v{version} — saving creates v{nextVersion} (inactive until activated)." [("name", template.name), ("version", tshow template.version), ("nextVersion", tshow (template.version + 1))]}
            {tr "Placeholders:"} {forEach templateSlotNames placeholderChip}
        </p>
        <form method="POST" action={UpdateLlmTemplateAction (get #id template)} data-testid="llm-template-form">
            <div class="mb-3">
                <label class="form-label">{tr "Body"}</label>
                <textarea name="body" class="form-control" rows="15" data-testid="llm-template-body">{template.body}</textarea>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Notes"}</label>
                <input name="notes" type="text" class="form-control" value={fromMaybe "" template.notes} data-testid="llm-template-notes"/>
            </div>
            <button type="submit" class="btn btn-primary" data-testid="llm-template-save">{tr "Save as new version"}</button>
            <a href={LlmAdminAction} class="btn btn-outline-secondary">{tr "Cancel"}</a>
        </form>
    |]
      where
        placeholderChip name = [hsx|<code>{"{{" <> name <> "}}" :: Text}</code>|]
