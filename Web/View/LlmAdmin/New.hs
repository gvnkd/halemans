module Web.View.LlmAdmin.New where

import Application.Service.Llm.Prompt (templateSlotNames)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data NewView = NewView

instance View NewView where
    html NewView =
        [hsx|
        {pageHeaderHtml (tr "New prompt template") mempty}
        <p class="text-muted">
            {tr "Placeholders:"} {forEach templateSlotNames placeholderChip}
        </p>
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={CreateLlmTemplateAction} data-testid="llm-template-new-form">
            <div class="mb-3">
                <label class="form-label">{tr "Name"}</label>
                <input name="name" type="text" class="form-control" data-testid="llm-template-name" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Version"}</label>
                <input name="version" type="number" class="form-control" value={1 :: Int} data-testid="llm-template-version-input" required="required"/>
            </div>
            <div class="mb-3 form-check">
                <input name="active" type="checkbox" class="form-check-input" data-testid="llm-template-active"/>
                <label class="form-check-label">{tr "Active"}</label>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Body"}</label>
                <textarea name="body" class="form-control" rows="15" data-testid="llm-template-body" required="required">{"" :: Text}</textarea>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Notes"}</label>
                <input name="notes" type="text" class="form-control" data-testid="llm-template-notes"/>
            </div>
            <button type="submit" class="btn btn-brand" data-testid="llm-template-create">{tr "Create template"}</button>
            <a href={LlmAdminAction} class="btn btn-ghost">{tr "Cancel"}</a>
        </form>
        </div></div>
    |]
      where
        placeholderChip name = [hsx|<code>{"{{" <> name <> "}}" :: Text}</code>|]
