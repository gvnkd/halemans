module Web.View.MattermostTemplates.Edit where

import Application.Service.Mattermost.Render (MattermostCardPreview, mattermostSlotNames)
import qualified Data.Text as Text
import Web.View.Fragments (inlinePostFormHtml, pageHeaderHtml)
import Web.View.MattermostTemplates.CardPreview (cardPreviewHtml)
import Web.View.MattermostTemplates.Index (partLabel)
import Web.View.Prelude

data EditView = EditView
    { name :: Text
    , latest :: Maybe LlmPromptTemplate
    , versions :: [LlmPromptTemplate]
    , draftBody :: Maybe Text
    , draftNotes :: Maybe Text
    , preview :: MattermostCardPreview
    }

instance View EditView where
    beforeRender _ = setPageTitle (tr "Mattermost card")
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (partLabel name <> " — " <> name) backLink}
        <p class="text-muted">
            {trp "Saving appends v{version} and activates it in one step. An empty body is allowed (e.g. an empty header line means no header at all)." [("version", tshow nextVersion)]}
            {tr "Placeholders:"} {forEach mattermostSlotNames placeholderChip}
        </p>
        <div class="row">
            <div class="col-lg-6">
                <div class="card"><div class="card-body">
                <form method="POST" action={UpdateMattermostTemplateAction} data-testid="mm-tpl-form">
                    <input type="hidden" name="name" value={name}/>
                    <div class="mb-3">
                        <label class="form-label">{tr "Body"}</label>
                        <textarea name="body" class="form-control" rows="14" data-testid="mm-tpl-body">{fromMaybe "" draftBody}</textarea>
                    </div>
                    <div class="mb-3">
                        <label class="form-label">{tr "Notes"}</label>
                        <input name="notes" type="text" class="form-control" value={fromMaybe "" draftNotes} data-testid="mm-tpl-notes"/>
                    </div>
                    <button type="submit" name="preview" value="1" class="btn btn-ghost" data-testid="mm-tpl-preview-btn">{tr "Preview"}</button>
                    <button type="submit" class="btn btn-brand" data-testid="mm-tpl-save">{tr "Save"}</button>
                    <a href={MattermostTemplatesAction} class="btn btn-ghost">{tr "Cancel"}</a>
                </form>
                <form method="POST" action={RestoreMattermostTemplateAction} class="mt-3" data-confirm={tr "Reset this part to the built-in default body? The current active version stays in the history."}>
                    <input type="hidden" name="name" value={name}/>
                    <button type="submit" class="btn btn-ghost" data-testid="mm-tpl-restore">{tr "Restore default"}</button>
                </form>
                </div></div>
            </div>
            <div class="col-lg-6">
                <h6>{tr "Card preview"}</h6>
                {cardPreviewHtml preview}
                <p class="text-muted small">{tr "The preview uses this part's draft body (the text area) and the active versions of the other four parts."}</p>

                <h6 class="mt-4">{tr "Version history"}</h6>
                <table class="table table-sm" data-testid="mm-tpl-versions">
                    <tbody>
                        {forEach versions versionRow}
                    </tbody>
                </table>
            </div>
        </div>
        |]
      where
        backLink = [hsx|<a href={MattermostTemplatesAction} class="btn btn-sm btn-ghost">{tr "Back to card templates"}</a>|]
        placeholderChip slotName = [hsx|<code>{"{{" <> slotName <> "}}" :: Text}</code>|]
        nextVersion = maybe (1 :: Int) (\t -> t.version + 1) latest
        versionRow template =
            [hsx|
            <tr data-testid={"mm-tpl-version-" <> tshow template.version}>
                <td>v{tshow template.version}</td>
                <td>{activeBadge}</td>
                <td class="text-muted small">{fromMaybe "" template.notes}</td>
                <td>{utcTimeHtml template.updatedAt}</td>
                <td>{activateForm}</td>
            </tr>
            |]
          where
            activeBadge =
                if template.active
                    then [hsx|<span class="badge text-bg-success">{tr "active"}</span>|]
                    else mempty
            activateForm =
                if not template.active
                    then
                        inlinePostFormHtml
                            (pathTo ActivateMattermostTemplateAction <> "?name=" <> name <> "&version=" <> tshow template.version)
                            (tr "Activate")
                            "btn btn-sm btn-ghost"
                            (Just "mm-tpl-activate")
                            False
                    else mempty
