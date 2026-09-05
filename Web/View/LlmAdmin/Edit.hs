module Web.View.LlmAdmin.Edit where
import Web.View.Prelude

data EditView = EditView
    { template :: LlmPromptTemplate
    }

instance View EditView where
    html EditView { .. } = [hsx|
        <h1>Edit prompt template</h1>
        <p class="text-muted">
            {template.name} v{template.version} — saving creates v{template.version + 1} (inactive until activated).
            Placeholders: <code>{"{{alert.title}}" :: Text}</code> <code>{"{{alert.severity}}" :: Text}</code> <code>{"{{alert.env}}" :: Text}</code> <code>{"{{alert.host}}" :: Text}</code> <code>{"{{alert.service}}" :: Text}</code> <code>{"{{alert.check_name}}" :: Text}</code> <code>{"{{alert.description}}" :: Text}</code> <code>{"{{alert.labels}}" :: Text}</code> <code>{"{{alert.annotations}}" :: Text}</code> <code>{"{{events}}" :: Text}</code> <code>{"{{cmdb_excerpt}}" :: Text}</code> <code>{"{{similar_alerts}}" :: Text}</code> <code>{"{{jira_links}}" :: Text}</code>
        </p>
        <form method="POST" action={UpdateLlmTemplateAction (get #id template)} data-testid="llm-template-form">
            <div class="mb-3">
                <label class="form-label">Body</label>
                <textarea name="body" class="form-control" rows="15" data-testid="llm-template-body">{template.body}</textarea>
            </div>
            <div class="mb-3">
                <label class="form-label">Notes</label>
                <input name="notes" type="text" class="form-control" value={fromMaybe "" template.notes} data-testid="llm-template-notes"/>
            </div>
            <button type="submit" class="btn btn-primary" data-testid="llm-template-save">Save as new version</button>
            <a href={LlmAdminAction} class="btn btn-outline-secondary">Cancel</a>
        </form>
    |]
