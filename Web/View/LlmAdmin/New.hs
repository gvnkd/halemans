module Web.View.LlmAdmin.New where
import Web.View.Prelude

data NewView = NewView

instance View NewView where
    html NewView = [hsx|
        <h1>New prompt template</h1>
        <p class="text-muted">
            Placeholders: <code>{"{{alert.title}}" :: Text}</code> <code>{"{{alert.severity}}" :: Text}</code> <code>{"{{alert.env}}" :: Text}</code> <code>{"{{alert.host}}" :: Text}</code> <code>{"{{alert.service}}" :: Text}</code> <code>{"{{alert.check_name}}" :: Text}</code> <code>{"{{alert.description}}" :: Text}</code> <code>{"{{alert.labels}}" :: Text}</code> <code>{"{{alert.annotations}}" :: Text}</code> <code>{"{{events}}" :: Text}</code> <code>{"{{cmdb_excerpt}}" :: Text}</code> <code>{"{{similar_alerts}}" :: Text}</code> <code>{"{{jira_links}}" :: Text}</code>
        </p>
        <form method="POST" action={CreateLlmTemplateAction} data-testid="llm-template-new-form">
            <div class="mb-3">
                <label class="form-label">Name</label>
                <input name="name" type="text" class="form-control" data-testid="llm-template-name" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Version</label>
                <input name="version" type="number" class="form-control" value={1 :: Int} data-testid="llm-template-version-input" required="required"/>
            </div>
            <div class="mb-3 form-check">
                <input name="active" type="checkbox" class="form-check-input" data-testid="llm-template-active"/>
                <label class="form-check-label">Active</label>
            </div>
            <div class="mb-3">
                <label class="form-label">Body</label>
                <textarea name="body" class="form-control" rows="15" data-testid="llm-template-body" required="required">{"" :: Text}</textarea>
            </div>
            <div class="mb-3">
                <label class="form-label">Notes</label>
                <input name="notes" type="text" class="form-control" data-testid="llm-template-notes"/>
            </div>
            <button type="submit" class="btn btn-primary" data-testid="llm-template-create">Create template</button>
            <a href={LlmAdminAction} class="btn btn-outline-secondary">Cancel</a>
        </form>
    |]
