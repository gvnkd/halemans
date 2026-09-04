module Web.View.Dashboards.Form (dashboardFormFields) where
import Web.View.Prelude

dashboardFormFields :: Text -> Text -> Bool -> Html
dashboardFormFields name config isDefault = [hsx|
    <div class="mb-3">
        <label class="form-label">Name</label>
        <input name="name" type="text" class="form-control" value={name} data-testid="dashboard-name" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Cards (JSON)</label>
        <textarea name="config" class="form-control font-monospace" rows="6" data-testid="dashboard-config">{config}</textarea>
        <div class="form-text">Ordered list of cards, e.g. {exampleConfig}. Empty filter lists match everything.</div>
    </div>
    <div class="mb-3 form-check">
        <input name="isDefault" type="checkbox" class="form-check-input" checked={isDefault} data-testid="dashboard-is-default"/>
        <label class="form-check-label">Default dashboard (landing page)</label>
    </div>
|]
    where
        exampleConfig :: Text
        exampleConfig = "[{\"env\": \"dev\", \"filters\": {\"status\": [\"firing\"], \"severity\": [\"critical\"]}}]"
