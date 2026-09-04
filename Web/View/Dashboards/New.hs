module Web.View.Dashboards.New where
import Web.View.Prelude
import Web.View.Dashboards.Form (dashboardFormFields)

data NewView = NewView

instance View NewView where
    html NewView = [hsx|
        <h1>New dashboard</h1>
        <form method="POST" action={CreateDashboardAction} data-testid="dashboard-form" style="max-width: 600px">
            {dashboardFormFields "" defaultConfig False}
            <button type="submit" class="btn btn-primary" data-testid="dashboard-submit">Create</button>
        </form>
    |]
        where
            defaultConfig :: Text
            defaultConfig = "[{\"env\":\"dev\",\"filters\":{\"status\":[],\"severity\":[]}}]"
