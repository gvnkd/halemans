module Web.View.Dashboards.Edit where
import Web.View.Prelude
import Web.View.Dashboards.Form (dashboardFormFields)

data EditView = EditView
    { dashboard :: Dashboard
    , configText :: Text
    }

instance View EditView where
    html EditView { .. } = [hsx|
        <h1>Edit dashboard</h1>
        <form method="POST" action={UpdateDashboardAction dashboard.id} data-testid="dashboard-form" style="max-width: 600px">
            {dashboardFormFields dashboard.name configText dashboard.isDefault}
            <button type="submit" class="btn btn-primary" data-testid="dashboard-submit">Save</button>
        </form>
    |]
