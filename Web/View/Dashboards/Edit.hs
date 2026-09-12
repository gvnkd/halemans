module Web.View.Dashboards.Edit where

import Web.View.Dashboards.Form (dashboardFormFields)
import Web.View.Prelude

data EditView = EditView
    { dashboard :: Dashboard
    , configText :: Text
    }

instance View EditView where
    html EditView{..} =
        [hsx|
        <h1>Edit dashboard</h1>
        <form method="POST" action={UpdateDashboardAction dashboard.id} data-testid="dashboard-form" class="maxw-600">
            {dashboardFormFields dashboard.name configText dashboard.isDefault}
            <button type="submit" class="btn btn-primary" data-testid="dashboard-submit">Save</button>
        </form>
    |]
