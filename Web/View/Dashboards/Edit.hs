module Web.View.Dashboards.Edit where

import Web.View.Dashboards.Form (dashboardFormFields)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data EditView = EditView
    { dashboard :: Dashboard
    , configText :: Text
    }

instance View EditView where
    beforeRender _ = setPageTitle (tr "Dashboards")
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit dashboard") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={UpdateDashboardAction dashboard.id} data-testid="dashboard-form">
            {dashboardFormFields dashboard.name configText dashboard.isDefault}
            <button type="submit" class="btn btn-brand" data-testid="dashboard-submit">{tr "Save"}</button>
        </form>
        </div></div>|]
