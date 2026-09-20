module Web.View.Dashboards.New where

import Web.View.Dashboards.Form (dashboardFormFields)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data NewView = NewView

instance View NewView where
    html NewView =
        [hsx|
        {pageHeaderHtml (tr "New dashboard") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={CreateDashboardAction} data-testid="dashboard-form">
            {dashboardFormFields "" defaultConfig False}
            <button type="submit" class="btn btn-brand" data-testid="dashboard-submit">{tr "Create"}</button>
        </form>
        </div></div>|]
      where
        defaultConfig :: Text
        defaultConfig = "[{\"env\":\"dev\",\"filters\":{\"status\":[],\"severity\":[]}}]"
