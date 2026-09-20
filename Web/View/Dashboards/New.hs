module Web.View.Dashboards.New where

import Web.View.Dashboards.Form (dashboardFormFields)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data NewView = NewView

instance View NewView where
    html NewView =
        [hsx|
        {pageHeaderHtml (tr "New dashboard") mempty}
        <form method="POST" action={CreateDashboardAction} data-testid="dashboard-form" class="maxw-600">
            {dashboardFormFields "" defaultConfig False}
            <button type="submit" class="btn btn-primary" data-testid="dashboard-submit">{tr "Create"}</button>
        </form>
    |]
      where
        defaultConfig :: Text
        defaultConfig = "[{\"env\":\"dev\",\"filters\":{\"status\":[],\"severity\":[]}}]"
