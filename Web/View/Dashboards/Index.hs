module Web.View.Dashboards.Index where

import Web.View.Fragments (emptyStateHtml, inlinePostFormHtml, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView {dashboards :: [Dashboard]}

instance View IndexView where
    beforeRender _ = setPageTitle (tr "Dashboards")
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml (tr "Dashboards") newButton}
        {tableOrEmpty}
    |]
      where
        tableOrEmpty =
            if null dashboards
                then emptyStateHtml "dashboards-empty" (tr "No dashboards yet — create one to organize your alerts.")
                else
                    [hsx|
        <table class="table" data-testid="dashboards-table">
            <thead>
                <tr><th>{tr "Name"}</th><th>{tr "Default"}</th><th>{tr "Position"}</th><th></th></tr>
            </thead>
            <tbody>
                {forEach dashboards renderRow}
            </tbody>
        </table>
                    |]
        newButton = [hsx|<a href={NewDashboardAction} class="btn btn-brand" data-testid="new-dashboard">{tr "New dashboard"}</a>|]

renderRow :: Dashboard -> Html
renderRow dashboard =
    [hsx|
    <tr data-testid="dashboard-row">
        <td><a href={ShowDashboardAction dashboard.id} data-testid="dashboard-link">{dashboard.name}</a> {protectedBadgeHtml (get #protected dashboard)}</td>
        <td>{defaultBadge}</td>
        <td>
            <form method="POST" action={MoveDashboardAction dashboard.id} class="d-inline" data-testid="dashboard-move-form">
                <input type="number" name="position" value={dashboard.position} class="form-control form-control-sm d-inline-block w-5rem" data-testid="dashboard-position"/>
                <button type="submit" class="btn btn-sm btn-ghost">{tr "Move"}</button>
            </form>
        </td>
        <td>
            {defaultButton}
            <a href={EditDashboardAction dashboard.id} class="btn btn-sm btn-ghost" data-testid="edit-dashboard">{tr "Edit"}</a>
            {inlinePostFormHtml (pathTo (DeleteDashboardAction dashboard.id)) (tr "Delete") "btn btn-sm btn-ghost btn-ghost-critical" (Just "delete-dashboard") True}
        </td>
    </tr>
|]
  where
    defaultBadge =
        if dashboard.isDefault
            then [hsx|<span class="badge status-ack" data-testid="dashboard-default">{tr "default"}</span>|]
            else mempty
    defaultButton =
        if dashboard.isDefault
            then mempty
            else inlinePostFormHtml (pathTo (SetDefaultDashboardAction dashboard.id)) (tr "Set default") "btn btn-sm btn-ghost" (Just "set-default-dashboard") False
