module Web.View.Dashboards.Index where
import Web.View.Prelude
import Web.View.Fragments (pageHeaderHtml, inlinePostFormHtml)

data IndexView = IndexView { dashboards :: [Dashboard] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {pageHeaderHtml "Dashboards" newButton}
        <table class="table" data-testid="dashboards-table">
            <thead>
                <tr><th>Name</th><th>Default</th><th>Position</th><th></th></tr>
            </thead>
            <tbody>
                {forEach dashboards renderRow}
            </tbody>
        </table>
    |]
        where
            newButton = [hsx|<a href={NewDashboardAction} class="btn btn-sm btn-primary" data-testid="new-dashboard">New dashboard</a>|]

renderRow :: Dashboard -> Html
renderRow dashboard = [hsx|
    <tr data-testid="dashboard-row">
        <td><a href={ShowDashboardAction dashboard.id} data-testid="dashboard-link">{dashboard.name}</a></td>
        <td>{defaultBadge}</td>
        <td>
            <form method="POST" action={MoveDashboardAction dashboard.id} class="d-inline" data-testid="dashboard-move-form">
                <input type="number" name="position" value={dashboard.position} class="form-control form-control-sm d-inline-block w-5rem" data-testid="dashboard-position"/>
                <button type="submit" class="btn btn-sm btn-outline-secondary">Move</button>
            </form>
        </td>
        <td>
            {defaultButton}
            <a href={EditDashboardAction dashboard.id} class="btn btn-sm btn-outline-primary" data-testid="edit-dashboard">Edit</a>
            {inlinePostFormHtml (pathTo (DeleteDashboardAction dashboard.id)) "Delete" "btn btn-sm btn-outline-danger" (Just "delete-dashboard") True}
        </td>
    </tr>
|]
    where
        defaultBadge = if dashboard.isDefault
            then [hsx|<span class="badge status-ack" data-testid="dashboard-default">default</span>|]
            else mempty
        defaultButton = if dashboard.isDefault
            then mempty
            else inlinePostFormHtml (pathTo (SetDefaultDashboardAction dashboard.id)) "Set default" "btn btn-sm btn-outline-secondary" (Just "set-default-dashboard") False
