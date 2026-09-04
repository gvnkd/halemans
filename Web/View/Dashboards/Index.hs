module Web.View.Dashboards.Index where
import Web.View.Prelude

data IndexView = IndexView { dashboards :: [Dashboard] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Dashboards</h1>
        <p><a href={NewDashboardAction} class="btn btn-sm btn-primary" data-testid="new-dashboard">New dashboard</a></p>
        <table class="table" data-testid="dashboards-table">
            <thead>
                <tr><th>Name</th><th>Default</th><th>Position</th><th></th></tr>
            </thead>
            <tbody>
                {forEach dashboards renderRow}
            </tbody>
        </table>
    |]

renderRow :: Dashboard -> Html
renderRow dashboard = [hsx|
    <tr data-testid="dashboard-row">
        <td><a href={ShowDashboardAction dashboard.id} data-testid="dashboard-link">{dashboard.name}</a></td>
        <td>{defaultBadge}</td>
        <td>
            <form method="POST" action={MoveDashboardAction dashboard.id} class="d-inline" data-testid="dashboard-move-form">
                <input type="number" name="position" value={dashboard.position} class="form-control form-control-sm d-inline-block" style="width: 5rem" data-testid="dashboard-position"/>
                <button type="submit" class="btn btn-sm btn-outline-secondary">Move</button>
            </form>
        </td>
        <td>
            {defaultButton}
            <a href={EditDashboardAction dashboard.id} class="btn btn-sm btn-outline-primary" data-testid="edit-dashboard">Edit</a>
            <form method="POST" action={DeleteDashboardAction dashboard.id} class="d-inline js-delete">
                <button type="submit" class="btn btn-sm btn-outline-danger" data-testid="delete-dashboard">Delete</button>
            </form>
        </td>
    </tr>
|]
    where
        defaultBadge = if dashboard.isDefault
            then [hsx|<span class="badge status-ack" data-testid="dashboard-default">default</span>|]
            else mempty
        defaultButton = if dashboard.isDefault
            then mempty
            else [hsx|
                <form method="POST" action={SetDefaultDashboardAction dashboard.id} class="d-inline">
                    <button type="submit" class="btn btn-sm btn-outline-secondary" data-testid="set-default-dashboard">Set default</button>
                </form>
            |]
