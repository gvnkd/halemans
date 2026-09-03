module Web.View.Alerts.Index where
import Web.View.Prelude

data IndexView = IndexView { alerts :: [Alert] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Alerts</h1>
        <table class="table" data-testid="alerts-table">
            <thead>
                <tr>
                    <th>Status</th>
                    <th>Severity</th>
                    <th>Title</th>
                    <th>Env</th>
                    <th>Host</th>
                    <th>Occurrences</th>
                    <th>Last seen</th>
                </tr>
            </thead>
            <tbody>
                {forEach alerts renderAlertRow}
            </tbody>
        </table>
    |]

renderAlertRow :: Alert -> Html
renderAlertRow alert = [hsx|
    <tr data-fingerprint={alert.fingerprint}>
        <td>{alert.status}</td>
        <td>{alert.severity}</td>
        <td><a href={ShowAlertAction alert.id}>{alert.title}</a></td>
        <td>{fromMaybe "" alert.env}</td>
        <td>{fromMaybe "" alert.host}</td>
        <td>{alert.occurrences}</td>
        <td>{show alert.lastSeenAt}</td>
    </tr>
|]
