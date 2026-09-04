module Web.View.Alerts.Index where
import Web.View.Prelude
import Web.View.Fragments (alertRowHtml)

data IndexView = IndexView { alerts :: [Alert] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Alerts</h1>
        <table class="table" data-testid="alerts-table" data-live-scope="alerts">
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
            <tbody id="alerts-tbody">
                {forEach alerts alertRowHtml}
            </tbody>
        </table>
    |]
