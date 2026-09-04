module Web.View.Groups.Show where
import Web.View.Prelude
import Web.View.Fragments (alertRowHtml, groupHeaderHtml)

data ShowView = ShowView
    { group :: AlertGroup
    , members :: [Alert]
    , canAck :: Bool
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div data-live-scope={"group:" <> tshow group.id}>
            {groupHeaderHtml group}
            {ackButton}
            <table class="table" data-testid="group-members-table">
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
                <tbody id="group-members-tbody">
                    {forEach members alertRowHtml}
                </tbody>
            </table>
        </div>
    |]
        where
            hasFiring = any (\alert -> alert.status == "firing") members
            ackButton = if canAck && hasFiring
                then [hsx|
                    <form method="POST" action={AckGroupAction group.id} class="mb-3">
                        <button type="submit" class="btn btn-sm btn-warning" data-testid="ack-group">Ack all firing</button>
                    </form>
                |]
                else mempty
