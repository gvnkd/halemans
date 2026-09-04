module Web.View.Blackouts.Index where
import Web.View.Prelude

data IndexView = IndexView { blackouts :: [(Blackout, Text)] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex justify-content-between align-items-center">
            <h1>Blackouts</h1>
            <a href={NewBlackoutAction} class="btn btn-sm btn-primary" data-testid="new-blackout">New blackout</a>
        </div>
        <table class="table" data-testid="blackouts-table">
            <thead>
                <tr>
                    <th>Scope</th>
                    <th>Starts</th>
                    <th>Ends</th>
                    <th>Reason</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach blackouts renderBlackout}
            </tbody>
        </table>
    |]

renderBlackout :: (Blackout, Text) -> Html
renderBlackout (blackout, scopeName) = [hsx|
    <tr data-testid="blackout-row">
        <td>{scopeName}</td>
        <td>{show blackout.startsAt}</td>
        <td>{show blackout.endsAt}</td>
        <td>{blackout.reason}</td>
        <td>
            <a href={EditBlackoutAction blackout.id} class="btn btn-sm btn-outline-secondary" data-testid="edit-blackout">Edit</a>
            <form method="POST" action={DeleteBlackoutAction blackout.id} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-danger">Delete</button>
            </form>
        </td>
    </tr>
|]
