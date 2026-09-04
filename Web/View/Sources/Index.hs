module Web.View.Sources.Index where
import Web.View.Prelude

data IndexView = IndexView
    { sources :: [Source]
    , canManage :: Bool
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex justify-content-between align-items-center">
            <h1>Sources</h1>
            {newButton}
        </div>
        <table class="table" data-testid="sources-table">
            <thead>
                <tr>
                    <th>Name</th>
                    <th>Type</th>
                    <th>Base URL</th>
                    <th>Env</th>
                    <th>Enabled</th>
                    <th>Poll interval</th>
                    <th>Last sync</th>
                    {actionsHeader}
                </tr>
            </thead>
            <tbody>
                {forEach sources (renderSourceRow canManage)}
            </tbody>
        </table>
    |]
        where
            newButton = if canManage
                then [hsx|<a href={NewSourceAction} class="btn btn-sm btn-primary" data-testid="new-source">New source</a>|]
                else mempty
            actionsHeader = if canManage
                then [hsx|<th></th>|]
                else mempty

renderSourceRow :: Bool -> Source -> Html
renderSourceRow canManage source =
    let sourceType = get #type_ source :: Text
        lastSync = maybe "never" (cs . show) source.lastSyncCursor :: Text
    in [hsx|
    <tr data-source-type={sourceType} data-testid="source-row">
        <td>{source.name}</td>
        <td>{sourceType}</td>
        <td>{source.baseUrl}</td>
        <td>{source.env}</td>
        <td>{enabledBadge}</td>
        <td>{source.pollIntervalSeconds}s</td>
        <td data-testid="source-last-sync">{lastSync}</td>
        {actions}
    </tr>
|]
    where
        enabledBadge = if source.enabled
            then [hsx|<span class="badge bg-success">enabled</span>|]
            else [hsx|<span class="badge bg-secondary">disabled</span>|]
        actions = if not canManage
            then mempty
            else [hsx|
                <td>
                    <a href={EditSourceAction source.id} class="btn btn-sm btn-outline-secondary" data-testid="edit-source">Edit</a>
                    <form method="POST" action={ToggleSourceAction source.id} class="d-inline">
                        <button type="submit" class="btn btn-sm btn-outline-warning" data-testid="toggle-source">{toggleLabel}</button>
                    </form>
                </td>
            |]
        toggleLabel :: Text
        toggleLabel = if source.enabled then "Disable" else "Enable"
