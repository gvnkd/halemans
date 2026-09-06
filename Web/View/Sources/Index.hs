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
                    <th>Health</th>
                    <th>Failures</th>
                    <th>Last error</th>
                    <th>Next poll</th>
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
    let lastSync = maybe "never" (cs . show) source.lastSyncCursor :: Text
        failures = show source.consecutiveFailures :: Text
        lastError = fromMaybe "" source.lastError
        nextPoll = maybe "on schedule" (cs . show) source.nextPollAt :: Text
    in [hsx|
    <tr data-source-type={sourceType} data-testid="source-row">
        <td>{source.name}</td>
        <td>{sourceType}</td>
        <td>{source.baseUrl}</td>
        <td>{source.env}</td>
        <td>{enabledBadge}</td>
        <td data-testid="source-health">{healthBadge}</td>
        <td data-testid="source-failures">{failures}</td>
        <td data-testid="source-last-error">{lastError}</td>
        <td data-testid="source-next-poll">{nextPoll}</td>
        <td>{source.pollIntervalSeconds}s</td>
        <td data-testid="source-last-sync">{lastSync}</td>
        {actions}
    </tr>
|]
    where
        sourceType :: Text
        sourceType = get #type_ source
        enabledBadge = if source.enabled
            then [hsx|<span class="badge bg-success">enabled</span>|]
            else [hsx|<span class="badge bg-secondary">disabled</span>|]
        healthBadge = if source.consecutiveFailures > 0
            then [hsx|<span class="badge bg-danger">failing</span>|]
            else [hsx|<span class="badge bg-success">healthy</span>|]
        actions = if not canManage
            then mempty
            else [hsx|
                <td>
                    <a href={EditSourceAction source.id} class="btn btn-sm btn-outline-secondary" data-testid="edit-source">Edit</a>
                    <form method="POST" action={ToggleSourceAction source.id} class="d-inline">
                        <button type="submit" class="btn btn-sm btn-outline-warning" data-testid="toggle-source">{toggleLabel}</button>
                    </form>
                    {syncButton}
                </td>
            |]
        syncButton = if sourceType == "zabbix"
            then [hsx|
                <form method="POST" action={SyncHostGroupsAction source.id} class="d-inline">
                    <button type="submit" class="btn btn-sm btn-outline-secondary" data-testid="sync-host-groups">Sync host groups</button>
                </form>
            |]
            else mempty
        toggleLabel :: Text
        toggleLabel = if source.enabled then "Disable" else "Enable"
