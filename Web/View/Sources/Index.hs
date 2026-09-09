module Web.View.Sources.Index where
import Web.View.Prelude
import Web.View.Fragments (pageHeaderHtml, inlinePostFormHtml, enabledBadgeHtml)

data IndexView = IndexView
    { sources :: [Source]
    , canManage :: Bool
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {pageHeaderHtml "Sources" newButton}
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
renderSourceRow canManage source = [hsx|
    <tr data-source-type={sourceType} data-testid="source-row">
        <td>{source.name}</td>
        <td>{sourceType}</td>
        <td>{source.baseUrl}</td>
        <td>{source.env}</td>
        <td>{enabledBadge}</td>
        <td data-testid="source-health">{healthBadge}</td>
        <td data-testid="source-failures">{show source.consecutiveFailures :: Text}</td>
        <td data-testid="source-last-error">{fromMaybe "" source.lastError}</td>
        <td data-testid="source-next-poll">{utcTimeOrHtml "on schedule" source.nextPollAt}</td>
        <td>{source.pollIntervalSeconds}s</td>
        <td data-testid="source-last-sync">{utcTimeOrHtml "never" source.lastSyncCursor}</td>
        {actions}
    </tr>
|]
    where
        sourceType :: Text
        sourceType = get #type_ source
        enabledBadge = enabledBadgeHtml source.enabled
        healthBadge = if source.consecutiveFailures > 0
            then [hsx|<span class="badge bg-danger">failing</span>|]
            else [hsx|<span class="badge bg-success">healthy</span>|]
        actions = if not canManage
            then mempty
            else [hsx|
                <td>
                    <a href={EditSourceAction source.id} class="btn btn-sm btn-outline-secondary" data-testid="edit-source">Edit</a>
                    {inlinePostFormHtml (pathTo (ToggleSourceAction source.id)) toggleLabel "btn btn-sm btn-outline-warning" (Just "toggle-source") False}
                    {syncButton}
                </td>
            |]
        syncButton = if sourceType == "zabbix"
            then inlinePostFormHtml (pathTo (SyncHostGroupsAction source.id)) "Sync host groups" "btn btn-sm btn-outline-secondary" (Just "sync-host-groups") False
            else mempty
        toggleLabel :: Text
        toggleLabel = if source.enabled then "Disable" else "Enable"
