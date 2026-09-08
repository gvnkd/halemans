module Web.View.AssetsAdmin.Index where
import Web.View.Prelude

data IndexView = IndexView
    { configs :: [AssetsConfig]
    , statsFor :: Id AssetsConfig -> (Int64, Maybe UTCTime)
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex justify-content-between align-items-center">
            <h1>Assets info sources</h1>
            <a href={NewAssetsConfigAction} class="btn btn-sm btn-primary" data-testid="new-assets-config">New info source</a>
        </div>
        <table class="table" data-testid="assets-configs">
            <thead>
                <tr><th>Name</th><th>Base URL</th><th>Schema</th><th>Auth</th><th>Cached objects</th><th>Newest fetch</th><th>Enabled</th><th></th></tr>
            </thead>
            <tbody>
                {forEach configs (configRowHtml statsFor)}
            </tbody>
        </table>
    |]

configRowHtml :: (Id AssetsConfig -> (Int64, Maybe UTCTime)) -> AssetsConfig -> Html
configRowHtml statsFor config = [hsx|
    <tr data-testid="assets-config">
        <td>{config.name}</td>
        <td data-testid="assets-config-base-url-cell">{config.baseUrl}</td>
        <td>{config.defaultSchemaName}</td>
        <td>{config.authMode}</td>
        <td data-testid="assets-cached-count">{cachedCount}</td>
        <td>{newestFetch}</td>
        <td>{enabledBadge}</td>
        <td>
            <a href={EditAssetsConfigAction configId} class="btn btn-sm btn-outline-secondary" data-testid="assets-config-edit">Edit</a>
            {toggleForm}
            <form method="POST" action={TestAssetsConnectionAction configId} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-primary" data-testid="assets-config-test">Test</button>
            </form>
            <form method="POST" action={DeleteAssetsConfigAction configId} class="d-inline js-delete">
                <button type="submit" class="btn btn-sm btn-outline-danger" data-testid="assets-config-delete">Delete</button>
            </form>
        </td>
    </tr>
|]
    where
        configId = get #id config
        (cachedCount, maybeNewest) = statsFor configId
        newestFetch = case maybeNewest of
            Just newest -> [hsx|{utcTimeHtml newest}|]
            Nothing -> [hsx|-|]
        enabledBadge = if config.enabled
            then [hsx|<span class="badge status-resolved" data-testid="assets-config-enabled">enabled</span>|]
            else [hsx|<span class="badge" data-testid="assets-config-disabled">disabled</span>|]
        toggleForm = [hsx|
            <form method="POST" action={ToggleAssetsConfigAction configId} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-warning" data-testid="assets-config-toggle">{toggleLabel}</button>
            </form>
        |]
        toggleLabel :: Text
        toggleLabel = if config.enabled then "Disable" else "Enable"
