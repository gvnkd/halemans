module Web.View.AssetsAdmin.Index where

import Web.View.Fragments (inlinePostFormHtml, pageHeaderHtml, stateBadgeHtml)
import Web.View.Prelude

data IndexView = IndexView
    { configs :: [AssetsConfig]
    , statsFor :: Id AssetsConfig -> (Int64, Maybe UTCTime)
    }

instance View IndexView where
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml "Assets info sources" newButton}
        <table class="table" data-testid="assets-configs">
            <thead>
                <tr><th>Name</th><th>Base URL</th><th>Schema</th><th>Auth</th><th>Cached objects</th><th>Newest fetch</th><th>Enabled</th><th></th></tr>
            </thead>
            <tbody>
                {forEach configs (configRowHtml statsFor)}
            </tbody>
        </table>
    |]
      where
        newButton = [hsx|<a href={NewAssetsConfigAction} class="btn btn-sm btn-primary" data-testid="new-assets-config">New info source</a>|]

configRowHtml :: (Id AssetsConfig -> (Int64, Maybe UTCTime)) -> AssetsConfig -> Html
configRowHtml statsFor config =
    [hsx|
    <tr data-testid="assets-config">
        <td>{config.name}</td>
        <td data-testid="assets-config-base-url-cell">{config.baseUrl}</td>
        <td>{config.defaultSchemaName}</td>
        <td>{config.authMode}</td>
        <td data-testid="assets-cached-count">{cachedCount}</td>
        <td>{newestFetch}</td>
        <td>{stateBadgeHtml config.enabled "assets-config"}</td>
        <td>
            <a href={EditAssetsConfigAction configId} class="btn btn-sm btn-outline-secondary" data-testid="assets-config-edit">Edit</a>
            {toggleForm}
            {inlinePostFormHtml (pathTo (TestAssetsConnectionAction configId)) "Test" "btn btn-sm btn-outline-primary" (Just "assets-config-test") False}
            {inlinePostFormHtml (pathTo (DeleteAssetsConfigAction configId)) "Delete" "btn btn-sm btn-outline-danger" (Just "assets-config-delete") True}
        </td>
    </tr>
|]
  where
    configId = get #id config
    (cachedCount, maybeNewest) = statsFor configId
    newestFetch = case maybeNewest of
        Just newest -> [hsx|{utcTimeHtml newest}|]
        Nothing -> [hsx|-|]
    toggleForm = inlinePostFormHtml (pathTo (ToggleAssetsConfigAction configId)) toggleLabel "btn btn-sm btn-outline-warning" (Just "assets-config-toggle") False
    toggleLabel :: Text
    toggleLabel = if config.enabled then "Disable" else "Enable"
