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
        {pageHeaderHtml (tr "Assets info sources") newButton}
        <table class="table" data-testid="assets-configs">
            <thead>
                <tr><th>{tr "Name"}</th><th>{tr "Base URL"}</th><th>{tr "Schema"}</th><th>{tr "Auth"}</th><th>{tr "Cached objects"}</th><th>{tr "Newest fetch"}</th><th>{tr "Enabled"}</th><th></th></tr>
            </thead>
            <tbody>
                {forEach configs (configRowHtml statsFor)}
            </tbody>
        </table>
    |]
      where
        newButton = [hsx|<a href={NewAssetsConfigAction} class="btn btn-sm btn-primary" data-testid="new-assets-config">{tr "New info source"}</a>|]

configRowHtml :: (Id AssetsConfig -> (Int64, Maybe UTCTime)) -> AssetsConfig -> Html
configRowHtml statsFor config =
    [hsx|
    <tr data-testid="assets-config">
        <td>{config.name} {protectedBadgeHtml (get #protected config)}</td>
        <td data-testid="assets-config-base-url-cell">{config.baseUrl}</td>
        <td>{config.defaultSchemaName}</td>
        <td>{config.authMode}</td>
        <td data-testid="assets-cached-count">{cachedCount}</td>
        <td>{newestFetch}</td>
        <td>{stateBadgeHtml config.enabled "assets-config"}</td>
        <td>
            <a href={EditAssetsConfigAction configId} class="btn btn-sm btn-outline-secondary" data-testid="assets-config-edit">{tr "Edit"}</a>
            {toggleForm}
            {inlinePostFormHtml (pathTo (TestAssetsConnectionAction configId)) (tr "Test") "btn btn-sm btn-outline-primary" (Just "assets-config-test") False}
            {inlinePostFormHtml (pathTo (DeleteAssetsConfigAction configId)) (tr "Delete") "btn btn-sm btn-outline-danger" (Just "assets-config-delete") True}
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
    toggleLabel = if config.enabled then tr "Disable" else tr "Enable"
