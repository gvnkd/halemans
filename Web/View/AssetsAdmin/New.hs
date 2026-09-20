module Web.View.AssetsAdmin.New where

import Web.View.AssetsAdmin.Form (assetsConfigFormFields)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data NewView = NewView

instance View NewView where
    html NewView =
        [hsx|
        {pageHeaderHtml (tr "New Assets info source") mempty}
        <p class="text-muted">
            {tr "Token env holds the NAME of the environment variable containing the bearer token, not the token itself. base_url points at the Assets REST base (.../rest/assets/latest)."}
        </p>
        <form method="POST" action={CreateAssetsConfigAction} data-testid="assets-config-new-form">
            {assetsConfigFormFields Nothing}
            <button type="submit" class="btn btn-primary" data-testid="assets-config-save">{tr "Create info source"}</button>
            <a href={AssetsAdminAction} class="btn btn-outline-secondary">{tr "Cancel"}</a>
        </form>
    |]
