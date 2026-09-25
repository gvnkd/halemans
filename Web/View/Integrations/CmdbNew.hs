module Web.View.Integrations.CmdbNew where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Integrations.Form (cmdbConfigFormFields)
import Web.View.Prelude

data CmdbNewView = CmdbNewView

instance View CmdbNewView where
    beforeRender _ = setPageTitle (tr "Integrations")
    html CmdbNewView =
        [hsx|
        {pageHeaderHtml (tr "New CMDB (Confluence) connection") mempty}
        <div class="card maxw-500"><div class="card-body">
        <form method="POST" action={CreateCmdbConfigAction} data-testid="cmdb-config-form">
            {cmdbConfigFormFields Nothing}
            <button type="submit" class="btn btn-brand" data-testid="cmdb-config-submit">{tr "Create"}</button>
        </form>
        </div></div>|]
