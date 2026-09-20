module Web.View.Integrations.CmdbNew where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Integrations.Form (cmdbConfigFormFields)
import Web.View.Prelude

data CmdbNewView = CmdbNewView

instance View CmdbNewView where
    html CmdbNewView =
        [hsx|
        {pageHeaderHtml (tr "New CMDB (Confluence) connection") mempty}
        <form method="POST" action={CreateCmdbConfigAction} data-testid="cmdb-config-form" class="maxw-500">
            {cmdbConfigFormFields Nothing}
            <button type="submit" class="btn btn-primary" data-testid="cmdb-config-submit">{tr "Create"}</button>
        </form>
    |]
