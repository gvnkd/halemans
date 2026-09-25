module Web.View.Integrations.CmdbEdit where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Integrations.Form (cmdbConfigFormFields)
import Web.View.Prelude

data CmdbEditView = CmdbEditView {config :: CmdbConfig}

instance View CmdbEditView where
    beforeRender _ = setPageTitle (tr "Integrations")
    html CmdbEditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit CMDB (Confluence) connection") mempty}
        <div class="card maxw-500"><div class="card-body">
        <form method="POST" action={UpdateCmdbConfigAction config.id} data-testid="cmdb-config-edit-form">
            {cmdbConfigFormFields (Just config)}
            <button type="submit" class="btn btn-brand" data-testid="cmdb-config-submit">{tr "Save"}</button>
        </form>
        </div></div>|]
