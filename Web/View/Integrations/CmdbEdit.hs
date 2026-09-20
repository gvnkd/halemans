module Web.View.Integrations.CmdbEdit where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Integrations.Form (cmdbConfigFormFields)
import Web.View.Prelude

data CmdbEditView = CmdbEditView {config :: CmdbConfig}

instance View CmdbEditView where
    html CmdbEditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit CMDB (Confluence) connection") mempty}
        <form method="POST" action={UpdateCmdbConfigAction config.id} data-testid="cmdb-config-edit-form" class="maxw-500">
            {cmdbConfigFormFields (Just config)}
            <button type="submit" class="btn btn-primary" data-testid="cmdb-config-submit">{tr "Save"}</button>
        </form>
    |]
