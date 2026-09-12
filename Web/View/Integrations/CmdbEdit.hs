module Web.View.Integrations.CmdbEdit where

import Web.View.Integrations.Form (cmdbConfigFormFields)
import Web.View.Prelude

data CmdbEditView = CmdbEditView {config :: CmdbConfig}

instance View CmdbEditView where
    html CmdbEditView{..} =
        [hsx|
        <h1>Edit CMDB (Confluence) connection</h1>
        <form method="POST" action={UpdateCmdbConfigAction config.id} data-testid="cmdb-config-edit-form" class="maxw-500">
            {cmdbConfigFormFields (Just config)}
            <button type="submit" class="btn btn-primary" data-testid="cmdb-config-submit">Save</button>
        </form>
    |]
