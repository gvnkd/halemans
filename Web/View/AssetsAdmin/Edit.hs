module Web.View.AssetsAdmin.Edit where

import Web.View.AssetsAdmin.Form (assetsConfigFormFields)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data EditView = EditView
    { config :: AssetsConfig
    }

instance View EditView where
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit Assets info source") mempty}
        <p class="text-muted">{trp "{name} is {state}. Enabling/disabling happens from the list." [("name", config.name), ("state", stateText)]}</p>
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={UpdateAssetsConfigAction (get #id config)} data-testid="assets-config-form">
            {assetsConfigFormFields (Just config)}
            <button type="submit" class="btn btn-brand" data-testid="assets-config-save">{tr "Save info source"}</button>
            <a href={AssetsAdminAction} class="btn btn-ghost">{tr "Cancel"}</a>
        </form>
        </div></div>
    |]
      where
        stateText :: Text
        stateText = if config.enabled then tr "enabled" else tr "disabled"
