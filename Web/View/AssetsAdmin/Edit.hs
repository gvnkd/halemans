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
        <form method="POST" action={UpdateAssetsConfigAction (get #id config)} data-testid="assets-config-form">
            {assetsConfigFormFields (Just config)}
            <button type="submit" class="btn btn-primary" data-testid="assets-config-save">{tr "Save info source"}</button>
            <a href={AssetsAdminAction} class="btn btn-outline-secondary">{tr "Cancel"}</a>
        </form>
    |]
      where
        stateText :: Text
        stateText = if config.enabled then tr "enabled" else tr "disabled"
