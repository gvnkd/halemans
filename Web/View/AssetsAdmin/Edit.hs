module Web.View.AssetsAdmin.Edit where

import Web.View.AssetsAdmin.Form (assetsConfigFormFields)
import Web.View.Prelude

data EditView = EditView
    { config :: AssetsConfig
    }

instance View EditView where
    html EditView{..} =
        [hsx|
        <h1>Edit Assets info source</h1>
        <p class="text-muted">{config.name} is {stateText}. Enabling/disabling happens from the list.</p>
        <form method="POST" action={UpdateAssetsConfigAction (get #id config)} data-testid="assets-config-form">
            {assetsConfigFormFields (Just config)}
            <button type="submit" class="btn btn-primary" data-testid="assets-config-save">Save info source</button>
            <a href={AssetsAdminAction} class="btn btn-outline-secondary">Cancel</a>
        </form>
    |]
      where
        stateText :: Text
        stateText = if config.enabled then "enabled" else "disabled"
