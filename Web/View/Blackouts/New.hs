module Web.View.Blackouts.New where

import Web.View.Blackouts.Form (blackoutFormFields)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data NewView = NewView
    { environments :: [Environment]
    , hosts :: [Host]
    , services :: [Service]
    }

instance View NewView where
    html NewView{..} =
        [hsx|
        {pageHeaderHtml (tr "New blackout") mempty}
        <div class="card maxw-500"><div class="card-body">
        <form method="POST" action={CreateBlackoutAction} data-testid="blackout-form">
            {blackoutFormFields Nothing environments hosts services}
            <button type="submit" class="btn btn-brand" data-testid="blackout-submit">{tr "Create"}</button>
        </form>
        </div></div>
    |]
