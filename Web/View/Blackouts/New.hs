module Web.View.Blackouts.New where

import Web.View.Blackouts.Form (blackoutFormFields)
import Web.View.Prelude

data NewView = NewView
    { environments :: [Environment]
    , hosts :: [Host]
    , services :: [Service]
    }

instance View NewView where
    html NewView{..} =
        [hsx|
        <h1>{tr "New blackout"}</h1>
        <form method="POST" action={CreateBlackoutAction} data-testid="blackout-form" class="maxw-500">
            {blackoutFormFields Nothing environments hosts services}
            <button type="submit" class="btn btn-primary" data-testid="blackout-submit">{tr "Create"}</button>
        </form>
    |]
