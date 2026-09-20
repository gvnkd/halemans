module Web.View.Blackouts.Edit where

import Web.View.Blackouts.Form (blackoutFormFields)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data EditView = EditView
    { blackout :: Blackout
    , environments :: [Environment]
    , hosts :: [Host]
    , services :: [Service]
    }

instance View EditView where
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit blackout") mempty}
        <div class="card maxw-500"><div class="card-body">
        <form method="POST" action={UpdateBlackoutAction blackout.id} data-testid="blackout-edit-form">
            {blackoutFormFields (Just blackout) environments hosts services}
            <button type="submit" class="btn btn-brand" data-testid="blackout-submit">{tr "Save"}</button>
        </form>
        </div></div>
    |]
