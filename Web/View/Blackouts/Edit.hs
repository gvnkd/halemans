module Web.View.Blackouts.Edit where
import Web.View.Prelude
import Web.View.Blackouts.Form (blackoutFormFields)

data EditView = EditView
    { blackout :: Blackout
    , environments :: [Environment]
    , hosts :: [Host]
    , services :: [Service]
    }

instance View EditView where
    html EditView { .. } = [hsx|
        <h1>Edit blackout</h1>
        <form method="POST" action={UpdateBlackoutAction blackout.id} data-testid="blackout-edit-form" class="maxw-500">
            {blackoutFormFields (Just blackout) environments hosts services}
            <button type="submit" class="btn btn-primary" data-testid="blackout-submit">Save</button>
        </form>
    |]
