module Web.View.Blackouts.New where
import Web.View.Prelude
import Web.View.Blackouts.Form (blackoutFormFields)

data NewView = NewView
    { environments :: [Environment]
    , hosts :: [Host]
    , services :: [Service]
    }

instance View NewView where
    html NewView { .. } = [hsx|
        <h1>New blackout</h1>
        <form method="POST" action={CreateBlackoutAction} data-testid="blackout-form" class="maxw-500">
            {blackoutFormFields Nothing environments hosts services}
            <button type="submit" class="btn btn-primary" data-testid="blackout-submit">Create</button>
        </form>
    |]
