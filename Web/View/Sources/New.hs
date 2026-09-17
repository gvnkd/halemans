module Web.View.Sources.New where

import Web.View.Prelude
import Web.View.Sources.Form (defaultSourceFormValues, sourceFormFields)

data NewView = NewView

instance View NewView where
    html NewView =
        [hsx|
        <h1>{tr "New source"}</h1>
        <form method="POST" action={CreateSourceAction} data-testid="source-form" class="maxw-500">
            {sourceFormFields defaultSourceFormValues}
            <button type="submit" class="btn btn-primary" data-testid="source-submit">{tr "Create"}</button>
        </form>
    |]
