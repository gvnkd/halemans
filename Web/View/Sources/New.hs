module Web.View.Sources.New where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude
import Web.View.Sources.Form (defaultSourceFormValues, sourceFormFields)

data NewView = NewView

instance View NewView where
    beforeRender _ = setPageTitle (tr "Sources")
    html NewView =
        [hsx|
        {pageHeaderHtml (tr "New source") mempty}
        <div class="card maxw-500"><div class="card-body">
        <form method="POST" action={CreateSourceAction} data-testid="source-form">
            {sourceFormFields defaultSourceFormValues}
            <button type="submit" class="btn btn-brand" data-testid="source-submit">{tr "Create"}</button>
        </form>
        </div></div>
    |]
