module Web.View.Roles.New where

import Application.Helper.Controller (allPrivileges)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data NewView = NewView

instance View NewView where
    beforeRender _ = setPageTitle (tr "Roles")
    html NewView =
        [hsx|
        {pageHeaderHtml (tr "New role") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={CreateRoleAction} data-testid="role-form">
            <div class="mb-3">
                <label class="form-label">{tr "Name"}</label>
                <input name="name" type="text" class="form-control" data-testid="role-name" required="required"/>
            </div>
            {privilegeCheckboxes []}
            <button type="submit" class="btn btn-brand" data-testid="role-submit">{tr "Create"}</button>
        </form>
        </div></div>|]

-- Shared with Edit. One checkbox per fixed privilege (the `admin` privilege
-- implies all others at runtime).
privilegeCheckboxes :: [Text] -> Html
privilegeCheckboxes granted =
    [hsx|
    <div class="mb-3">
        <label class="form-label">{tr "Privileges"}</label>
        <div data-testid="role-privileges">
            {forEach allPrivileges privilegeCheckbox}
        </div>
    </div>
|]
  where
    privilegeCheckbox privilege =
        [hsx|
        <div class="form-check">
            <input name={"privilege_" <> privilege} type="checkbox" class="form-check-input" checked={privilege `elem` granted} data-testid={"role-privilege-" <> privilege}/>
            <label class="form-check-label"><code>{privilege}</code></label>
        </div>
    |]
