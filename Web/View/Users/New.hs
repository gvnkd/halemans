module Web.View.Users.New where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data NewView = NewView {roles :: [Role]}

instance View NewView where
    beforeRender _ = setPageTitle (tr "Users")
    html NewView{..} =
        [hsx|
        {pageHeaderHtml (tr "New user") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={CreateUserAction} data-testid="user-form">
            <div class="mb-3">
                <label class="form-label">{tr "Email"}</label>
                <input name="email" type="email" class="form-control" data-testid="user-email" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Display name"}</label>
                <input name="displayName" type="text" class="form-control" data-testid="user-display-name"/>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Password"}</label>
                <input name="password" type="password" class="form-control" data-testid="user-password" required="required"/>
            </div>
            {roleCheckboxes roles []}
            <button type="submit" class="btn btn-brand" data-testid="user-submit">{tr "Create"}</button>
        </form>
        </div></div>|]

-- Shared with Edit. One checkbox per role; checked when the user holds it.
roleCheckboxes :: [Role] -> [Id Role] -> Html
roleCheckboxes roles assigned =
    [hsx|
    <div class="mb-3">
        <label class="form-label">{tr "Roles"}</label>
        <div data-testid="user-roles">
            {forEach roles roleCheckbox}
        </div>
    </div>
|]
  where
    roleCheckbox role =
        [hsx|
        <div class="form-check">
            <input name="roles" type="checkbox" class="form-check-input" value={role.name} checked={get #id role `elem` assigned} data-testid={"user-role-" <> role.name}/>
            <label class="form-check-label">{role.name}</label>
        </div>
    |]
