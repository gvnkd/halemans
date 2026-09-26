module Web.View.Users.Edit where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude
import Web.View.Users.New (roleCheckboxes)

data EditView = EditView
    { user :: User
    , roles :: [Role]
    , assignedRoleIds :: [Id Role]
    }

instance View EditView where
    beforeRender _ = setPageTitle (tr "Users")
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit user") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={UpdateUserAction user.id} data-testid="user-edit-form">
            <div class="mb-3">
                <label class="form-label">{tr "Email"}</label>
                <input type="email" class="form-control" value={user.email} disabled="disabled"/>
                <div class="form-text">{tr "The email is the account's identity (and the provision file's key) and cannot be changed."}</div>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Display name"}</label>
                <input name="displayName" type="text" class="form-control" value={user.displayName} data-testid="user-display-name"/>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "New password"}</label>
                <input name="password" type="password" class="form-control" data-testid="user-password"/>
                <div class="form-text">{tr "Leave empty to keep the current password."}</div>
            </div>
            {roleCheckboxes roles assignedRoleIds}
            <button type="submit" class="btn btn-brand" data-testid="user-submit">{tr "Save"}</button>
        </form>
        </div></div>|]
