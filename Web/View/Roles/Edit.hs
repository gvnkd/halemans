module Web.View.Roles.Edit where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude
import Web.View.Roles.New (privilegeCheckboxes)

data EditView = EditView {role :: Role}

instance View EditView where
    beforeRender _ = setPageTitle (tr "Roles")
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit role") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={UpdateRoleAction role.id} data-testid="role-edit-form">
            <div class="mb-3">
                <label class="form-label">{tr "Name"}</label>
                <input name="name" type="text" class="form-control" value={role.name} data-testid="role-name" required="required"/>
            </div>
            {privilegeCheckboxes role.privileges}
            <button type="submit" class="btn btn-brand" data-testid="role-submit">{tr "Save"}</button>
        </form>
        </div></div>|]
