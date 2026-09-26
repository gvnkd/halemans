module Web.View.Roles.Index where

import qualified Data.Text as Text
import Web.View.Fragments (editDeleteActionsHtml, emptyStateHtml, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView {roles :: [Role], memberCounts :: [(Id Role, Int)]}

instance View IndexView where
    beforeRender _ = setPageTitle (tr "Roles")
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml (tr "Roles") newButton}
        {tableOrEmpty}
    |]
      where
        tableOrEmpty =
            if null roles
                then emptyStateHtml "roles-empty" (tr "No roles yet.")
                else
                    [hsx|
        <table class="table" data-testid="roles-table">
            <thead>
                <tr>
                    <th>{tr "Name"}</th>
                    <th>{tr "Privileges"}</th>
                    <th>{tr "Members"}</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach roles renderRole}
            </tbody>
        </table>
                    |]
        newButton = [hsx|<a href={NewRoleAction} class="btn btn-brand" data-testid="new-role">{tr "New role"}</a>|]
        renderRole role =
            let memberCount = fromMaybe 0 (lookup (get #id role) memberCounts)
             in [hsx|
            <tr data-testid="role-row">
                <td>{role.name} {protectedBadgeHtml (get #protected role)}</td>
                <td>{privilegeList role}</td>
                <td>{memberCount}</td>
                <td>
                    {editDeleteActionsHtml (pathTo (EditRoleAction role.id)) (pathTo (DeleteRoleAction role.id)) "edit-role"}
                </td>
            </tr>
        |]
        privilegeList role =
            if null role.privileges
                then "-" :: Text
                else Text.intercalate ", " role.privileges
