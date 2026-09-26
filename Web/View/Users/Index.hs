module Web.View.Users.Index where

import qualified Data.Text as Text
import Web.View.Fragments (editDeleteActionsHtml, emptyStateHtml, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView {usersWithRoles :: [(User, [Text])]}

instance View IndexView where
    beforeRender _ = setPageTitle (tr "Users")
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml (tr "Users") newButton}
        {tableOrEmpty}
    |]
      where
        tableOrEmpty =
            if null usersWithRoles
                then emptyStateHtml "users-empty" (tr "No users yet.")
                else
                    [hsx|
        <table class="table" data-testid="users-table">
            <thead>
                <tr>
                    <th>{tr "Email"}</th>
                    <th>{tr "Display name"}</th>
                    <th>{tr "Roles"}</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach usersWithRoles renderUser}
            </tbody>
        </table>
                    |]
        newButton = [hsx|<a href={NewUserAction} class="btn btn-brand" data-testid="new-user">{tr "New user"}</a>|]

renderUser :: (User, [Text]) -> Html
renderUser (user, roleNames) =
    [hsx|
    <tr data-testid="user-row">
        <td>{user.email} {protectedBadgeHtml (get #protected user)}</td>
        <td>{user.displayName}</td>
        <td>{roleList}</td>
        <td>
            {editDeleteActionsHtml (pathTo (EditUserAction user.id)) (pathTo (DeleteUserAction user.id)) "edit-user"}
        </td>
    </tr>
|]
  where
    roleList = if null roleNames then "-" :: Text else Text.intercalate ", " roleNames
