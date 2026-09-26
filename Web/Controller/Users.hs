module Web.Controller.Users where

import Application.Helper.Controller (allPrivileges)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Crypto.PasswordStore (makePassword)
import qualified Data.Text as Text
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
import Web.Controller.Prelude
import Web.View.Users.Edit
import Web.View.Users.Index
import Web.View.Users.New

-- Admin → Users (provision parity): user accounts are provisionable
-- (users section keyed by email); this page is their webUI counterpart.
-- Email is the natural key and create-only; settings (theme/timezone/
-- language) stay on the profile page.
instance Controller UsersController where
    beforeAction = ensureIsUser

    action UsersAction = do
        requirePrivilege "manage_users"
        users <- query @User |> orderByAsc #email |> fetch
        rolesByUser <- forM users \user -> do
            let userId = get #id user
            rows <-
                sqlQueryTyped
                    [typedSql|
                SELECT r.name FROM user_roles ur JOIN roles r ON r.id = ur.role_id
                WHERE ur.user_id = ${userId} ORDER BY r.name
            |]
            pure (rows :: [Text])
        render IndexView{usersWithRoles = zip users rolesByUser}
    action NewUserAction = do
        requirePrivilege "manage_users"
        roles <- query @Role |> orderByAsc #name |> fetch
        render NewView{roles}
    action CreateUserAction = do
        requirePrivilege "manage_users"
        let email = Text.strip (param @Text "email")
            displayName = param @Text "displayName"
            password = param @Text "password"
        existing <- query @User |> filterWhere (#email, email) |> fetchOneOrNothing
        if Text.null email || Text.null password
            then do
                setErrorMessage (tr "Email and password are required")
                redirectTo NewUserAction
            else case existing of
                Just _ -> do
                    setErrorMessage (trp "User {email} already exists" [("email", email)])
                    redirectTo NewUserAction
                Nothing -> do
                    passwordHash <- cs <$> makePassword (cs password) 17
                    user <-
                        newRecord @User
                            |> set #email email
                            |> set #displayName displayName
                            |> set #passwordHash passwordHash
                            |> createRecord
                    assignRoles (get #id user)
                    setSuccessMessage (trp "User {email} created" [("email", email)])
                    redirectTo UsersAction
    action EditUserAction{userId} = do
        requirePrivilege "manage_users"
        user <- fetch userId
        ensureNotProtected user.email (get #protected user)
        roles <- query @Role |> orderByAsc #name |> fetch
        assigned <- roleIdsFor userId
        render EditView{user, roles, assignedRoleIds = assigned}
    action UpdateUserAction{userId} = do
        requirePrivilege "manage_users"
        user <- fetch userId
        ensureNotProtected user.email (get #protected user)
        let displayName = param @Text "displayName"
            password = param @Text "password"
        withTransaction do
            updated <-
                if Text.null password
                    then pure user
                    else do
                        passwordHash <- cs <$> makePassword (cs password) 17
                        updateRecord (user |> set #passwordHash passwordHash)
            _ <-
                updated
                    |> set #displayName displayName
                    |> updateRecord
            replaceRoles userId
        setSuccessMessage (trp "User {email} updated" [("email", user.email)])
        redirectTo UsersAction
    action DeleteUserAction{userId} = do
        requirePrivilege "manage_users"
        user <- fetch userId
        ensureNotProtected user.email (get #protected user)
        if userId == currentUserId
            then do
                setErrorMessage (tr "You cannot delete your own account")
                redirectTo UsersAction
            else do
                result <- try do
                    void $ sqlExecTyped [typedSql| DELETE FROM user_roles WHERE user_id = ${userId} |]
                    void $ sqlExecTyped [typedSql| DELETE FROM team_members WHERE user_id = ${userId} |]
                    deleteRecord user
                case result of
                    Left err -> setErrorMessage (trp "Cannot delete user {email}: the account is referenced (alerts, comments, dashboards, …)" [("email", user.email)] <> " — " <> tshow (err :: SomeException))
                    Right () -> setSuccessMessage (trp "User {email} deleted" [("email", user.email)])
                redirectTo UsersAction

-- Checkbox list param "roles" carrying role names; unknown names are dropped.
assignRoles :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond) => Id User -> IO ()
assignRoles userId = do
    let names = filter (not . Text.null) (paramList @Text "roles")
    forM_ names \roleName -> do
        void $
            sqlExecTyped
                [typedSql|
            INSERT INTO user_roles (user_id, role_id)
            SELECT ${userId}, id FROM roles WHERE name = ${roleName}
            ON CONFLICT (user_id, role_id) DO NOTHING
        |]

replaceRoles :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond) => Id User -> IO ()
replaceRoles userId = do
    void $ sqlExecTyped [typedSql| DELETE FROM user_roles WHERE user_id = ${userId} |]
    assignRoles userId

roleIdsFor :: (?modelContext :: ModelContext) => Id User -> IO [Id Role]
roleIdsFor userId = do
    rows <- query @UserRole |> filterWhere (#userId, userId) |> fetch
    pure (map (.roleId) rows)
