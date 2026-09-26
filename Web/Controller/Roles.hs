module Web.Controller.Roles where

import Application.Helper.Controller (allPrivileges)
import Control.Monad (void)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Web.Controller.Prelude
import Web.View.Roles.Edit
import Web.View.Roles.Index
import Web.View.Roles.New

-- Admin → Roles (provision parity): the privilege matrix itself is
-- provisionable (roles section: name → privileges); this page edits the
-- same rows. Deleting removes user_roles links first (mirroring the
-- provision strict-delete order).
instance Controller RolesController where
    beforeAction = ensureIsUser

    action RolesAction = do
        requirePrivilege "manage_users"
        roles <- query @Role |> orderByAsc #name |> fetch
        memberCounts <- forM roles \role -> do
            count <- query @UserRole |> filterWhere (#roleId, get #id role) |> fetchCount
            pure (get #id role, count)
        render IndexView{roles, memberCounts}
    action NewRoleAction = do
        requirePrivilege "manage_users"
        render NewView
    action CreateRoleAction = do
        requirePrivilege "manage_users"
        let name = param @Text "name"
            privileges = privilegeParams
        existing <- query @Role |> filterWhere (#name, name) |> fetchOneOrNothing
        if null name
            then do
                setErrorMessage (tr "Role name is required")
                redirectTo NewRoleAction
            else case existing of
                Just _ -> do
                    setErrorMessage (trp "Role {name} already exists" [("name", name)])
                    redirectTo NewRoleAction
                Nothing -> do
                    _ <-
                        newRecord @Role
                            |> set #name name
                            |> set #privileges privileges
                            |> createRecord
                    setSuccessMessage (trp "Role {name} created" [("name", name)])
                    redirectTo RolesAction
    action EditRoleAction{roleId} = do
        requirePrivilege "manage_users"
        role <- fetch roleId
        ensureNotProtected role.name (get #protected role)
        render EditView{role}
    action UpdateRoleAction{roleId} = do
        requirePrivilege "manage_users"
        role <- fetch roleId
        ensureNotProtected role.name (get #protected role)
        let name = param @Text "name"
        _ <-
            role
                |> set #name name
                |> set #privileges privilegeParams
                |> updateRecord
        setSuccessMessage (trp "Role {name} updated" [("name", name)])
        redirectTo RolesAction
    action DeleteRoleAction{roleId} = do
        requirePrivilege "manage_users"
        role <- fetch roleId
        ensureNotProtected role.name (get #protected role)
        void $ sqlExecTyped [typedSql| DELETE FROM user_roles WHERE role_id = ${roleId} |]
        deleteRecord role
        setSuccessMessage (trp "Role {name} deleted" [("name", role.name)])
        redirectTo RolesAction

-- Checkbox group "privilege_<name>" (one input per allPrivileges entry);
-- the fixed privilege set is enforced in code, so unknown values are dropped.
privilegeParams :: (?request :: Request, ?respond :: Respond) => [Text]
privilegeParams =
    [ privilege
    | privilege <- allPrivileges
    , isJust (paramOrNothing @Text (cs ("privilege_" <> privilege)))
    ]
