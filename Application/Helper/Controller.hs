module Application.Helper.Controller where

import Generated.Types
import IHP.ControllerPrelude
import IHP.LoginSupport.Helper.Controller
import IHP.LoginSupport.Types (HasNewSessionUrl (..))

-- Auth identity lives here (not Web.Types) so Config.hs can see the
-- instances for the AuthMiddleware without importing the Web layer.
instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User

-- Fixed privilege set enforced in code (design_docs/milestone_1.md §2).
allPrivileges :: [Text]
allPrivileges =
    [ "view"
    , "ack"
    , "close"
    , "escalate"
    , "manage_blackouts"
    , "manage_rules"
    , "manage_users"
    , "manage_sources"
    , "admin"
    ]

-- | All privileges granted to the user via their roles. The @admin@
-- privilege implies every other privilege.
userPrivileges :: (?modelContext :: ModelContext) => Id User -> IO [Text]
userPrivileges userId = do
    userRoles <-
        query @UserRole
            |> filterWhere (#userId, userId)
            |> fetch
    roles <- forM userRoles \userRole -> fetch userRole.roleId
    let granted = concatMap (.privileges) roles
    pure (if "admin" `elem` granted then allPrivileges else granted)

currentUserPrivileges :: (CurrentUserRecord ~ User, ?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext) => IO [Text]
currentUserPrivileges = case currentUserIdOrNothing of
    Just userId -> userPrivileges userId
    Nothing -> pure []

currentUserHasPrivilege :: (CurrentUserRecord ~ User, ?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext) => Text -> IO Bool
currentUserHasPrivilege privilege = elem privilege <$> currentUserPrivileges

-- | Text query parameter; empty string means "not set".
nonEmptyParam :: (?request :: Request) => ByteString -> Maybe Text
nonEmptyParam name =
    paramOrNothing @Text name >>= \value ->
        if value == "" then Nothing else Just value
