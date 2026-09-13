module Test.PrivilegeSpec where

import Application.Service.DatabaseStats (DatabaseStats (..), TableStats (..), analyzeTable, fetchDatabaseStats)
import qualified Config
import Control.Exception (bracket_)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Generated.Types
import IHP.ControllerSupport (InitControllerContext)
import IHP.FrameworkConfig (FrameworkConfig)
import qualified IHP.FrameworkConfig as FrameworkConfig
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import IHP.ModelSupport (Id' (..), ModelContext, createRecord, newRecord)
import qualified IHP.ModelSupport as ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, query)
import IHP.Test.Mocking
import Network.HTTP.Types (status200, status403)
import Network.Wai (Response, defaultRequest, responseStatus)
import Network.Wai.Internal (ResponseReceived (..))
import System.Environment (getEnv, lookupEnv)
import System.Process (callProcess)
import Test.Hspec
import Web.Controller.Prelude
import Web.FrontController
import Web.Types

-- Privilege matrix (milestone 12 §7): every mutating/admin action must
-- answer 403 for a user with NO roles, and must not 403 for an admin.
-- requirePrivilege runs before any fetch, so nil ids are safe.
spec :: Spec
spec = around withTestApp do
    describe "privilege matrix" do
        it "denies every privileged action to a role-less user" \mockContext -> withCtx mockContext do
            user <- newTestUser "noroles@example.com"
            forM_ deniedCases \(label, denied) ->
                withUser user do
                    response <- runDenied denied
                    when (responseStatus response /= status403) do
                        expectationFailure (cs (label <> ": expected 403, got " <> tshow (responseStatus response)))
        it "lets an admin through the privilege guard" \mockContext -> withCtx mockContext do
            user <- newTestUser "admin@example.com"
            role <- newRecord @Role |> set #name "admin" |> set #privileges ["admin"] |> createRecord
            _ <- newRecord @UserRole |> set #userId user.id |> set #roleId role.id |> createRecord
            withUser user do
                response <- callAction AdminAction
                responseStatus response `shouldBe` status200
                dbResponse <- callAction AdminDatabaseAction
                responseStatus dbResponse `shouldBe` status200
    describe "database maintenance" do
        it "reports stats for real tables" \mockContext -> withCtx mockContext do
            stats <- fetchDatabaseStats
            stats.databaseName `shouldSatisfy` ("test_db_" `Text.isPrefixOf`)
            map (.tableName) stats.tables `shouldContain` ["users"]
        it "analyzes known tables and rejects unknown names" \mockContext -> withCtx mockContext do
            analyzeTable "users" `shouldReturn` True
            analyzeTable "users; DROP TABLE users" `shouldReturn` False

-- | Existential wrapper so the matrix can hold actions of different
-- controller types. The controller value is kept (not the IO) so
-- 'runDenied' can call it INSIDE 'withUser' — callAction closes over the
-- implicit request, which must already carry the seeded user.
data DeniedAction = forall controller. (Controller controller, Typeable controller) => DeniedAction controller

mkDenied :: (Controller controller, Typeable controller) => controller -> DeniedAction
mkDenied = DeniedAction

runDenied :: (ContextParameters WebApplication, Typeable WebApplication) => DeniedAction -> IO Response
runDenied (DeniedAction action) = callAction action

nil :: (PrimaryKey table ~ UUID) => Id' table
nil = Id UUID.nil

deniedCases :: (ContextParameters WebApplication, Typeable WebApplication) => [(Text, DeniedAction)]
deniedCases =
    [ -- alerts (ack/close/view privileges)
      ("AckAlertAction", mkDenied (AckAlertAction nil))
    , ("UnackAlertAction", mkDenied (UnackAlertAction nil))
    , ("CloseAlertAction", mkDenied (CloseAlertAction nil))
    , ("CreateCommentAction", mkDenied (CreateCommentAction nil))
    , ("RefreshCmdbAction", mkDenied (RefreshCmdbAction nil))
    , ("RefreshAssetsAction", mkDenied (RefreshAssetsAction nil))
    , ("CreateJiraTicketAction", mkDenied (CreateJiraTicketAction nil))
    , ("DeleteJiraLinkAction", mkDenied (DeleteJiraLinkAction nil nil))
    , ("ReanalyzeAlertAction", mkDenied (ReanalyzeAlertAction nil))
    , ("LlmFeedbackAction", mkDenied (LlmFeedbackAction nil nil))
    , ("AckGroupAction", mkDenied (AckGroupAction nil))
    , -- blackouts
      ("NewBlackoutAction", mkDenied NewBlackoutAction)
    , ("CreateBlackoutAction", mkDenied CreateBlackoutAction)
    , ("EditBlackoutAction", mkDenied (EditBlackoutAction nil))
    , ("UpdateBlackoutAction", mkDenied (UpdateBlackoutAction nil))
    , ("DeleteBlackoutAction", mkDenied (DeleteBlackoutAction nil))
    , -- sources
      ("NewSourceAction", mkDenied NewSourceAction)
    , ("CreateSourceAction", mkDenied CreateSourceAction)
    , ("EditSourceAction", mkDenied (EditSourceAction nil))
    , ("UpdateSourceAction", mkDenied (UpdateSourceAction nil))
    , ("ToggleSourceAction", mkDenied (ToggleSourceAction nil))
    , ("SyncHostGroupsAction", mkDenied (SyncHostGroupsAction nil))
    , -- teams
      ("NewTeamAction", mkDenied NewTeamAction)
    , ("CreateTeamAction", mkDenied CreateTeamAction)
    , ("EditTeamAction", mkDenied (EditTeamAction nil))
    , ("UpdateTeamAction", mkDenied (UpdateTeamAction nil))
    , ("DeleteTeamAction", mkDenied (DeleteTeamAction nil))
    , -- integrations
      ("NewJiraConfigAction", mkDenied NewJiraConfigAction)
    , ("CreateJiraConfigAction", mkDenied CreateJiraConfigAction)
    , ("EditJiraConfigAction", mkDenied (EditJiraConfigAction nil))
    , ("UpdateJiraConfigAction", mkDenied (UpdateJiraConfigAction nil))
    , ("ToggleJiraConfigAction", mkDenied (ToggleJiraConfigAction nil))
    , ("DeleteJiraConfigAction", mkDenied (DeleteJiraConfigAction nil))
    , ("TestJiraConfigAction", mkDenied (TestJiraConfigAction nil))
    , ("NewCmdbConfigAction", mkDenied NewCmdbConfigAction)
    , ("CreateCmdbConfigAction", mkDenied CreateCmdbConfigAction)
    , ("EditCmdbConfigAction", mkDenied (EditCmdbConfigAction nil))
    , ("UpdateCmdbConfigAction", mkDenied (UpdateCmdbConfigAction nil))
    , ("ToggleCmdbConfigAction", mkDenied (ToggleCmdbConfigAction nil))
    , ("DeleteCmdbConfigAction", mkDenied (DeleteCmdbConfigAction nil))
    , ("TestCmdbConfigAction", mkDenied (TestCmdbConfigAction nil))
    , -- llm admin
      ("DropLlmAnalysisAction", mkDenied (DropLlmAnalysisAction nil))
    , ("NewLlmTemplateAction", mkDenied NewLlmTemplateAction)
    , ("CreateLlmTemplateAction", mkDenied CreateLlmTemplateAction)
    , ("EditLlmTemplateAction", mkDenied (EditLlmTemplateAction nil))
    , ("UpdateLlmTemplateAction", mkDenied (UpdateLlmTemplateAction nil))
    , ("ActivateLlmTemplateAction", mkDenied (ActivateLlmTemplateAction nil))
    , ("DeleteLlmTemplateAction", mkDenied (DeleteLlmTemplateAction nil))
    , ("TestLlmConnectionAction", mkDenied TestLlmConnectionAction)
    , ("NewLlmProviderAction", mkDenied NewLlmProviderAction)
    , ("CreateLlmProviderAction", mkDenied CreateLlmProviderAction)
    , ("EditLlmProviderAction", mkDenied (EditLlmProviderAction nil))
    , ("UpdateLlmProviderAction", mkDenied (UpdateLlmProviderAction nil))
    , ("EnableLlmProviderAction", mkDenied (EnableLlmProviderAction nil))
    , ("DisableLlmProviderAction", mkDenied (DisableLlmProviderAction nil))
    , ("DeleteLlmProviderAction", mkDenied (DeleteLlmProviderAction nil))
    , ("NewLlmRoleAction", mkDenied NewLlmRoleAction)
    , ("CreateLlmRoleAction", mkDenied CreateLlmRoleAction)
    , ("EditLlmRoleAction", mkDenied (EditLlmRoleAction nil))
    , ("UpdateLlmRoleAction", mkDenied (UpdateLlmRoleAction nil))
    , ("ToggleLlmRoleAction", mkDenied (ToggleLlmRoleAction nil))
    , ("SetDefaultLlmRoleAction", mkDenied (SetDefaultLlmRoleAction nil))
    , ("DeleteLlmRoleAction", mkDenied (DeleteLlmRoleAction nil))
    , ("UpdateAutoAnalyzeAction", mkDenied UpdateAutoAnalyzeAction)
    , ("UpdateToolCacheAction", mkDenied UpdateToolCacheAction)
    , -- assets admin
      ("NewAssetsConfigAction", mkDenied NewAssetsConfigAction)
    , ("CreateAssetsConfigAction", mkDenied CreateAssetsConfigAction)
    , ("EditAssetsConfigAction", mkDenied (EditAssetsConfigAction nil))
    , ("UpdateAssetsConfigAction", mkDenied (UpdateAssetsConfigAction nil))
    , ("ToggleAssetsConfigAction", mkDenied (ToggleAssetsConfigAction nil))
    , ("DeleteAssetsConfigAction", mkDenied (DeleteAssetsConfigAction nil))
    , ("TestAssetsConnectionAction", mkDenied (TestAssetsConnectionAction nil))
    , -- field mappings
      ("NewFieldMappingAction", mkDenied NewFieldMappingAction)
    , ("CreateFieldMappingAction", mkDenied CreateFieldMappingAction)
    , ("EditFieldMappingAction", mkDenied (EditFieldMappingAction nil))
    , ("UpdateFieldMappingAction", mkDenied (UpdateFieldMappingAction nil))
    , ("DeleteFieldMappingAction", mkDenied (DeleteFieldMappingAction nil))
    , ("RecomputeFacetsAction", mkDenied RecomputeFacetsAction)
    , -- grouping rules
      ("NewGroupingRuleAction", mkDenied NewGroupingRuleAction)
    , ("CreateGroupingRuleAction", mkDenied CreateGroupingRuleAction)
    , ("EditGroupingRuleAction", mkDenied (EditGroupingRuleAction nil))
    , ("UpdateGroupingRuleAction", mkDenied (UpdateGroupingRuleAction nil))
    , ("DeleteGroupingRuleAction", mkDenied (DeleteGroupingRuleAction nil))
    , ("PreviewGroupingRuleAction", mkDenied (PreviewGroupingRuleAction nil))
    , -- notification rules
      ("NewNotificationRuleAction", mkDenied NewNotificationRuleAction)
    , ("CreateNotificationRuleAction", mkDenied CreateNotificationRuleAction)
    , ("EditNotificationRuleAction", mkDenied (EditNotificationRuleAction nil))
    , ("UpdateNotificationRuleAction", mkDenied (UpdateNotificationRuleAction nil))
    , ("DeleteNotificationRuleAction", mkDenied (DeleteNotificationRuleAction nil))
    , -- escalation policies
      ("NewEscalationPolicyAction", mkDenied NewEscalationPolicyAction)
    , ("CreateEscalationPolicyAction", mkDenied CreateEscalationPolicyAction)
    , ("EditEscalationPolicyAction", mkDenied (EditEscalationPolicyAction nil))
    , ("UpdateEscalationPolicyAction", mkDenied (UpdateEscalationPolicyAction nil))
    , ("DeleteEscalationPolicyAction", mkDenied (DeleteEscalationPolicyAction nil))
    , -- admin / audit
      ("AdminAction", mkDenied AdminAction)
    , ("AdminRevokeApiTokenAction", mkDenied (AdminRevokeApiTokenAction nil))
    , ("AdminPurgeAlertsAction", mkDenied AdminPurgeAlertsAction)
    , ("AdminDatabaseAction", mkDenied AdminDatabaseAction)
    , ("AdminDbAnalyzeAction", mkDenied AdminDbAnalyzeAction)
    , ("AdminDbVacuumAction", mkDenied AdminDbVacuumAction)
    , ("AdminDbAnalyzeTableAction", mkDenied (AdminDbAnalyzeTableAction "users"))
    , ("ExportAuditAction", mkDenied ExportAuditAction)
    ]

withCtx :: (InitControllerContext WebApplication) => MockContext WebApplication -> ((ContextParameters WebApplication, Typeable WebApplication) => IO a) -> IO a
withCtx mockContext action =
    let ?request = mockContext.mockRequest
        ?respond = mockContext.mockRespond
        ?modelContext = mockContext.modelContext
        ?application = mockContext.application
        ?mocking = mockContext
     in action

newTestUser :: (?modelContext :: ModelContext) => Text -> IO User
newTestUser email =
    newRecord @User
        |> set #email email
        |> set #passwordHash "unused"
        |> createRecord

-- | Local replacement for IHP.Hspec.withIHPApp: upstream injects the test
-- database name via a hardcoded "/app" replace, which no-ops on our
-- "halemans" database and would run the matrix against the DEV database.
-- This one swaps the dbname path segment properly and drops the test
-- database afterwards.
withTestApp :: (MockContext WebApplication -> IO a) -> IO a
withTestApp action = do
    databaseUrl <- fromMaybe (error "DATABASE_URL not set") <$> lookupEnv "DATABASE_URL"
    ihpLib <- getEnv "IHP_LIB"
    dbName <- ("test_db_" <>) . Text.replace "-" "_" . UUID.toText <$> nextRandom
    let testUrl = injectDbName dbName (cs databaseUrl)
    bracket_
        (callProcess "psql" [cs databaseUrl, "-q", "-c", "CREATE DATABASE " <> cs dbName])
        (callProcess "psql" [cs databaseUrl, "-q", "-c", "DROP DATABASE IF EXISTS " <> cs dbName <> " WITH (FORCE)"])
        do
            callProcess "psql" [testUrl, "-q", "-f", ihpLib <> "/IHPSchema.sql", "-f", "Application/Schema.sql", "-f", "Application/Fixtures.sql"]
            frameworkConfig <- FrameworkConfig.buildFrameworkConfig noopLogger Config.config
            ModelSupport.withModelContext (cs testUrl) noopLogger \modelContext -> do
                mockRequest <- runTestMiddlewares frameworkConfig modelContext Nothing defaultRequest
                let mockRespond = const (pure ResponseReceived)
                let pgListener = Nothing
                let application = WebApplication
                action MockContext{..}
  where
    -- postgres:///halemans?host=... -> postgres:///test_db_x?host=...
    injectDbName dbName url =
        let (beforeDb, rest) = Text.breakOn "/halemans" url
         in cs (beforeDb <> "/" <> dbName <> Text.drop 9 rest)
