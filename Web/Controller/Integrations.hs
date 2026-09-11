module Web.Controller.Integrations where

import Web.Controller.Prelude
import Web.View.Integrations.Index
import Web.View.Integrations.JiraNew
import Web.View.Integrations.JiraEdit
import Web.View.Integrations.CmdbNew
import Web.View.Integrations.CmdbEdit
import qualified Application.Service.Cmdb as Cmdb
import qualified Application.Service.Jira as Jira
import qualified Application.Service.Log as Log
import System.Environment (lookupEnv)
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)

-- Admin → integrations (design_docs/milestone_3.md §8, milestone 10):
-- DB-resident Jira/Confluence connections (jira_configs/cmdb_configs, each
-- carrying MULTIPLE search projects/spaces) with connection tests, plus the
-- legacy env-var presence checks and CMDB cache stats.
instance Controller IntegrationsController where
    beforeAction = ensureIsUser

    action IntegrationsAction = do
        requirePrivilege "manage_sources"
        confluenceConfigured <- bothSet "HALEMANS_CONFLUENCE_URL" "CONFLUENCE_TOKEN"
        jiraConfigured <- bothSet "HALEMANS_JIRA_URL" "JIRA_TOKEN"
        jiraConfigs <- query @JiraConfig
            |> orderByAsc #name
            |> fetch
        cmdbConfigs <- query @CmdbConfig
            |> orderByAsc #name
            |> fetch
        cacheTotal <- countCmdbEntries
        render IndexView { .. }

    action TestConfluenceAction = do
        requirePrivilege "manage_sources"
        result <- Cmdb.cmdbEnvConfig "DEV"
            >>= maybe (pure (Left "HALEMANS_CONFLUENCE_URL / CONFLUENCE_TOKEN not set")) Cmdb.connectionOk
        case result of
            Right () -> setSuccessMessage "Confluence reachable"
            Left err -> do
                let ?context = ?context.frameworkConfig
                Log.logWarn ("confluence connection test failed: " <> err)
                setErrorMessage ("Confluence unreachable: " <> err)
        redirectTo IntegrationsAction

    action TestJiraAction = do
        requirePrivilege "manage_sources"
        result <- Jira.jiraEnvConfig "DEV"
            >>= maybe (pure (Left "HALEMANS_JIRA_URL / JIRA_TOKEN not set")) Jira.connectionOk
        case result of
            Right () -> setSuccessMessage "Jira reachable"
            Left err -> do
                let ?context = ?context.frameworkConfig
                Log.logWarn ("jira connection test failed: " <> err)
                setErrorMessage ("Jira unreachable: " <> err)
        redirectTo IntegrationsAction

    action NewJiraConfigAction = do
        requirePrivilege "manage_sources"
        render JiraNewView

    action CreateJiraConfigAction = do
        requirePrivilege "manage_sources"
        let form = readJiraForm
        case validateJiraForm form of
            Just err -> do
                setErrorMessage err
                redirectTo NewJiraConfigAction
            Nothing -> do
                existing <- query @JiraConfig
                    |> filterWhere (#name, form.jiraName)
                    |> fetchOneOrNothing
                case existing of
                    Just _ -> do
                        setErrorMessage ("Jira connection " <> form.jiraName <> " already exists")
                        redirectTo NewJiraConfigAction
                    Nothing -> do
                        _ <- createRecord (applyJiraForm form (newRecord @JiraConfig))
                        setSuccessMessage ("Created Jira connection " <> form.jiraName)
                        redirectTo IntegrationsAction

    action EditJiraConfigAction { jiraConfigId } = do
        requirePrivilege "manage_sources"
        config <- fetch jiraConfigId
        render JiraEditView { .. }

    action UpdateJiraConfigAction { jiraConfigId } = do
        requirePrivilege "manage_sources"
        config <- fetch jiraConfigId
        let form = readJiraForm
        case validateJiraForm form of
            Just err -> do
                setErrorMessage err
                redirectTo (EditJiraConfigAction jiraConfigId)
            Nothing -> do
                clash <- query @JiraConfig
                    |> filterWhere (#name, form.jiraName)
                    |> fetchOneOrNothing
                case clash of
                    Just other | get #id other /= jiraConfigId -> do
                        setErrorMessage ("Jira connection " <> form.jiraName <> " already exists")
                        redirectTo (EditJiraConfigAction jiraConfigId)
                    _ -> do
                        now <- getCurrentTime
                        _ <- updateRecord (applyJiraForm form config |> set #updatedAt now)
                        setSuccessMessage ("Updated Jira connection " <> form.jiraName)
                        redirectTo IntegrationsAction

    action ToggleJiraConfigAction { jiraConfigId } = do
        requirePrivilege "manage_sources"
        config <- fetch jiraConfigId
        now <- getCurrentTime
        _ <- config
            |> set #enabled (not config.enabled)
            |> set #updatedAt now
            |> updateRecord
        setSuccessMessage ((if config.enabled then "Disabled " else "Enabled ") <> config.name)
        redirectTo IntegrationsAction

    action DeleteJiraConfigAction { jiraConfigId } = do
        requirePrivilege "manage_sources"
        config <- fetch jiraConfigId
        deleteRecord config
        setSuccessMessage ("Deleted Jira connection " <> config.name)
        redirectTo IntegrationsAction

    action TestJiraConfigAction { jiraConfigId } = do
        requirePrivilege "manage_sources"
        config <- fetch jiraConfigId
        result <- jiraServiceConfig config >>= maybe (pure (Left "token env var not set")) Jira.connectionOk
        case result of
            Right () -> setSuccessMessage ("Jira reachable at " <> config.baseUrl <> " (" <> config.name <> ")")
            Left err -> do
                let ?context = ?context.frameworkConfig
                Log.logWarn ("jira connection test failed: " <> err)
                setErrorMessage ("Jira " <> config.name <> " unreachable: " <> err)
        redirectTo IntegrationsAction

    action NewCmdbConfigAction = do
        requirePrivilege "manage_sources"
        render CmdbNewView

    action CreateCmdbConfigAction = do
        requirePrivilege "manage_sources"
        let form = readCmdbForm
        case validateCmdbForm form of
            Just err -> do
                setErrorMessage err
                redirectTo NewCmdbConfigAction
            Nothing -> do
                existing <- query @CmdbConfig
                    |> filterWhere (#name, form.cmdbName)
                    |> fetchOneOrNothing
                case existing of
                    Just _ -> do
                        setErrorMessage ("CMDB connection " <> form.cmdbName <> " already exists")
                        redirectTo NewCmdbConfigAction
                    Nothing -> do
                        _ <- createRecord (applyCmdbForm form (newRecord @CmdbConfig))
                        setSuccessMessage ("Created CMDB connection " <> form.cmdbName)
                        redirectTo IntegrationsAction

    action EditCmdbConfigAction { cmdbConfigId } = do
        requirePrivilege "manage_sources"
        config <- fetch cmdbConfigId
        render CmdbEditView { .. }

    action UpdateCmdbConfigAction { cmdbConfigId } = do
        requirePrivilege "manage_sources"
        config <- fetch cmdbConfigId
        let form = readCmdbForm
        case validateCmdbForm form of
            Just err -> do
                setErrorMessage err
                redirectTo (EditCmdbConfigAction cmdbConfigId)
            Nothing -> do
                clash <- query @CmdbConfig
                    |> filterWhere (#name, form.cmdbName)
                    |> fetchOneOrNothing
                case clash of
                    Just other | get #id other /= cmdbConfigId -> do
                        setErrorMessage ("CMDB connection " <> form.cmdbName <> " already exists")
                        redirectTo (EditCmdbConfigAction cmdbConfigId)
                    _ -> do
                        now <- getCurrentTime
                        _ <- updateRecord (applyCmdbForm form config |> set #updatedAt now)
                        setSuccessMessage ("Updated CMDB connection " <> form.cmdbName)
                        redirectTo IntegrationsAction

    action ToggleCmdbConfigAction { cmdbConfigId } = do
        requirePrivilege "manage_sources"
        config <- fetch cmdbConfigId
        now <- getCurrentTime
        _ <- config
            |> set #enabled (not config.enabled)
            |> set #updatedAt now
            |> updateRecord
        setSuccessMessage ((if config.enabled then "Disabled " else "Enabled ") <> config.name)
        redirectTo IntegrationsAction

    action DeleteCmdbConfigAction { cmdbConfigId } = do
        requirePrivilege "manage_sources"
        config <- fetch cmdbConfigId
        deleteRecord config
        setSuccessMessage ("Deleted CMDB connection " <> config.name)
        redirectTo IntegrationsAction

    action TestCmdbConfigAction { cmdbConfigId } = do
        requirePrivilege "manage_sources"
        config <- fetch cmdbConfigId
        result <- cmdbServiceConfig config >>= maybe (pure (Left "token env var not set")) Cmdb.connectionOk
        case result of
            Right () -> setSuccessMessage ("Confluence reachable at " <> config.baseUrl <> " (" <> config.name <> ")")
            Left err -> do
                let ?context = ?context.frameworkConfig
                Log.logWarn ("confluence connection test failed: " <> err)
                setErrorMessage ("Confluence " <> config.name <> " unreachable: " <> err)
        redirectTo IntegrationsAction

-- Builds the service-layer config from a DB row, resolving token_env
-- against the process environment (the row holds the env var NAME only).
jiraServiceConfig :: JiraConfig -> IO (Maybe Jira.JiraConfig)
jiraServiceConfig row = do
    maybeToken <- lookupEnv (cs row.tokenEnv)
    pure case maybeToken of
        Nothing -> Nothing
        Just token ->
            let projects = stringList row.projects
            in Just Jira.JiraConfig
                { Jira.baseUrl = row.baseUrl
                , Jira.token = cs token
                , Jira.project = fromMaybe "" (head projects)
                , Jira.projects = projects
                , Jira.apiVersion = row.apiVersion
                }

cmdbServiceConfig :: CmdbConfig -> IO (Maybe Cmdb.CmdbConfig)
cmdbServiceConfig row = do
    maybeToken <- lookupEnv (cs row.tokenEnv)
    pure case maybeToken of
        Nothing -> Nothing
        Just token ->
            let spaces = stringList row.spaces
            in Just Cmdb.CmdbConfig
                { Cmdb.baseUrl = row.baseUrl
                , Cmdb.token = cs token
                , Cmdb.space = fromMaybe "" (head spaces)
                , Cmdb.spaces = spaces
                }

stringList :: Aeson.Value -> [Text]
stringList value = fromMaybe [] (parseMaybe Aeson.parseJSON value)

data JiraConfigForm = JiraConfigForm
    { jiraName :: Text
    , jiraBaseUrl :: Text
    , jiraTokenEnv :: Text
    , jiraApiVersion :: Text
    , jiraProjects :: [Text]
    }

readJiraForm :: (?request :: Request) => JiraConfigForm
readJiraForm = JiraConfigForm
    { jiraName = param @Text "name"
    , jiraBaseUrl = Text.dropWhileEnd (== '/') (param @Text "baseUrl")
    , jiraTokenEnv = param @Text "tokenEnv"
    , jiraApiVersion = param @Text "apiVersion"
    , jiraProjects = csvList (param @Text "projects")
    }

validateJiraForm :: JiraConfigForm -> Maybe Text
validateJiraForm form
    | Text.null form.jiraName = Just "Name is required"
    | Text.null form.jiraBaseUrl = Just "Base URL is required"
    | Text.null form.jiraTokenEnv = Just "Token env var is required"
    | form.jiraApiVersion `notElem` ["2", "3"] = Just "API version must be 2 or 3"
    | otherwise = Nothing

applyJiraForm :: JiraConfigForm -> JiraConfig -> JiraConfig
applyJiraForm form record = record
    |> set #name form.jiraName
    |> set #baseUrl form.jiraBaseUrl
    |> set #tokenEnv form.jiraTokenEnv
    |> set #apiVersion form.jiraApiVersion
    |> set #projects (Aeson.toJSON form.jiraProjects)

data CmdbConfigForm = CmdbConfigForm
    { cmdbName :: Text
    , cmdbBaseUrl :: Text
    , cmdbTokenEnv :: Text
    , cmdbSpaces :: [Text]
    }

readCmdbForm :: (?request :: Request) => CmdbConfigForm
readCmdbForm = CmdbConfigForm
    { cmdbName = param @Text "name"
    , cmdbBaseUrl = Text.dropWhileEnd (== '/') (param @Text "baseUrl")
    , cmdbTokenEnv = param @Text "tokenEnv"
    , cmdbSpaces = csvList (param @Text "spaces")
    }

validateCmdbForm :: CmdbConfigForm -> Maybe Text
validateCmdbForm form
    | Text.null form.cmdbName = Just "Name is required"
    | Text.null form.cmdbBaseUrl = Just "Base URL is required"
    | Text.null form.cmdbTokenEnv = Just "Token env var is required"
    | otherwise = Nothing

applyCmdbForm :: CmdbConfigForm -> CmdbConfig -> CmdbConfig
applyCmdbForm form record = record
    |> set #name form.cmdbName
    |> set #baseUrl form.cmdbBaseUrl
    |> set #tokenEnv form.cmdbTokenEnv
    |> set #spaces (Aeson.toJSON form.cmdbSpaces)

-- Comma-separated scope list (projects/spaces); empty input = no scope
-- clause, i.e. search everything the token can see.
csvList :: Text -> [Text]
csvList input = [item | item <- map Text.strip (Text.splitOn "," input), not (Text.null item)]

countCmdbEntries :: (?modelContext :: ModelContext) => IO Int64
countCmdbEntries = do
    rows <- sqlQueryTyped [typedSql| SELECT count(*) FROM cmdb_entries |]
    pure (fromMaybe 0 (head rows))

-- An integration counts as configured only when BOTH the URL and the token
-- are set — the connection test fails otherwise.
bothSet :: Text -> Text -> IO Bool
bothSet urlVar tokenVar = do
    url <- lookupEnv (cs urlVar)
    token <- lookupEnv (cs tokenVar)
    pure (isJust url && isJust token)
