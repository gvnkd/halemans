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
-- carrying MULTIPLE search projects/spaces) with connection tests, plus
-- CMDB/Jira cache stats. The legacy env-var fallback was removed in 2.0.
instance Controller IntegrationsController where
    beforeAction = ensureIsUser

    action IntegrationsAction = do
        requirePrivilege "manage_sources"
        jiraConfigs <- query @JiraConfig
            |> orderByAsc #name
            |> fetch
        cmdbConfigs <- query @CmdbConfig
            |> orderByAsc #name
            |> fetch
        cmdbCache <- cmdbCacheStats
        jiraCache <- jiraCacheStats
        render IndexView { .. }

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

-- CMDB cache stats. The freshness TTLs are the service-layer constants
-- (Cmdb.positiveTtlSeconds/negativeTtlSeconds), passed as params so this
-- query cannot drift from them.
cmdbCacheStats :: (?modelContext :: ModelContext) => IO CmdbCacheStats
cmdbCacheStats = do
    let positiveSecs = tshow (round Cmdb.positiveTtlSeconds :: Int)
        negativeSecs = tshow (round Cmdb.negativeTtlSeconds :: Int)
    rows <- sqlQueryTyped [typedSql|
        SELECT count(*) AS total,
               count(*) FILTER (WHERE page_id IS NULL) AS negatives,
               count(*) FILTER (WHERE (page_id IS NOT NULL AND fetched_at > now() - (${positiveSecs} || ' seconds')::interval)
                                   OR (page_id IS NULL AND fetched_at > now() - (${negativeSecs} || ' seconds')::interval)) AS fresh,
               max(fetched_at) AS last_fetched
        FROM cmdb_entries
    |]
    pure case head rows of
        Nothing -> CmdbCacheStats 0 0 0 Nothing
        Just row -> CmdbCacheStats
            { cmdbTotal = get #total row
            , cmdbFresh = get #fresh row
            , cmdbNegative = get #negatives row
            , cmdbLastFetch = get #last_fetched row
            }

-- Jira link cache stats. Stale = not synced within 3× the JiraSyncJob
-- cadence (5 min).
jiraCacheStats :: (?modelContext :: ModelContext) => IO JiraCacheStats
jiraCacheStats = do
    rows <- sqlQueryTyped [typedSql|
        SELECT count(*) AS total,
               count(*) FILTER (WHERE synced_at < now() - interval '15 minutes') AS stale,
               max(synced_at) AS last_synced
        FROM jira_links
    |]
    pure case head rows of
        Nothing -> JiraCacheStats 0 0 Nothing
        Just row -> JiraCacheStats
            { jiraTotal = get #total row
            , jiraStale = get #stale row
            , jiraLastSync = get #last_synced row
            }
