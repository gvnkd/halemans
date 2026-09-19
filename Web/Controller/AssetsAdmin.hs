module Web.Controller.AssetsAdmin where

import Application.Service.Assets (AssetsClient (..), clientFromConfig, connectionOk)
import qualified Application.Service.Log as Log
import Data.Functor ((<&>))
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Web.Controller.Prelude
import Web.View.AssetsAdmin.Edit
import Web.View.AssetsAdmin.Index
import Web.View.AssetsAdmin.New

-- Admin → Assets info sources (design_docs/milestone_8.md §8): assets_configs
-- CRUD with enable toggle and a connection test (listSchemas). Same
-- privilege as /admin/llm.
instance Controller AssetsAdminController where
    beforeAction = ensureIsUser

    action AssetsAdminAction = do
        requirePrivilege "manage_rules"
        statsRows <-
            sqlQueryTyped
                [typedSql|
            SELECT c.id, COUNT(o.id) AS cached_objects, MAX(o.fetched_at) AS newest_fetched_at
            FROM assets_configs c
            LEFT JOIN assets_objects o ON o.config_id = c.id
            GROUP BY c.id
        |]
        configs <-
            query @AssetsConfig
                |> orderByAsc #name
                |> fetch
        let statsFor configId = case [row | row <- statsRows, get #id row == configId] of
                (row : _) -> (get #cached_objects row, get #newest_fetched_at row)
                [] -> (0, Nothing)
        render IndexView{..}
    action NewAssetsConfigAction = do
        requirePrivilege "manage_rules"
        render NewView
    action CreateAssetsConfigAction = do
        requirePrivilege "manage_rules"
        let form = readForm
        case validateForm form of
            Just err -> do
                setErrorMessage err
                redirectTo NewAssetsConfigAction
            Nothing -> do
                existing <-
                    query @AssetsConfig
                        |> filterWhere (#name, form.name)
                        |> fetchOneOrNothing
                case existing of
                    Just _ -> do
                        setErrorMessage (trp "Info source {name} already exists" [("name", form.name)])
                        redirectTo NewAssetsConfigAction
                    Nothing -> do
                        _ <- createRecord (applyForm form (newRecord @AssetsConfig))
                        setSuccessMessage (trp "Created info source {name}" [("name", form.name)])
                        redirectTo AssetsAdminAction
    action EditAssetsConfigAction{configId} = do
        requirePrivilege "manage_rules"
        config <- fetch configId
        ensureNotProtected config.name (get #protected config)
        render EditView{..}
    action UpdateAssetsConfigAction{configId} = do
        requirePrivilege "manage_rules"
        config <- fetch configId
        ensureNotProtected config.name (get #protected config)
        let form = readForm
        case validateForm form of
            Just err -> do
                setErrorMessage err
                redirectTo (EditAssetsConfigAction configId)
            Nothing -> do
                clash <-
                    query @AssetsConfig
                        |> filterWhere (#name, form.name)
                        |> fetchOneOrNothing
                case clash of
                    Just other | get #id other /= configId -> do
                        setErrorMessage (trp "Info source {name} already exists" [("name", form.name)])
                        redirectTo (EditAssetsConfigAction configId)
                    _ -> do
                        now <- getCurrentTime
                        _ <- updateRecord (applyForm form config |> set #updatedAt now)
                        setSuccessMessage (trp "Updated info source {name}" [("name", form.name)])
                        redirectTo AssetsAdminAction
    action ToggleAssetsConfigAction{configId} = do
        requirePrivilege "manage_rules"
        config <- fetch configId
        ensureNotProtected config.name (get #protected config)
        now <- getCurrentTime
        _ <-
            config
                |> set #enabled (not config.enabled)
                |> set #updatedAt now
                |> updateRecord
        setSuccessMessage (trp (if config.enabled then "Disabled {name}" else "Enabled {name}") [("name", config.name)])
        redirectTo AssetsAdminAction

    -- assets_objects rows reference the config; enable/disable is the
    -- lifecycle, delete is only for unused configs (milestone_8.md §8).
    action DeleteAssetsConfigAction{configId} = do
        requirePrivilege "manage_rules"
        config <- fetch configId
        ensureNotProtected config.name (get #protected config)
        cached <-
            query @AssetsObject
                |> filterWhere (#configId, configId)
                |> fetchCount
        if cached > 0
            then setErrorMessage (trp "Cannot delete {name}: {count} cached objects reference it (disable instead)" [("name", config.name), ("count", tshow cached)])
            else do
                deleteRecord config
                setSuccessMessage (trp "Deleted info source {name}" [("name", config.name)])
        redirectTo AssetsAdminAction
    action TestAssetsConnectionAction{configId} = do
        requirePrivilege "manage_rules"
        config <- fetch configId
        clientResult <- clientFromConfig config
        case clientResult of
            Left err -> do
                let ?context = ?context.frameworkConfig
                Log.logWarn ("assets connection test failed: " <> err)
                setErrorMessage (trp "Assets config {name}: {error}" [("name", config.name), ("error", err)])
            Right client -> do
                result <- connectionOk client
                case result of
                    Right () -> setSuccessMessage (trp "Assets reachable at {url} ({name})" [("url", client.clientBaseUrl), ("name", config.name)])
                    Left err -> do
                        let ?context = ?context.frameworkConfig
                        Log.logWarn ("assets connection test failed: " <> err)
                        setErrorMessage (trp "Assets endpoint {url} did not answer: {error}" [("url", client.clientBaseUrl), ("error", err)])
        redirectTo AssetsAdminAction

data AssetsConfigForm = AssetsConfigForm
    { name :: Text
    , baseUrl :: Text
    , tokenEnv :: Text
    , authMode :: Text
    , jiraEmailEnv :: Maybe Text
    , defaultSchemaName :: Text
    , hostQueryTemplate :: Text
    , attributeNames :: Text
    }

readForm :: (?request :: Request) => AssetsConfigForm
readForm =
    AssetsConfigForm
        { name = param @Text "name"
        , baseUrl = Text.dropWhileEnd (== '/') (param @Text "baseUrl")
        , tokenEnv = param @Text "tokenEnv"
        , authMode = param @Text "authMode"
        , jiraEmailEnv = nonEmptyParam "jiraEmailEnv"
        , defaultSchemaName = param @Text "defaultSchemaName"
        , hostQueryTemplate = param @Text "hostQueryTemplate"
        , attributeNames = param @Text "attributeNames"
        }

validateForm :: (?request :: Request) => AssetsConfigForm -> Maybe Text
validateForm form
    | Text.null form.name = Just (tr "Name is required")
    | Text.null form.baseUrl = Just (tr "Base URL is required")
    | Text.null form.tokenEnv = Just (tr "Token env var is required")
    | form.authMode `notElem` ["bearer", "basic"] = Just (tr "Auth mode must be bearer or basic")
    | form.authMode == "basic" && isNothing form.jiraEmailEnv = Just (tr "Basic auth needs the Jira email env var")
    | otherwise = Nothing

applyForm :: AssetsConfigForm -> AssetsConfig -> AssetsConfig
applyForm form record =
    record
        |> set #name form.name
        |> set #baseUrl form.baseUrl
        |> set #tokenEnv form.tokenEnv
        |> set #authMode form.authMode
        |> set #jiraEmailEnv form.jiraEmailEnv
        |> set #defaultSchemaName form.defaultSchemaName
        |> set #hostQueryTemplate form.hostQueryTemplate
        |> set #attributeNames form.attributeNames
