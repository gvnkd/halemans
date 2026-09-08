module Web.Controller.AssetsAdmin where

import Web.Controller.Prelude
import Web.View.AssetsAdmin.Index
import Web.View.AssetsAdmin.New
import Web.View.AssetsAdmin.Edit
import Application.Service.Assets (AssetsClient (..), clientFromConfig, connectionOk)
import qualified Application.Service.Log as Log
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Data.Functor ((<&>))
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)

-- Admin → Assets info sources (design_docs/milestone_8.md §8): assets_configs
-- CRUD with enable toggle and a connection test (listSchemas). Same
-- privilege as /admin/llm.
instance Controller AssetsAdminController where
    beforeAction = ensureIsUser

    action AssetsAdminAction = do
        requirePrivilege "manage_rules"
        statsRows <- sqlQueryTyped [typedSql|
            SELECT c.id, COUNT(o.id) AS cached_objects, MAX(o.fetched_at) AS newest_fetched_at
            FROM assets_configs c
            LEFT JOIN assets_objects o ON o.config_id = c.id
            GROUP BY c.id
        |]
        configs <- query @AssetsConfig
            |> orderByAsc #name
            |> fetch
        let statsFor configId = case [row | row <- statsRows, get #id row == configId] of
                (row:_) -> (get #cached_objects row, get #newest_fetched_at row)
                [] -> (0, Nothing)
        render IndexView { .. }

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
                existing <- query @AssetsConfig
                    |> filterWhere (#name, form.name)
                    |> fetchOneOrNothing
                case existing of
                    Just _ -> do
                        setErrorMessage ("Info source " <> form.name <> " already exists")
                        redirectTo NewAssetsConfigAction
                    Nothing -> do
                        _ <- createRecord (applyForm form (newRecord @AssetsConfig))
                        setSuccessMessage ("Created info source " <> form.name)
                        redirectTo AssetsAdminAction

    action EditAssetsConfigAction { configId } = do
        requirePrivilege "manage_rules"
        config <- fetch configId
        render EditView { .. }

    action UpdateAssetsConfigAction { configId } = do
        requirePrivilege "manage_rules"
        config <- fetch configId
        let form = readForm
        case validateForm form of
            Just err -> do
                setErrorMessage err
                redirectTo (EditAssetsConfigAction configId)
            Nothing -> do
                clash <- query @AssetsConfig
                    |> filterWhere (#name, form.name)
                    |> fetchOneOrNothing
                case clash of
                    Just other | get #id other /= configId -> do
                        setErrorMessage ("Info source " <> form.name <> " already exists")
                        redirectTo (EditAssetsConfigAction configId)
                    _ -> do
                        now <- getCurrentTime
                        _ <- updateRecord (applyForm form config |> set #updatedAt now)
                        setSuccessMessage ("Updated info source " <> form.name)
                        redirectTo AssetsAdminAction

    action ToggleAssetsConfigAction { configId } = do
        requirePrivilege "manage_rules"
        config <- fetch configId
        now <- getCurrentTime
        _ <- config
            |> set #enabled (not config.enabled)
            |> set #updatedAt now
            |> updateRecord
        setSuccessMessage ((if config.enabled then "Disabled " else "Enabled ") <> config.name)
        redirectTo AssetsAdminAction

    -- assets_objects rows reference the config; enable/disable is the
    -- lifecycle, delete is only for unused configs (milestone_8.md §8).
    action DeleteAssetsConfigAction { configId } = do
        requirePrivilege "manage_rules"
        config <- fetch configId
        cached <- query @AssetsObject
            |> filterWhere (#configId, configId)
            |> fetchCount
        if cached > 0
            then setErrorMessage ("Cannot delete " <> config.name <> ": " <> tshow cached <> " cached objects reference it (disable instead)")
            else do
                deleteRecord config
                setSuccessMessage ("Deleted info source " <> config.name)
        redirectTo AssetsAdminAction

    action TestAssetsConnectionAction { configId } = do
        requirePrivilege "manage_rules"
        config <- fetch configId
        clientResult <- clientFromConfig config
        case clientResult of
            Left err -> do
                let ?context = ?context.frameworkConfig
                Log.logWarn ("assets connection test failed: " <> err)
                setErrorMessage ("Assets config " <> config.name <> ": " <> err)
            Right client -> do
                result <- connectionOk client
                case result of
                    Right () -> setSuccessMessage ("Assets reachable at " <> client.clientBaseUrl <> " (" <> config.name <> ")")
                    Left err -> do
                        let ?context = ?context.frameworkConfig
                        Log.logWarn ("assets connection test failed: " <> err)
                        setErrorMessage ("Assets endpoint " <> client.clientBaseUrl <> " did not answer: " <> err)
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
readForm = AssetsConfigForm
    { name = param @Text "name"
    , baseUrl = Text.dropWhileEnd (== '/') (param @Text "baseUrl")
    , tokenEnv = param @Text "tokenEnv"
    , authMode = param @Text "authMode"
    , jiraEmailEnv = nonEmptyParam "jiraEmailEnv"
    , defaultSchemaName = param @Text "defaultSchemaName"
    , hostQueryTemplate = param @Text "hostQueryTemplate"
    , attributeNames = param @Text "attributeNames"
    }

validateForm :: AssetsConfigForm -> Maybe Text
validateForm form
    | Text.null form.name = Just "Name is required"
    | Text.null form.baseUrl = Just "Base URL is required"
    | Text.null form.tokenEnv = Just "Token env var is required"
    | form.authMode `notElem` ["bearer", "basic"] = Just "Auth mode must be bearer or basic"
    | form.authMode == "basic" && isNothing form.jiraEmailEnv = Just "Basic auth needs the Jira email env var"
    | otherwise = Nothing

applyForm :: AssetsConfigForm -> AssetsConfig -> AssetsConfig
applyForm form record = record
    |> set #name form.name
    |> set #baseUrl form.baseUrl
    |> set #tokenEnv form.tokenEnv
    |> set #authMode form.authMode
    |> set #jiraEmailEnv form.jiraEmailEnv
    |> set #defaultSchemaName form.defaultSchemaName
    |> set #hostQueryTemplate form.hostQueryTemplate
    |> set #attributeNames form.attributeNames
