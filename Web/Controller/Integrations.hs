module Web.Controller.Integrations where

import Web.Controller.Prelude
import Web.View.Integrations.Index
import qualified Application.Service.Cmdb as Cmdb
import qualified Application.Service.Jira as Jira
import qualified Application.Service.Log as Log
import System.Environment (lookupEnv)
import IHP.TypedSql (sqlQueryTyped, typedSql)

-- Admin → integrations (design_docs/milestone_3.md §8): connection tests
-- (env var presence + API ping) and CMDB cache stats.
instance Controller IntegrationsController where
    beforeAction = ensureIsUser

    action IntegrationsAction = do
        requirePrivilege "manage_sources"
        confluenceConfigured <- bothSet "HALEMANS_CONFLUENCE_URL" "CONFLUENCE_TOKEN"
        jiraConfigured <- bothSet "HALEMANS_JIRA_URL" "JIRA_TOKEN"
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
