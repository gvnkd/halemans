module Web.Controller.Integrations where

import Web.Controller.Prelude
import Web.View.Integrations.Index
import qualified Application.Service.Cmdb as Cmdb
import qualified Application.Service.Jira as Jira
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
        result <- testConfluence
        case result of
            True -> setSuccessMessage "Confluence reachable"
            False -> setErrorMessage "Confluence unreachable (check HALEMANS_CONFLUENCE_URL / CONFLUENCE_TOKEN)"
        redirectTo IntegrationsAction

    action TestJiraAction = do
        requirePrivilege "manage_sources"
        result <- testJira
        case result of
            True -> setSuccessMessage "Jira reachable"
            False -> setErrorMessage "Jira unreachable (check HALEMANS_JIRA_URL / JIRA_TOKEN)"
        redirectTo IntegrationsAction

testConfluence :: (?modelContext :: ModelContext) => IO Bool
testConfluence = do
    url <- lookupEnv "HALEMANS_CONFLUENCE_URL"
    token <- lookupEnv "CONFLUENCE_TOKEN"
    case (url, token) of
        (Just url, Just token) -> Cmdb.connectionOk (Cmdb.CmdbConfig (cs url) (cs token) "DEV")
        _ -> pure False

testJira :: (?modelContext :: ModelContext) => IO Bool
testJira = do
    url <- lookupEnv "HALEMANS_JIRA_URL"
    token <- lookupEnv "JIRA_TOKEN"
    case (url, token) of
        (Just url, Just token) -> Jira.connectionOk (Jira.JiraConfig (cs url) (cs token) "DEV")
        _ -> pure False

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
