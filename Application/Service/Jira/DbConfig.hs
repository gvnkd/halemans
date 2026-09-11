module Application.Service.Jira.DbConfig
( jiraConfigsFromDb
, currentJiraConfigs
, jiraConfigsForSource
, autoLinkForAlert
, createTicketForAlert
, syncOpenLinks
) where

import IHP.Prelude
import IHP.ModelSupport (ModelContext, newRecord, createRecord, updateRecord, Id' (..))
import IHP.HaskellSupport (set, get)
import IHP.QueryBuilder (query, filterWhere, filterWhereNot, filterWhereIn, orderByAsc)
import IHP.Fetch (fetch)
import Generated.Types (Source, Alert, Alert' (..), JiraLink, JiraLink' (..), JiraConfig, JiraConfig' (..))
import qualified Application.Service.Jira as Jira
import Application.Service.Jira (JiraIssue (..))
import Data.Aeson (Value)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import Data.Functor ((<&>))
import System.Environment (lookupEnv)

-- DB-first Jira config resolution (milestone 10), same shape as
-- Application.Service.Llm.DbConfig — separate module because the generated
-- JiraConfig record shares field names with the service one. DB enabled rows
-- win; the env-based per-source fallback keeps pre-milestone-10 installs
-- working unchanged.

-- Enabled jira_configs rows with their token resolved (token_env holds the
-- env var NAME, never the secret). Rows whose env var is unset are skipped.
jiraConfigsFromDb :: (?modelContext :: ModelContext) => IO [Jira.JiraConfig]
jiraConfigsFromDb = do
    rows <- query @JiraConfig
        |> filterWhere (#enabled, True)
        |> orderByAsc #name
        |> fetch
    catMaybes <$> forM rows \row -> do
        maybeToken <- lookupEnv (cs (get #tokenEnv row))
        pure case maybeToken of
            Nothing -> Nothing
            Just token ->
                let projects = stringList (get #projects row)
                in Just Jira.JiraConfig
                    { Jira.baseUrl = get #baseUrl row
                    , Jira.token = cs token
                    , Jira.project = fromMaybe "" (head projects)
                    , Jira.projects = projects
                    , Jira.apiVersion = get #apiVersion row
                    }

stringList :: Value -> [Text]
stringList value = fromMaybe [] (parseMaybe Aeson.parseJSON value)

-- All usable configs: enabled DB rows, else the legacy env config taken from
-- the first source that has credentials (pre-milestone-10 behaviour).
currentJiraConfigs :: (?modelContext :: ModelContext) => IO [Jira.JiraConfig]
currentJiraConfigs = do
    dbConfigs <- jiraConfigsFromDb
    if null dbConfigs
        then do
            sources <- query @Source |> fetch
            envConfigs <- forM sources Jira.jiraConfigFromEnv
            pure (maybeToList (foldr (<|>) Nothing envConfigs))
        else pure dbConfigs

-- Alert-scoped resolution (enrichment, ticket creation): DB rows when
-- present, otherwise the source's own env-based config (per-source
-- jiraProject honoured by the fallback).
jiraConfigsForSource :: (?modelContext :: ModelContext) => Source -> IO [Jira.JiraConfig]
jiraConfigsForSource source = do
    dbConfigs <- jiraConfigsFromDb
    if null dbConfigs
        then maybeToList <$> Jira.jiraConfigFromEnv source
        else pure dbConfigs

-- Auto-link (milestone_3.md §5): top 5 open tickets matching the alert
-- subject become jira_links rows with origin 'auto'. Every configured Jira
-- connection is searched across ALL its projects (milestone 10). Only a
-- total failure (every config errors, or nothing configured) yields Left.
autoLinkForAlert :: (?modelContext :: ModelContext) => Source -> Alert -> IO (Either Text [JiraLink])
autoLinkForAlert source alert = do
    configs <- jiraConfigsForSource source
    if null configs
        then pure (Left "jira not configured")
        else do
            results <- forM configs \config -> do
                result <- Jira.searchIssues config (Jira.jqlForAlert (Jira.projects config) alert) 5
                pure (result <&> map (config,))
            case [err | Left err <- results] of
                errs | length errs == length configs -> pure (Left (fromMaybe "jira search failed" (head errs)))
                _ -> do
                    let found = take 5 [pair | Right pairs <- results, pair <- pairs]
                    Right <$> forM found \(config, issue) ->
                        Jira.upsertLink config (get #id alert) "auto" issue

-- Manual creation (milestone_3.md §5): the source's own jiraProject is the
-- creation target when set, else the first configured project.
createTicketForAlert :: (?modelContext :: ModelContext) => Source -> Alert -> Text -> Text -> Text -> IO (Either Text JiraLink)
createTicketForAlert source alert issueType summary body = do
    configs <- jiraConfigsForSource source
    case configs of
        [] -> pure (Left "jira not configured")
        (config:_) -> do
            let targetProject = fromMaybe (Jira.project config) (Jira.sourceConfigText "jiraProject" source)
                createConfig = config { Jira.project = targetProject }
            Jira.createTicketWithConfig createConfig (get #id alert) issueType summary body

-- JiraSyncJob body (milestone_3.md §5): refresh status/summary of every link
-- whose alert is not closed. Returns the number of links refreshed. Each
-- link is tried against every configured connection until one answers
-- (milestone 10: links may live in different Jira projects/instances).
syncOpenLinks :: (?modelContext :: ModelContext) => IO Int
syncOpenLinks = do
    openAlerts <- query @Alert
        |> filterWhereNot (#status, "closed" :: Text)
        |> fetch
    links <- case openAlerts of
        [] -> pure []
        alerts -> query @JiraLink
            |> filterWhereIn (#alertId, map (get #id) alerts)
            |> fetch
    configs <- currentJiraConfigs
    if null configs
        then pure 0
        else do
            refreshed <- forM links \link -> do
                result <- firstIssue configs link.ticketKey
                case result of
                    Nothing -> pure False
                    Just issue -> do
                        now <- getCurrentTime
                        _ <- link
                            |> set #summary issue.issueSummary
                            |> set #status issue.issueStatus
                            |> set #syncedAt now
                            |> updateRecord
                        pure True
            pure (length (filter (\did -> did) refreshed))
    where
        firstIssue [] _ = pure Nothing
        firstIssue (config:rest) key = do
            result <- Jira.getIssue config key
            case result of
                Right issue -> pure (Just issue)
                Left _ -> firstIssue rest key
