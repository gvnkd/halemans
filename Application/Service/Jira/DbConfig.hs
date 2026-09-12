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
import Application.Helper.Json (stringList)
import Data.Functor ((<&>))
import System.Environment (lookupEnv)

-- DB-only Jira config resolution (milestone 10; env fallback removed in
-- 2.0) — separate module because the generated JiraConfig record shares
-- field names with the service one.

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

-- All usable configs: the enabled DB rows.
currentJiraConfigs :: (?modelContext :: ModelContext) => IO [Jira.JiraConfig]
currentJiraConfigs = jiraConfigsFromDb

-- Alert-scoped resolution (enrichment, ticket creation): the enabled DB
-- rows, with the source's own scope override (jiraProjects) replacing every
-- connection's project list when set.
jiraConfigsForSource :: (?modelContext :: ModelContext) => Source -> IO [Jira.JiraConfig]
jiraConfigsForSource source = do
    dbConfigs <- jiraConfigsFromDb
    pure case Jira.sourceProjectOverride source of
        [] -> dbConfigs
        projects -> map (applyScope projects) dbConfigs
    where
        applyScope projects config = config
            { Jira.projects = projects
            , Jira.project = fromMaybe "" (head projects)
            }

-- Auto-link (milestone_3.md §5): top 5 open tickets matching the alert
-- subject become jira_links rows with origin 'auto'. Every configured Jira
-- connection is searched across ALL its projects (milestone 10). Nothing
-- configured is a silent skip, not a failure (unconfigured installs must
-- not spam enrichment_failed + retries); a total search failure (every
-- config errors) yields Left.
autoLinkForAlert :: (?modelContext :: ModelContext) => Source -> Alert -> IO (Either Text [JiraLink])
autoLinkForAlert source alert = do
    configs <- jiraConfigsForSource source
    if null configs
        then pure (Right [])
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

-- Manual creation (milestone_3.md §5): the target project is the first of
-- the resolved scope — the source's jiraProjects override when set, else
-- the connection's first configured project (jiraConfigsForSource already
-- applied the override).
createTicketForAlert :: (?modelContext :: ModelContext) => Source -> Alert -> Text -> Text -> Text -> IO (Either Text JiraLink)
createTicketForAlert source alert issueType summary body = do
    configs <- jiraConfigsForSource source
    case configs of
        [] -> pure (Left "jira not configured")
        (config:_) ->
            Jira.createTicketWithConfig config (get #id alert) issueType summary body

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
