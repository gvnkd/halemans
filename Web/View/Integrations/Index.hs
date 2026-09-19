module Web.View.Integrations.Index where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as Text
import Web.View.Fragments (inlinePostFormHtml, sectionHeaderHtml, stateBadgeHtml)
import Web.View.Prelude

data IndexView = IndexView
    { jiraConfigs :: [JiraConfig]
    , cmdbConfigs :: [CmdbConfig]
    , cmdbCache :: CmdbCacheStats
    , jiraCache :: JiraCacheStats
    }

data CmdbCacheStats = CmdbCacheStats
    { cmdbTotal :: Int64
    , cmdbFresh :: Int64
    , cmdbNegative :: Int64
    , cmdbLastFetch :: Maybe UTCTime
    }

data JiraCacheStats = JiraCacheStats
    { jiraTotal :: Int64
    , jiraStale :: Int64
    , jiraLastSync :: Maybe UTCTime
    }

instance View IndexView where
    html IndexView{..} =
        [hsx|
        <h1>{tr "Integrations"}</h1>

        {sectionHeaderHtml (tr "Jira connections") newJiraButton}
        <table class="table" data-testid="jira-configs">
            <thead>
                <tr><th>{tr "Name"}</th><th>{tr "Base URL"}</th><th>{tr "API"}</th><th>{tr "Projects"}</th><th>{tr "Enabled"}</th><th></th></tr>
            </thead>
            <tbody>
                {forEach jiraConfigs jiraConfigRowHtml}
            </tbody>
        </table>

        {sectionHeaderHtml (tr "CMDB (Confluence) connections") newCmdbButton}
        <table class="table" data-testid="cmdb-configs">
            <thead>
                <tr><th>{tr "Name"}</th><th>{tr "Base URL"}</th><th>{tr "Spaces"}</th><th>{tr "Enabled"}</th><th></th></tr>
            </thead>
            <tbody>
                {forEach cmdbConfigs cmdbConfigRowHtml}
            </tbody>
        </table>

        <h2 class="mt-4">{tr "CMDB cache"}</h2>
        <p data-testid="cmdb-cache-stats">
            {trp "Cached entries: {total} (fresh: {fresh}, negative lookups: {negative})" [("total", tshow cmdbCache.cmdbTotal), ("fresh", tshow cmdbCache.cmdbFresh), ("negative", tshow cmdbCache.cmdbNegative)]}
            {cmdbLastFetch}
        </p>

        <h2>{tr "Jira cache"}</h2>
        <p data-testid="jira-cache-stats">
            {trp "Tracked tickets: {total} (awaiting sync: {stale})" [("total", tshow jiraCache.jiraTotal), ("stale", tshow jiraCache.jiraStale)]}
            {jiraLastSync}
        </p>
    |]
      where
        newJiraButton = [hsx|<a href={NewJiraConfigAction} class="btn btn-sm btn-primary" data-testid="new-jira-config">{tr "New Jira connection"}</a>|]
        newCmdbButton = [hsx|<a href={NewCmdbConfigAction} class="btn btn-sm btn-primary" data-testid="new-cmdb-config">{tr "New CMDB connection"}</a>|]
        cmdbLastFetch = case cmdbCache.cmdbLastFetch of
            Nothing -> mempty
            Just fetchedAt -> [hsx| — {tr "last fetch"} {utcTimeHtml fetchedAt}|]
        jiraLastSync = case jiraCache.jiraLastSync of
            Nothing -> mempty
            Just syncedAt -> [hsx| — {tr "last sync"} {utcTimeHtml syncedAt}|]

jiraConfigRowHtml :: JiraConfig -> Html
jiraConfigRowHtml config =
    [hsx|
    <tr data-testid="jira-config">
        <td>{config.name} {protectedBadgeHtml (get #protected config)}</td>
        <td data-testid="jira-config-base-url-cell">{config.baseUrl}</td>
        <td>{config.apiVersion}</td>
        <td data-testid="jira-config-projects-cell">{scopeText config.projects}</td>
        <td>{stateBadgeHtml config.enabled "jira-config"}</td>
        <td>
            <a href={EditJiraConfigAction configId} class="btn btn-sm btn-outline-secondary" data-testid="jira-config-edit">{tr "Edit"}</a>
            {toggleForm}
            {inlinePostFormHtml (pathTo (TestJiraConfigAction configId)) (tr "Test") "btn btn-sm btn-outline-primary" (Just "jira-config-test") False}
            {inlinePostFormHtml (pathTo (DeleteJiraConfigAction configId)) (tr "Delete") "btn btn-sm btn-outline-danger" (Just "jira-config-delete") True}
        </td>
    </tr>
|]
  where
    configId = get #id config
    toggleForm = inlinePostFormHtml (pathTo (ToggleJiraConfigAction configId)) toggleLabel "btn btn-sm btn-outline-warning" (Just "jira-config-toggle") False
    toggleLabel :: Text
    toggleLabel = if config.enabled then tr "Disable" else tr "Enable"

cmdbConfigRowHtml :: CmdbConfig -> Html
cmdbConfigRowHtml config =
    [hsx|
    <tr data-testid="cmdb-config">
        <td>{config.name} {protectedBadgeHtml (get #protected config)}</td>
        <td data-testid="cmdb-config-base-url-cell">{config.baseUrl}</td>
        <td data-testid="cmdb-config-spaces-cell">{scopeText config.spaces}</td>
        <td>{stateBadgeHtml config.enabled "cmdb-config"}</td>
        <td>
            <a href={EditCmdbConfigAction configId} class="btn btn-sm btn-outline-secondary" data-testid="cmdb-config-edit">{tr "Edit"}</a>
            {toggleForm}
            {inlinePostFormHtml (pathTo (TestCmdbConfigAction configId)) (tr "Test") "btn btn-sm btn-outline-primary" (Just "cmdb-config-test") False}
            {inlinePostFormHtml (pathTo (DeleteCmdbConfigAction configId)) (tr "Delete") "btn btn-sm btn-outline-danger" (Just "cmdb-config-delete") True}
        </td>
    </tr>
|]
  where
    configId = get #id config
    toggleForm = inlinePostFormHtml (pathTo (ToggleCmdbConfigAction configId)) toggleLabel "btn btn-sm btn-outline-warning" (Just "cmdb-config-toggle") False
    toggleLabel :: Text
    toggleLabel = if config.enabled then tr "Disable" else tr "Enable"

scopeText :: (?request :: Request) => Aeson.Value -> Text
scopeText value = case fromMaybe [] (parseMaybe Aeson.parseJSON value) of
    [] -> tr "all"
    scopes -> Text.intercalate ", " scopes
