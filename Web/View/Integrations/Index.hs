module Web.View.Integrations.Index where
import Web.View.Prelude
import Web.View.Fragments (inlinePostFormHtml, stateBadgeHtml, sectionHeaderHtml)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

data IndexView = IndexView
    { confluenceConfigured :: Bool
    , jiraConfigured :: Bool
    , jiraConfigs :: [JiraConfig]
    , cmdbConfigs :: [CmdbConfig]
    , cacheTotal :: Int64
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Integrations</h1>

        {sectionHeaderHtml "Jira connections" newJiraButton}
        <table class="table" data-testid="jira-configs">
            <thead>
                <tr><th>Name</th><th>Base URL</th><th>API</th><th>Projects</th><th>Enabled</th><th></th></tr>
            </thead>
            <tbody>
                {forEach jiraConfigs jiraConfigRowHtml}
            </tbody>
        </table>

        {sectionHeaderHtml "CMDB (Confluence) connections" newCmdbButton}
        <table class="table" data-testid="cmdb-configs">
            <thead>
                <tr><th>Name</th><th>Base URL</th><th>Spaces</th><th>Enabled</th><th></th></tr>
            </thead>
            <tbody>
                {forEach cmdbConfigs cmdbConfigRowHtml}
            </tbody>
        </table>

        <h2 class="mt-4">Legacy env-var fallback</h2>
        <table class="table maxw-700" data-testid="integrations-table">
            <thead>
                <tr><th>Integration</th><th>Configured</th><th></th></tr>
            </thead>
            <tbody>
                <tr data-testid="integration-confluence">
                    <td>Confluence (CMDB)</td>
                    <td>{configuredBadge confluenceConfigured}</td>
                    <td>{testButton confluenceConfigured TestConfluenceAction "test-confluence" "HALEMANS_CONFLUENCE_URL / CONFLUENCE_TOKEN"}</td>
                </tr>
                <tr data-testid="integration-jira">
                    <td>Jira</td>
                    <td>{configuredBadge jiraConfigured}</td>
                    <td>{testButton jiraConfigured TestJiraAction "test-jira" "HALEMANS_JIRA_URL / JIRA_TOKEN"}</td>
                </tr>
            </tbody>
        </table>

        <h2>CMDB cache</h2>
        <p data-testid="cmdb-cache-stats">{cacheTotal} cached entries</p>
    |]
        where
            newJiraButton = [hsx|<a href={NewJiraConfigAction} class="btn btn-sm btn-primary" data-testid="new-jira-config">New Jira connection</a>|]
            newCmdbButton = [hsx|<a href={NewCmdbConfigAction} class="btn btn-sm btn-primary" data-testid="new-cmdb-config">New CMDB connection</a>|]

jiraConfigRowHtml :: JiraConfig -> Html
jiraConfigRowHtml config = [hsx|
    <tr data-testid="jira-config">
        <td>{config.name}</td>
        <td data-testid="jira-config-base-url-cell">{config.baseUrl}</td>
        <td>{config.apiVersion}</td>
        <td data-testid="jira-config-projects-cell">{scopeText config.projects}</td>
        <td>{stateBadgeHtml config.enabled "jira-config"}</td>
        <td>
            <a href={EditJiraConfigAction configId} class="btn btn-sm btn-outline-secondary" data-testid="jira-config-edit">Edit</a>
            {toggleForm}
            {inlinePostFormHtml (pathTo (TestJiraConfigAction configId)) "Test" "btn btn-sm btn-outline-primary" (Just "jira-config-test") False}
            {inlinePostFormHtml (pathTo (DeleteJiraConfigAction configId)) "Delete" "btn btn-sm btn-outline-danger" (Just "jira-config-delete") True}
        </td>
    </tr>
|]
    where
        configId = get #id config
        toggleForm = inlinePostFormHtml (pathTo (ToggleJiraConfigAction configId)) toggleLabel "btn btn-sm btn-outline-warning" (Just "jira-config-toggle") False
        toggleLabel :: Text
        toggleLabel = if config.enabled then "Disable" else "Enable"

cmdbConfigRowHtml :: CmdbConfig -> Html
cmdbConfigRowHtml config = [hsx|
    <tr data-testid="cmdb-config">
        <td>{config.name}</td>
        <td data-testid="cmdb-config-base-url-cell">{config.baseUrl}</td>
        <td data-testid="cmdb-config-spaces-cell">{scopeText config.spaces}</td>
        <td>{stateBadgeHtml config.enabled "cmdb-config"}</td>
        <td>
            <a href={EditCmdbConfigAction configId} class="btn btn-sm btn-outline-secondary" data-testid="cmdb-config-edit">Edit</a>
            {toggleForm}
            {inlinePostFormHtml (pathTo (TestCmdbConfigAction configId)) "Test" "btn btn-sm btn-outline-primary" (Just "cmdb-config-test") False}
            {inlinePostFormHtml (pathTo (DeleteCmdbConfigAction configId)) "Delete" "btn btn-sm btn-outline-danger" (Just "cmdb-config-delete") True}
        </td>
    </tr>
|]
    where
        configId = get #id config
        toggleForm = inlinePostFormHtml (pathTo (ToggleCmdbConfigAction configId)) toggleLabel "btn btn-sm btn-outline-warning" (Just "cmdb-config-toggle") False
        toggleLabel :: Text
        toggleLabel = if config.enabled then "Disable" else "Enable"

scopeText :: Aeson.Value -> Text
scopeText value = case fromMaybe [] (parseMaybe Aeson.parseJSON value) of
    [] -> "all"
    scopes -> Text.intercalate ", " scopes

configuredBadge :: Bool -> Html
configuredBadge True = [hsx|<span class="badge status-resolved">yes</span>|]
configuredBadge False = [hsx|<span class="badge status-firing">no</span>|]

-- Connection test only makes sense with both env vars present; otherwise
-- render an inert button that names the missing configuration.
testButton :: Bool -> IntegrationsController -> Text -> Text -> Html
testButton configured action testId envNames
    | configured = inlinePostFormHtml (pathTo action) "Test connection" "btn btn-sm btn-outline-primary" (Just testId) False
    | otherwise = [hsx|
        <button class="btn btn-sm btn-outline-secondary" disabled={True} data-testid={testId} title={hint}>Test connection</button>
    |]
    where
        hint :: Text
        hint = "not configured — set " <> envNames <> " and restart"
