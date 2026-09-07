module Web.View.Integrations.Index where
import Web.View.Prelude

data IndexView = IndexView
    { confluenceConfigured :: Bool
    , jiraConfigured :: Bool
    , cacheTotal :: Int64
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Integrations</h1>
        <table class="table" style="max-width: 700px" data-testid="integrations-table">
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

configuredBadge :: Bool -> Html
configuredBadge True = [hsx|<span class="badge status-resolved">yes</span>|]
configuredBadge False = [hsx|<span class="badge status-firing">no</span>|]

-- Connection test only makes sense with both env vars present; otherwise
-- render an inert button that names the missing configuration.
testButton :: Bool -> IntegrationsController -> Text -> Text -> Html
testButton configured action testId envNames
    | configured = [hsx|
        <form method="POST" action={action} class="d-inline">
            <button type="submit" class="btn btn-sm btn-outline-primary" data-testid={testId}>Test connection</button>
        </form>
    |]
    | otherwise = [hsx|
        <button class="btn btn-sm btn-outline-secondary" disabled={True} data-testid={testId} title={hint}>Test connection</button>
    |]
    where
        hint :: Text
        hint = "not configured — set " <> envNames <> " and restart"
