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
                    <td>
                        <form method="POST" action={TestConfluenceAction} class="d-inline">
                            <button type="submit" class="btn btn-sm btn-outline-primary" data-testid="test-confluence">Test connection</button>
                        </form>
                    </td>
                </tr>
                <tr data-testid="integration-jira">
                    <td>Jira</td>
                    <td>{configuredBadge jiraConfigured}</td>
                    <td>
                        <form method="POST" action={TestJiraAction} class="d-inline">
                            <button type="submit" class="btn btn-sm btn-outline-primary" data-testid="test-jira">Test connection</button>
                        </form>
                    </td>
                </tr>
            </tbody>
        </table>

        <h2>CMDB cache</h2>
        <p data-testid="cmdb-cache-stats">{cacheTotal} cached entries</p>
    |]

configuredBadge :: Bool -> Html
configuredBadge True = [hsx|<span class="badge status-resolved">yes</span>|]
configuredBadge False = [hsx|<span class="badge status-firing">no</span>|]
