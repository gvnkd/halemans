module Web.View.Admin.Index where

import Application.Service.JobMetrics (FailedJobRow (..), JobTypeMetrics (..))
import qualified Data.Text as Text
import Web.View.Fragments (emptyStateHtml, inlinePostFormHtml, pageHeaderHtml, sectionHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView
    { metrics :: [JobTypeMetrics]
    , failures :: [FailedJobRow]
    , apiTokens :: [(ApiToken, Text)]
    }

instance View IndexView where
    beforeRender _ = setPageTitle (tr "Jobs")
    html IndexView{..} =
        [hsx|
    <div>
        {pageHeaderHtml (tr "Admin") mempty}
        {sectionHeaderHtml (tr "Job metrics (last 24h)") mempty}
        {metricsTable}
        {sectionHeaderHtml (tr "Recent job failures") mempty}
        {failuresTable}
        {sectionHeaderHtml (tr "API tokens") mempty}
        {tokensTable}
        {sectionHeaderHtml (tr "Provisioning") provisionLinks}
        <div class="card"><div class="card-body">
            <p class="text-muted mb-2">{tr "Snapshot of users, roles, sources, teams, LLM config and agent roles, field mappings, dashboards, grouping, notification and escalation rules and integrations in the provision format. Webhook tokens are exported as env references when the token value matches a process env var; tokens with no env match and hostGroupsFile are not exported."}</p>
        </div></div>
        {sectionHeaderHtml (tr "Danger zone") mempty}
        <div class="card"><div class="card-body">
            <form method="POST" action={AdminPurgeAlertsAction} data-confirm={tr "Delete ALL alerts, groups, events, comments and analyses? This cannot be undone."}>
                <button type="submit" class="btn btn-ghost btn-ghost-critical" data-testid="purge-alerts">{tr "Purge all alerts"}</button>
            </form>
        </div></div>
    </div>
    |]
      where
        metricsTable =
            if null metrics
                then emptyStateHtml "job-metrics-empty" (tr "No jobs recorded in the last 24 hours.")
                else
                    [hsx|
                    <table class="table" data-testid="job-metrics-table">
                        <thead>
                            <tr>
                                <th>{tr "Job type"}</th>
                                <th>{tr "Succeeded"}</th>
                                <th>{tr "Retried"}</th>
                                <th>{tr "Failed"}</th>
                            </tr>
                        </thead>
                        <tbody>
                            {forEach metrics renderMetricsRow}
                        </tbody>
                    </table>
                    |]
        failuresTable =
            if null failures
                then emptyStateHtml "job-failures-empty" (tr "No failed jobs — nothing to review.")
                else
                    [hsx|
                    <table class="table" data-testid="job-failures-table">
                        <thead>
                            <tr>
                                <th>{tr "Job type"}</th>
                                <th>{tr "Job"}</th>
                                <th>{tr "Error"}</th>
                                <th>{tr "Updated"}</th>
                            </tr>
                        </thead>
                        <tbody>
                            {forEach failures renderFailureRow}
                        </tbody>
                    </table>
                    |]
        tokensTable =
            if null apiTokens
                then emptyStateHtml "admin-api-tokens-empty" (tr "No API tokens exist yet.")
                else
                    [hsx|
                    <table class="table" data-testid="admin-api-tokens-table">
                        <thead>
                            <tr>
                                <th>{tr "Owner"}</th>
                                <th>{tr "Name"}</th>
                                <th>{tr "Prefix"}</th>
                                <th>{tr "Scopes"}</th>
                                <th>{tr "Last used"}</th>
                                <th></th>
                            </tr>
                        </thead>
                        <tbody>
                            {forEach apiTokens renderApiTokenRow}
                        </tbody>
                    </table>
                    |]
        provisionLinks =
            [hsx|
                <a class="btn btn-sm btn-ghost" href={exportYamlUrl} download="provision.yaml" data-testid="export-provision-yaml">{tr "Download provision.yaml"}</a>
                <a class="btn btn-sm btn-ghost" href={exportJsonUrl} download="provision.json" data-testid="export-provision-json">{tr "Download provision.json"}</a>
            |]
        exportYamlUrl :: Text
        exportYamlUrl = pathTo AdminExportProvisionAction <> "?format=yaml"
        exportJsonUrl :: Text
        exportJsonUrl = pathTo AdminExportProvisionAction <> "?format=json"

renderMetricsRow :: JobTypeMetrics -> Html
renderMetricsRow row =
    let failed = show row.failed :: Text
        retried = show row.retried :: Text
        succeeded = show row.succeeded :: Text
     in [hsx|
    <tr data-testid="job-metrics-row">
        <td>{row.jobType}</td>
        <td>{succeeded}</td>
        <td>{retried}</td>
        <td>{failed}</td>
    </tr>
|]

renderFailureRow :: FailedJobRow -> Html
renderFailureRow row =
    let lastError = fromMaybe "" row.failedJobError
     in [hsx|
    <tr data-testid="job-failure-row">
        <td>{row.failedJobType}</td>
        <td>{row.failedJobId}</td>
        <td>{lastError}</td>
        <td>{utcTimeHtml row.failedJobUpdatedAt}</td>
    </tr>
|]

renderApiTokenRow :: (ApiToken, Text) -> Html
renderApiTokenRow (token, ownerEmail) =
    let scopes :: Text
        scopes = Text.intercalate ", " token.scopes
        revoked = isJust token.revokedAt
     in [hsx|
    <tr data-testid="admin-api-token-row">
        <td>{ownerEmail}</td>
        <td>{token.name}</td>
        <td><code>{token.prefix}</code></td>
        <td>{scopes}</td>
        <td>{utcTimeOrHtml (tr "never") token.lastUsedAt}</td>
        <td>{revokeCell revoked}</td>
    </tr>
|]
  where
    revokeCell revoked
        | revoked = [hsx|<span class="badge bg-secondary">{tr "revoked"}</span>|]
        | otherwise = inlinePostFormHtml (pathTo (AdminRevokeApiTokenAction (get #id token))) (tr "Revoke") "btn btn-sm btn-ghost btn-ghost-critical" (Just "admin-api-token-revoke") True
