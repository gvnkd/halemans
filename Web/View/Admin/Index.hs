module Web.View.Admin.Index where

import Application.Service.JobMetrics (FailedJobRow (..), JobTypeMetrics (..))
import qualified Data.Text as Text
import Web.View.Fragments (inlinePostFormHtml)
import Web.View.Prelude

data IndexView = IndexView
    { metrics :: [JobTypeMetrics]
    , failures :: [FailedJobRow]
    , apiTokens :: [(ApiToken, Text)]
    }

instance View IndexView where
    html IndexView{..} =
        [hsx|
        <h1>{tr "Admin"}</h1>
        <h2>{tr "Job metrics (last 24h)"}</h2>
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
        <h2>{tr "Recent job failures"}</h2>
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
        <h2>{tr "API tokens"}</h2>
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
        <h2>{tr "Provisioning"}</h2>
        <p>
            <a class="btn btn-sm btn-outline-secondary" href={exportYamlUrl} download="provision.yaml" data-testid="export-provision-yaml">{tr "Download provision.yaml"}</a>
            <a class="btn btn-sm btn-outline-secondary" href={exportJsonUrl} download="provision.json" data-testid="export-provision-json">{tr "Download provision.json"}</a>
        </p>
        <p class="text-muted">{tr "Snapshot of users, sources, teams, LLM config, field mappings, dashboards and integrations in the provision format. Webhook tokens and hostGroupsFile are not exported (secrets stay env references)."}</p>
        <h2>{tr "Danger zone"}</h2>
        <form method="POST" action={AdminPurgeAlertsAction} data-confirm={tr "Delete ALL alerts, groups, events, comments and analyses? This cannot be undone."}>
            <button type="submit" class="btn btn-sm btn-danger" data-testid="purge-alerts">{tr "Purge all alerts"}</button>
        </form>
    |]
      where
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
        | otherwise = inlinePostFormHtml (pathTo (AdminRevokeApiTokenAction (get #id token))) (tr "Revoke") "btn btn-sm btn-outline-danger" (Just "admin-api-token-revoke") False
