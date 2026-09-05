module Web.View.Admin.Index where
import Web.View.Prelude
import Application.Service.JobMetrics (JobTypeMetrics (..), FailedJobRow (..))
import qualified Data.Text as Text

data IndexView = IndexView
    { metrics :: [JobTypeMetrics]
    , failures :: [FailedJobRow]
    , apiTokens :: [(ApiToken, Text)]
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Admin</h1>
        <h2>Job metrics (last 24h)</h2>
        <table class="table" data-testid="job-metrics-table">
            <thead>
                <tr>
                    <th>Job type</th>
                    <th>Succeeded</th>
                    <th>Retried</th>
                    <th>Failed</th>
                </tr>
            </thead>
            <tbody>
                {forEach metrics renderMetricsRow}
            </tbody>
        </table>
        <h2>Recent job failures</h2>
        <table class="table" data-testid="job-failures-table">
            <thead>
                <tr>
                    <th>Job type</th>
                    <th>Job</th>
                    <th>Error</th>
                    <th>Updated</th>
                </tr>
            </thead>
            <tbody>
                {forEach failures renderFailureRow}
            </tbody>
        </table>
        <h2>API tokens</h2>
        <table class="table" data-testid="admin-api-tokens-table">
            <thead>
                <tr>
                    <th>Owner</th>
                    <th>Name</th>
                    <th>Prefix</th>
                    <th>Scopes</th>
                    <th>Last used</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach apiTokens renderApiTokenRow}
            </tbody>
        </table>
    |]

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
    let updatedAt = show row.failedJobUpdatedAt :: Text
        lastError = fromMaybe "" row.failedJobError
    in [hsx|
    <tr data-testid="job-failure-row">
        <td>{row.failedJobType}</td>
        <td>{row.failedJobId}</td>
        <td>{lastError}</td>
        <td>{updatedAt}</td>
    </tr>
|]

renderApiTokenRow :: (ApiToken, Text) -> Html
renderApiTokenRow (token, ownerEmail) =
    let scopes :: Text
        scopes = Text.intercalate ", " token.scopes
        lastUsed :: Text
        lastUsed = maybe "never" tshow token.lastUsedAt
        revoked = isJust token.revokedAt
    in [hsx|
    <tr data-testid="admin-api-token-row">
        <td>{ownerEmail}</td>
        <td>{token.name}</td>
        <td><code>{token.prefix}</code></td>
        <td>{scopes}</td>
        <td>{lastUsed}</td>
        <td>{revokeCell revoked}</td>
    </tr>
|]
    where
        revokeCell revoked
            | revoked = [hsx|<span class="badge bg-secondary">revoked</span>|]
            | otherwise = [hsx|
                <form method="POST" action={AdminRevokeApiTokenAction (get #id token)}>
                    <button type="submit" class="btn btn-sm btn-outline-danger" data-testid="admin-api-token-revoke">Revoke</button>
                </form>
            |]
