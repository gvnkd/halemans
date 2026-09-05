module Web.View.Admin.Index where
import Web.View.Prelude
import Application.Service.JobMetrics (JobTypeMetrics (..), FailedJobRow (..))

data IndexView = IndexView
    { metrics :: [JobTypeMetrics]
    , failures :: [FailedJobRow]
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
