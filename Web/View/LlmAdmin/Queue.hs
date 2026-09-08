module Web.View.LlmAdmin.Queue where
import Web.View.Prelude

data QueueRow = QueueRow
    { analysisId :: Id LlmAnalysis
    , alertId :: Id Alert
    , alertTitle :: Text
    , alertFingerprint :: Text
    , analysisStatus :: Text
    , analysisError :: Maybe Text
    , analysisCreatedAt :: UTCTime
    , analysisUpdatedAt :: UTCTime
    , jobId :: Maybe (Id LlmAnalysisJob)
    , jobStatus :: Maybe Text
    , jobLastError :: Maybe Text
    , jobAttempts :: Maybe Int
    , jobCreatedAt :: Maybe UTCTime
    , jobUpdatedAt :: Maybe UTCTime
    , jobRunAt :: Maybe UTCTime
    , jobLockedAt :: Maybe UTCTime
    }

data QueueView = QueueView
    { queue :: [QueueRow]
    }

instance View QueueView where
    html QueueView { .. } = [hsx|
        <h1>LLM queue</h1>
        <p><a href={LlmAdminAction} class="btn btn-sm btn-outline-secondary">Back to LLM</a></p>
        {queueTable}
    |]
        where
            queueTable = if null queue
                then [hsx|<p class="text-muted" data-testid="llm-queue-empty">No pending LLM requests.</p>|]
                else [hsx|
                    <table class="table" data-testid="llm-queue-table">
                        <thead>
                            <tr>
                                <th>Alert</th>
                                <th>Analysis</th>
                                <th>Analysis status</th>
                                <th>Job</th>
                                <th>Job status</th>
                                <th>Attempts</th>
                                <th>Queued</th>
                                <th>Updated</th>
                                <th>Run at</th>
                                <th>Locked at</th>
                                <th>Error</th>
                                <th></th>
                            </tr>
                        </thead>
                        <tbody>
                            {forEach queue queueRowHtml}
                        </tbody>
                    </table>
                |]

queueRowHtml :: QueueRow -> Html
queueRowHtml row = [hsx|
    <tr data-testid="llm-queue-row">
        <td>
            <a href={ShowAlertAction row.alertId}>{row.alertTitle}</a><br/>
            <code>{row.alertFingerprint}</code>
        </td>
        <td><code>{tshow row.analysisId}</code></td>
        <td data-testid="llm-queue-analysis-status">{row.analysisStatus}</td>
        <td><code>{maybe "-" tshow row.jobId}</code></td>
        <td data-testid="llm-queue-job-status">{fromMaybe "-" row.jobStatus}</td>
        <td>{maybe "-" tshow row.jobAttempts}</td>
        <td>{utcTimeHtml row.analysisCreatedAt}</td>
        <td>{utcTimeHtml row.analysisUpdatedAt}</td>
        <td>{maybeUtcTimeHtml row.jobRunAt}</td>
        <td>{maybeUtcTimeHtml row.jobLockedAt}</td>
        <td><span data-testid="llm-queue-error">{errorText}</span></td>
        <td>{dropForm}</td>
    </tr>
|]
    where
        errorText = case (row.analysisError, row.jobLastError) of
            (Just err, _) -> err
            (Nothing, Just err) -> err
            (Nothing, Nothing) -> ""
        dropForm = if row.analysisStatus == "queued"
            then [hsx|
                <form method="POST" action={DropLlmAnalysisAction row.analysisId} class="d-inline">
                    <button type="submit" class="btn btn-sm btn-outline-danger" data-testid="llm-queue-drop">Drop</button>
                </form>
            |]
            else mempty
