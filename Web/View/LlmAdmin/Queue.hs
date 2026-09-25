module Web.View.LlmAdmin.Queue where

import Web.View.Fragments (emptyStateHtml, inlinePostFormHtml, pageHeaderHtml)
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
    beforeRender _ = setPageTitle (tr "LLM queue")
    html QueueView{..} =
        [hsx|
        <div>
        {pageHeaderHtml (tr "LLM queue") backLink}
        {queueTable}
        </div>
    |]
      where
        backLink = [hsx|<a href={LlmAdminAction} class="btn btn-sm btn-ghost">{tr "Back to LLM"}</a>|]
        queueTable =
            if null queue
                then emptyStateHtml "llm-queue-empty" (tr "No pending LLM requests.")
                else
                    [hsx|
                    <table class="table" data-testid="llm-queue-table">
                        <thead>
                            <tr>
                                <th>{tr "Alert"}</th>
                                <th>{tr "Analysis"}</th>
                                <th>{tr "Analysis status"}</th>
                                <th>{tr "Job"}</th>
                                <th>{tr "Job status"}</th>
                                <th>{tr "Attempts"}</th>
                                <th>{tr "Queued"}</th>
                                <th>{tr "Updated"}</th>
                                <th>{tr "Run at"}</th>
                                <th>{tr "Locked at"}</th>
                                <th>{tr "Error"}</th>
                                <th></th>
                            </tr>
                        </thead>
                        <tbody>
                            {forEach queue queueRowHtml}
                        </tbody>
                    </table>
                |]

queueRowHtml :: QueueRow -> Html
queueRowHtml row =
    [hsx|
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
    dropForm =
        if row.analysisStatus == "queued"
            then inlinePostFormHtml (pathTo (DropLlmAnalysisAction row.analysisId)) (tr "Drop") "btn btn-sm btn-ghost btn-ghost-critical" (Just "llm-queue-drop") True
            else mempty
