module Web.View.Alerts.Show where

import Application.Service.Timeline (groupTimeline)
import qualified Data.Aeson as Aeson
import Web.View.Fragments (alertDetailsCardHtml, alertStatusBadgeHtml, assetsPanelHtml, cmdbPanelHtml, detailsJsonHtml, emptyStateHtml, inlinePostFormHtml, jiraLinksHtml, llmPanelHtml, pageHeaderTestIdHtml, panelHtml, sectionHeaderHtml, severityBadgeHtml, timelineDomId, timelineGroupHtml, writeBackChipHtml)
import Web.View.Prelude

data ShowView = ShowView
    { alert :: Alert
    , events :: [AlertEvent]
    , comments :: [Comment]
    , commentAuthors :: [User]
    , eventActors :: [User]
    , cmdbEntry :: Maybe CmdbEntry
    , jiraLinks :: [JiraLink]
    , linkedAssetEntries :: [(AssetAlertLink, AssetsObject, AssetsConfig)]
    , agentRoles :: [LlmAgentRole]
    , writeBackAttempts :: [WriteBackAttempt]
    , analyses :: [LlmAnalysis]
    , feedback :: [LlmFeedback]
    , llmJobErrors :: [(Id LlmAnalysis, Text)]
    , canAck :: Bool
    , canClose :: Bool
    , jiraWritable :: Bool
    , metricsAvailable :: Bool
    }

instance View ShowView where
    html ShowView{..} =
        [hsx|
        <div data-testid="alert-card" data-live-scope={"alert:" <> tshow alert.id}>
            {pageHeaderTestIdHtml alert.title "alert-title" mempty}
            <p>
                {alertStatusBadgeHtml alert}
                {severityBadgeHtml alert.severity (Just "alert-severity")}
                {suppressedBadge}
                {writeBackChipHtml (head writeBackAttempts)}
            </p>

            {alertDetailsCardHtml alert}

            {actionBar}

            {cmdbPanelHtml alert cmdbEntry}

            {assetsPanelHtml alert linkedAssetEntries}

            {metricChartPanel}

            {llmPanelHtml alert analyses feedback llmJobErrors agentRoles}

            {panelHtml "jira-panel" Nothing "Jira" mempty jiraPanelBody}

            {sectionHeaderHtml (tr "Timeline") mempty}
            {timelineContent}

            {sectionHeaderHtml (tr "Comments") mempty}
            {commentsContent}
            <form method="POST" action={CreateCommentAction alert.id} data-testid="comment-form">
                <div class="mb-2">
                    <textarea name="body" class="form-control" placeholder={tr "Add a comment"} data-testid="comment-body"></textarea>
                </div>
                <button type="submit" class="btn btn-sm btn-ghost" data-testid="comment-submit">{tr "Comment"}</button>
            </form>

            {sectionHeaderHtml (tr "Labels") mempty}
            {detailsJsonHtml "alert-labels" "labels.json" (prettyJson alert.labels)}
            {sectionHeaderHtml (tr "Annotations") mempty}
            {detailsJsonHtml "alert-annotations" "annotations.json" (prettyJson alert.annotations)}
        </div>
    |]
      where
        timelineContent =
            if null events
                then emptyStateHtml "alert-timeline-empty" (tr "No events recorded yet.")
                else
                    [hsx|
                    <ul class="timeline" id={timelineDomId} data-testid="alert-timeline">
                        {forEach (groupTimeline events) timelineGroupHtml}
                    </ul>
                    |]
        commentsContent =
            if null comments
                then emptyStateHtml "alert-comments-empty" (tr "No comments yet.")
                else
                    [hsx|
                    <ul class="comments" data-testid="alert-comments">
                        {forEach (zip comments commentAuthors) renderComment}
                    </ul>
                    |]
        suppressedBadge =
            if alert.suppressed
                then [hsx|<span class="badge status-suppressed" data-testid="alert-suppressed" title={suppressedTitle}>{tr "suppressed"}</span>|]
                else mempty
        suppressedTitle :: Text
        suppressedTitle = case alert.suppressedBy of
            Just "source" -> tr "muted at source"
            _ -> tr "under blackout"
        actionBar = renderActionBar alert canAck canClose
        metricChartPanel =
            if metricsAvailable
                then
                    [hsx|
                    <section class="card mb-3" data-testid="metric-panel">
                        <div class="card-body">
                            <h5 class="card-title">{tr "Metrics"}</h5>
                            <button type="button" class="btn btn-sm btn-ghost" data-testid="metric-chart-load"
                                data-metric-chart-url={pathTo (RenderMetricChartAction alert.id)}
                                data-metric-chart-target="metric-chart-container">{tr "Show metrics"}</button>
                            <div id="metric-chart-container" class="metric-chart-container" data-testid="metric-chart-container"></div>
                        </div>
                    </section>
                    |]
                else mempty
        jiraPanelBody =
            [hsx|
            {jiraEmpty}
            {jiraLinksHtml alert jiraLinks}
            {jiraCreateForm}
            |]
        jiraEmpty =
            if null jiraLinks
                then emptyStateHtml "jira-links-empty" (tr "No Jira tickets linked to this alert.")
                else mempty
        -- Ticket creation only when the source opts into writable Jira
        -- (milestone 10); related/auto links render regardless.
        jiraCreateForm =
            if canAck && jiraWritable && alert.status /= "closed"
                then jiraTicketForm alert
                else mempty

renderActionBar :: Alert -> Bool -> Bool -> Html
renderActionBar alert canAck canClose =
    [hsx|
    <div class="action-bar mb-3" data-testid="alert-actions">
        {ackButton}
        {ackTimeoutForm}
        {unackButton}
        {closeForm}
    </div>
|]
  where
    ackButton =
        if canAck && alert.status `elem` ["firing", "stalled"]
            then inlinePostFormHtml (pathTo (AckAlertAction alert.id)) (tr "Ack") "btn btn-brand" (Just "ack-button") False
            else mempty
    ackTimeoutForm =
        if canAck && alert.status `elem` ["firing", "stalled"]
            then
                [hsx|
                <form method="POST" action={AckAlertAction alert.id} class="d-inline" data-testid="ack-timeout-form">
                    <input type="hidden" name="timeoutMinutes" value="120"/>
                    <button type="submit" class="btn btn-sm btn-ghost" data-testid="ack-timeout-button">{tr "Ack 2h"}</button>
                </form>
            |]
            else mempty
    unackButton =
        if canAck && alert.status == "ack"
            then inlinePostFormHtml (pathTo (UnackAlertAction alert.id)) (tr "Unack") "btn btn-sm btn-ghost" (Just "unack-button") False
            else mempty
    closeForm =
        if canClose && alert.status `elem` ["ack", "stalled"]
            then
                [hsx|
                <form method="POST" action={CloseAlertAction alert.id} class="d-inline" data-testid="close-form">
                    <input type="text" name="reason" class="form-control form-control-sm d-inline-block w-auto" placeholder={tr "reason"} data-testid="close-reason"/>
                    <button type="submit" class="btn btn-sm btn-ghost" data-testid="close-button">{tr "Close"}</button>
                </form>
            |]
            else mempty

-- Manual ticket creation (milestone_3.md §5): prefilled from the alert,
-- v1 never auto-creates.
jiraTicketForm :: Alert -> Html
jiraTicketForm alert =
    [hsx|
    <form method="POST" action={CreateJiraTicketAction alert.id} data-testid="jira-create-form">
        <div class="mb-2">
            <select name="issueType" class="select select-sm w-auto d-inline-block" data-testid="jira-issue-type">
                <option value="Task">Task</option>
                <option value="Bug">Bug</option>
                <option value="Incident">Incident</option>
            </select>
        </div>
        <div class="mb-2">
            <input name="summary" type="text" class="form-control form-control-sm" value={alert.title} data-testid="jira-summary"/>
        </div>
        <div class="mb-2">
            <textarea name="body" class="form-control form-control-sm" rows="3" data-testid="jira-body">{prefillBody}</textarea>
        </div>
        <button type="submit" class="btn btn-sm btn-ghost" data-testid="jira-create-submit">{tr "Create Jira ticket"}</button>
    </form>
|]
  where
    prefillBody :: Text
    prefillBody = alert.description <> "\n\n" <> trp "Source: {url}" [("url", fromMaybe "-" alert.sourceUrl)]

renderComment :: (Comment, User) -> Html
renderComment (comment, author) =
    [hsx|
    <li class="comment">
        <strong>{author.displayName}</strong>
        <span class="comment-time">{utcTimeHtml comment.createdAt}</span>
        <p>{comment.body}</p>
    </li>
|]

prettyJson :: Aeson.Value -> Text
prettyJson = cs . Aeson.encode
