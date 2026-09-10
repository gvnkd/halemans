module Web.View.Alerts.Show where
import Web.View.Prelude
import Web.View.Fragments (alertStatusBadgeHtml, timelineDomId, timelineEventHtml, cmdbPanelHtml, assetsPanelHtml, jiraLinksHtml, writeBackChipHtml, llmPanelHtml, severityBadgeHtml, panelHtml, detailsJsonHtml, inlinePostFormHtml)
import Application.Pipeline.Grouping (AlertField (..), alertFieldText, effectiveFieldText)
import qualified Data.Aeson as Aeson

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
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div data-testid="alert-card" data-live-scope={"alert:" <> tshow alert.id}>
            <h1 data-testid="alert-title">{alert.title}</h1>
            <p>
                {alertStatusBadgeHtml alert}
                {severityBadgeHtml alert.severity (Just "alert-severity")}
                {suppressedBadge}
                {writeBackChipHtml (head writeBackAttempts)}
            </p>
            <dl>
                <dt>Fingerprint</dt><dd>{alert.fingerprint}</dd>
                <dt>Env</dt><dd>{fieldCell FieldEnv}</dd>
                <dt>Host</dt><dd>{fieldCell FieldHost}</dd>
                <dt>Service</dt><dd>{fieldCell FieldService}</dd>
                <dt>Check</dt><dd>{fromMaybe "-" alert.checkName}</dd>
                <dt>Occurrences</dt><dd>{alert.occurrences}</dd>
                <dt>Started at</dt><dd>{maybeUtcTimeHtml alert.startedAt}</dd>
                <dt>Last seen</dt><dd>{utcTimeHtml alert.lastSeenAt}</dd>
                <dt>Resolved at</dt><dd>{maybeUtcTimeHtml alert.resolvedAt}</dd>
            </dl>
            {sourceLink}
            <h2>Description</h2>
            <p>{alert.description}</p>

            {actionBar}

            {cmdbPanelHtml alert cmdbEntry}

            {assetsPanelHtml alert linkedAssetEntries}

            {llmPanelHtml alert analyses feedback llmJobErrors agentRoles}

            {panelHtml "jira-panel" Nothing "Jira" mempty jiraPanelBody}

            <h2>Timeline</h2>
            <ul class="timeline" id={timelineDomId} data-testid="alert-timeline">
                {forEach events timelineEventHtml}
            </ul>

            <h2>Comments</h2>
            <ul class="comments" data-testid="alert-comments">
                {forEach (zip comments commentAuthors) renderComment}
            </ul>
            <form method="POST" action={CreateCommentAction alert.id} data-testid="comment-form">
                <div class="mb-2">
                    <textarea name="body" class="form-control" placeholder="Add a comment" data-testid="comment-body"></textarea>
                </div>
                <button type="submit" class="btn btn-sm btn-primary" data-testid="comment-submit">Comment</button>
            </form>

            <h2>Labels</h2>
            {detailsJsonHtml "alert-labels" "labels.json" (prettyJson alert.labels)}
            <h2>Annotations</h2>
            {detailsJsonHtml "alert-annotations" "annotations.json" (prettyJson alert.annotations)}
        </div>
    |]
        where
            suppressedBadge = if alert.suppressed
                then [hsx|<span class="badge status-suppressed" data-testid="alert-suppressed">suppressed</span>|]
                else mempty
            sourceLink = case alert.sourceUrl of
                Just url -> [hsx|<p class="source-link" data-testid="alert-source-link"><a href={url} target="_blank">source: {url}</a></p>|]
                Nothing -> mempty
            actionBar = renderActionBar alert canAck canClose
            jiraPanelBody = [hsx|{jiraLinksHtml alert jiraLinks}{jiraCreateForm}|]
            jiraCreateForm = if canAck && alert.status /= "closed"
                then jiraTicketForm alert
                else mempty
            -- Effective value (facet override wins); the raw column is shown
            -- alongside when they differ, for provenance.
            fieldCell :: AlertField -> Html
            fieldCell field = case (effectiveFieldText field alert, alertFieldText field alert) of
                (Just eff, Just raw) | eff /= raw -> [hsx|{eff} <span class="text-muted" data-testid="field-override-raw">(raw: {raw})</span>|]
                (Just eff, _) -> [hsx|{eff}|]
                (Nothing, _) -> [hsx|<span>-</span>|]

renderActionBar :: Alert -> Bool -> Bool -> Html
renderActionBar alert canAck canClose = [hsx|
    <div class="action-bar mb-3" data-testid="alert-actions">
        {ackButton}
        {ackTimeoutForm}
        {unackButton}
        {closeForm}
    </div>
|]
    where
        ackButton = if canAck && alert.status == "firing"
            then inlinePostFormHtml (pathTo (AckAlertAction alert.id)) "Ack" "btn btn-sm btn-warning" (Just "ack-button") False
            else mempty
        ackTimeoutForm = if canAck && alert.status == "firing"
            then [hsx|
                <form method="POST" action={AckAlertAction alert.id} class="d-inline" data-testid="ack-timeout-form">
                    <input type="hidden" name="timeoutMinutes" value="120"/>
                    <button type="submit" class="btn btn-sm btn-outline-warning" data-testid="ack-timeout-button">Ack 2h</button>
                </form>
            |]
            else mempty
        unackButton = if canAck && alert.status == "ack"
            then inlinePostFormHtml (pathTo (UnackAlertAction alert.id)) "Unack" "btn btn-sm btn-outline-secondary" (Just "unack-button") False
            else mempty
        closeForm = if canClose && alert.status == "ack"
            then [hsx|
                <form method="POST" action={CloseAlertAction alert.id} class="d-inline" data-testid="close-form">
                    <input type="text" name="reason" class="form-control form-control-sm d-inline-block w-auto" placeholder="reason" data-testid="close-reason"/>
                    <button type="submit" class="btn btn-sm btn-danger" data-testid="close-button">Close</button>
                </form>
            |]
            else mempty

-- Manual ticket creation (milestone_3.md §5): prefilled from the alert,
-- v1 never auto-creates.
jiraTicketForm :: Alert -> Html
jiraTicketForm alert = [hsx|
    <form method="POST" action={CreateJiraTicketAction alert.id} data-testid="jira-create-form">
        <div class="mb-2">
            <select name="issueType" class="form-select form-select-sm w-auto d-inline-block" data-testid="jira-issue-type">
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
        <button type="submit" class="btn btn-sm btn-primary" data-testid="jira-create-submit">Create Jira ticket</button>
    </form>
|]
    where
        prefillBody :: Text
        prefillBody = alert.description <> "\n\nSource: " <> fromMaybe "-" alert.sourceUrl

renderComment :: (Comment, User) -> Html
renderComment (comment, author) = [hsx|
    <li class="comment">
        <strong>{author.displayName}</strong>
        <span class="comment-time">{utcTimeHtml comment.createdAt}</span>
        <p>{comment.body}</p>
    </li>
|]

prettyJson :: Aeson.Value -> Text
prettyJson = cs . Aeson.encode
