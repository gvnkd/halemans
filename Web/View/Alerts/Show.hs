module Web.View.Alerts.Show where
import Web.View.Prelude
import Web.View.Fragments (alertStatusBadgeHtml, timelineDomId)
import qualified Data.Aeson as Aeson

data ShowView = ShowView
    { alert :: Alert
    , events :: [AlertEvent]
    , comments :: [Comment]
    , commentAuthors :: [User]
    , eventActors :: [User]
    , canAck :: Bool
    , canClose :: Bool
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div data-testid="alert-card" data-live-scope={"alert:" <> tshow alert.id}>
            <h1 data-testid="alert-title">{alert.title}</h1>
            <p>
                {alertStatusBadgeHtml alert}
                <span class={"badge severity-badge severity-" <> alert.severity} data-testid="alert-severity">{alert.severity}</span>
                {suppressedBadge}
            </p>
            <dl>
                <dt>Fingerprint</dt><dd>{alert.fingerprint}</dd>
                <dt>Env</dt><dd>{fromMaybe "-" alert.env}</dd>
                <dt>Host</dt><dd>{fromMaybe "-" alert.host}</dd>
                <dt>Service</dt><dd>{fromMaybe "-" alert.service}</dd>
                <dt>Check</dt><dd>{fromMaybe "-" alert.checkName}</dd>
                <dt>Occurrences</dt><dd>{alert.occurrences}</dd>
                <dt>Started at</dt><dd>{show alert.startedAt}</dd>
                <dt>Last seen</dt><dd>{show alert.lastSeenAt}</dd>
                <dt>Resolved at</dt><dd>{show alert.resolvedAt}</dd>
            </dl>
            {sourceLink}
            <h2>Description</h2>
            <p>{alert.description}</p>

            {actionBar}

            <h2>Timeline</h2>
            <ul class="timeline" id={timelineDomId} data-testid="alert-timeline">
                {forEach events renderEvent}
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
            <details data-testid="alert-labels">
                <summary>labels.json</summary>
                <pre class="json-viewer">{prettyJson alert.labels}</pre>
            </details>
            <h2>Annotations</h2>
            <details data-testid="alert-annotations">
                <summary>annotations.json</summary>
                <pre class="json-viewer">{prettyJson alert.annotations}</pre>
            </details>
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
            then [hsx|
                <form method="POST" action={AckAlertAction alert.id} class="d-inline">
                    <button type="submit" class="btn btn-sm btn-warning" data-testid="ack-button">Ack</button>
                </form>
            |]
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
            then [hsx|
                <form method="POST" action={UnackAlertAction alert.id} class="d-inline">
                    <button type="submit" class="btn btn-sm btn-outline-secondary" data-testid="unack-button">Unack</button>
                </form>
            |]
            else mempty
        closeForm = if canClose && alert.status == "ack"
            then [hsx|
                <form method="POST" action={CloseAlertAction alert.id} class="d-inline" data-testid="close-form">
                    <input type="text" name="reason" class="form-control form-control-sm d-inline-block w-auto" placeholder="reason" data-testid="close-reason"/>
                    <button type="submit" class="btn btn-sm btn-danger" data-testid="close-button">Close</button>
                </form>
            |]
            else mempty

renderEvent :: AlertEvent -> Html
renderEvent event = [hsx|
    <li class="timeline-event" data-kind={event.kind}>
        <span class="timeline-kind">{event.kind}</span>
        <span class="timeline-time" title={cs (show event.createdAt) :: Text}>{show event.createdAt}</span>
        <span class="timeline-payload">{prettyJson event.payload}</span>
    </li>
|]

renderComment :: (Comment, User) -> Html
renderComment (comment, author) = [hsx|
    <li class="comment">
        <strong>{author.displayName}</strong>
        <span class="comment-time">{show comment.createdAt}</span>
        <p>{comment.body}</p>
    </li>
|]

prettyJson :: Aeson.Value -> Text
prettyJson = cs . Aeson.encode
