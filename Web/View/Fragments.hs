module Web.View.Fragments
( alertRowHtml
, alertRowDomId
, alertStatusBadgeHtml
, alertStatusDomId
, timelineEventHtml
, timelineDomId
) where

import Web.View.Prelude

-- Pre-rendered HSX fragments shared by initial page renders and the
-- websocket broadcaster (milestone_1.md §7: no client-side rendering).

alertRowDomId :: Alert -> Text
alertRowDomId alert = "alert-row-" <> tshow (get #id alert)

alertRowHtml :: Alert -> Html
alertRowHtml alert = [hsx|
    <tr data-fingerprint={alert.fingerprint} class={rowClass} id={alertRowDomId alert}>
        <td>
            <span class={"badge status-badge status-" <> alert.status}>{alert.status}</span>
            {suppressedMarker}
        </td>
        <td><span class={"badge severity-badge severity-" <> alert.severity}>{alert.severity}</span></td>
        <td><a href={ShowAlertAction (get #id alert)}>{alert.title}</a></td>
        <td>{fromMaybe "" alert.env}</td>
        <td>{fromMaybe "" alert.host}</td>
        <td>{alert.occurrences}</td>
        <td>{show alert.lastSeenAt}</td>
    </tr>
|]
    where
        rowClass :: Text
        rowClass = if alert.suppressed then "alert-row suppressed" else "alert-row"
        suppressedMarker = if alert.suppressed
            then [hsx|<span class="badge status-suppressed" title="under blackout">muted</span>|]
            else mempty

alertStatusDomId :: Alert -> Text
alertStatusDomId alert = "alert-status-" <> tshow (get #id alert)

alertStatusBadgeHtml :: Alert -> Html
alertStatusBadgeHtml alert = [hsx|
    <span id={alertStatusDomId alert} class={"badge status-badge status-" <> alert.status} data-testid="alert-status">{alert.status}</span>
|]

timelineDomId :: Text
timelineDomId = "alert-timeline"

timelineEventHtml :: AlertEvent -> Html
timelineEventHtml event = [hsx|
    <li class="timeline-event" data-kind={event.kind}>
        <span class="timeline-kind">{event.kind}</span>
        <span class="timeline-time">{show event.createdAt}</span>
        <span class="timeline-payload">{cs (show event.payload) :: Text}</span>
    </li>
|]
