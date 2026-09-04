module Web.View.Fragments
( alertRowHtml
, alertRowDomId
, alertStatusBadgeHtml
, alertStatusDomId
, timelineEventHtml
, timelineDomId
, groupRowHtml
, groupRowDomId
, groupHeaderHtml
, groupHeaderDomId
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
        <td><a href={ShowAlertAction (get #id alert)}>{alert.title}</a>{groupBadge}</td>
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
        groupBadge = case alert.groupId of
            Just groupId -> [hsx| <a href={ShowGroupAction groupId} class="badge group-badge" data-testid="group-badge">group</a>|]
            Nothing -> mempty

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

-- Group fragments (milestone_2.md §9): the env page grouped view and the
-- group card share these with the websocket broadcaster.

groupRowDomId :: AlertGroup -> Text
groupRowDomId group = "group-row-" <> tshow (get #id group)

-- | One expandable group row in the env page grouped view.
groupRowHtml :: (AlertGroup, [Alert]) -> Html
groupRowHtml (group, members) = [hsx|
    <tr class="group-row" id={groupRowDomId group} data-group-key={group.groupKey}>
        <td><span class={"badge status-badge status-" <> group.status}>{group.status}</span></td>
        <td><span class={"badge severity-badge severity-" <> group.worstSeverity}>{group.worstSeverity}</span></td>
        <td>
            <a href={ShowGroupAction (get #id group)}>{group.title}</a>
            <span class="badge group-member-count" data-testid="group-member-count">{group.memberCount}</span>
        </td>
        <td>{memberDetails}</td>
    </tr>
|]
    where
        memberDetails = if null members
            then mempty
            else [hsx|
                <details class="group-members">
                    <summary>members</summary>
                    <table class="table table-sm">
                        <tbody>
                            {forEach members alertRowHtml}
                        </tbody>
                    </table>
                </details>
            |]

groupHeaderDomId :: AlertGroup -> Text
groupHeaderDomId group = "group-header-" <> tshow (get #id group)

groupHeaderHtml :: AlertGroup -> Html
groupHeaderHtml group = [hsx|
    <div id={groupHeaderDomId group} data-testid="group-header">
        <h1>{group.title}</h1>
        <p>
            <code>{group.groupKey}</code>
            <span class={"badge status-badge status-" <> group.status}>{group.status}</span>
            <span class={"badge severity-badge severity-" <> group.worstSeverity}>{group.worstSeverity}</span>
            <span class="badge group-member-count">{group.memberCount} members</span>
        </p>
    </div>
|]
