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
, cmdbPanelHtml
, cmdbPanelDomId
, jiraLinksHtml
, jiraLinksDomId
, writeBackChipHtml
, writeBackChipDomId
, eventSummary
) where

import Web.View.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseMaybe)

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
        <span class="timeline-summary">{eventSummary event}</span>
        <span class="timeline-payload">{cs (show event.payload) :: Text}</span>
    </li>
|]

-- Human-readable summary for the kind-aware timeline (milestone_3.md §8):
-- external actions render with source-side attribution ("acked in zabbix by
-- admin"), enrichment/write-back failures with their subsystem/error.
eventSummary :: AlertEvent -> Text
eventSummary event = case event.kind of
    "external" -> case (payloadText "action", payloadText "source", payloadText "actor") of
        (Just action, Just source, actor) -> actionLabel action <> " in " <> source <> " by " <> fromMaybe "?" actor
        _ -> ""
    "writeback_failed" -> "write-back failed" <> maybe "" (\err -> ": " <> err) (payloadText "error")
    "enrichment_failed" -> "enrichment failed" <> maybe "" (\s -> " (" <> s <> ")") (payloadText "subsystem")
    _ -> ""
    where
        payloadText :: Text -> Maybe Text
        payloadText key = parseMaybe (Aeson.withObject "payload" (\o -> o Aeson..: Key.fromText key)) event.payload
        actionLabel = \case
            "ack" -> "acked"
            "unack" -> "unacked"
            other -> other

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

-- Context panels on the alert card (milestone_3.md §8), shared by the
-- initial render and the websocket broadcaster (kinds enriched/writeback).

cmdbPanelDomId :: Text
cmdbPanelDomId = "cmdb-panel"

cmdbPanelHtml :: Alert -> Maybe CmdbEntry -> Html
cmdbPanelHtml alert entry = [hsx|
    <section class="card mb-3" id={cmdbPanelDomId} data-testid="cmdb-panel">
        <div class="card-body">
            <h5 class="card-title">CMDB {refreshButton}</h5>
            {body}
        </div>
    </section>
|]
    where
        refreshButton = [hsx|
            <form method="POST" action={RefreshCmdbAction (get #id alert)} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-secondary" data-testid="cmdb-refresh">Refresh</button>
            </form>
        |]
        body = case entry of
            Nothing -> [hsx|<p class="text-muted" data-testid="cmdb-empty">No CMDB entry (no host/service subject, or lookup pending).</p>|]
            Just cached
                | isNothing cached.pageId -> [hsx|<p class="text-muted" data-testid="cmdb-negative">No Confluence page found for this subject (cached miss).</p>|]
                | otherwise -> [hsx|
                    <div data-testid="cmdb-entry">
                        <p><strong>{cached.title}</strong></p>
                        <p data-testid="cmdb-excerpt">{cached.excerpt}</p>
                        <p>
                            <a href={cached.url} target="_blank" data-testid="cmdb-link">Open in Confluence</a>
                            <span class="text-muted"> · cached {show cached.fetchedAt}</span>
                        </p>
                    </div>
                |]

jiraLinksDomId :: Text
jiraLinksDomId = "jira-links"

jiraLinksHtml :: Alert -> [JiraLink] -> Html
jiraLinksHtml alert links = [hsx|
    <ul id={jiraLinksDomId} data-testid="jira-links">
        {forEach links (jiraLinkItem alert)}
    </ul>
|]

jiraLinkItem :: Alert -> JiraLink -> Html
jiraLinkItem alert link = [hsx|
    <li data-testid="jira-link">
        <a href={link.url} target="_blank">{link.ticketKey}</a>
        <span class="badge status-badge" data-testid="jira-status">{link.status}</span>
        <span class="badge" data-testid="jira-origin">{link.origin}</span>
        {link.summary}
        {unlinkForm}
    </li>
|]
    where
        unlinkForm = if link.origin == "manual"
            then [hsx|
                <form method="POST" action={DeleteJiraLinkAction (get #id alert) (get #id link)} class="d-inline js-delete">
                    <button type="submit" class="btn btn-sm btn-outline-danger" data-testid="jira-unlink">unlink</button>
                </form>
            |]
            else mempty

writeBackChipDomId :: Text
writeBackChipDomId = "writeback-chip"

writeBackChipHtml :: Maybe WriteBackAttempt -> Html
writeBackChipHtml latest = [hsx|<span id={writeBackChipDomId}>{chip}</span>|]
    where
        chip = case latest of
            Nothing -> mempty
            Just attempt -> case attempt.status of
                "queued" -> [hsx|<span class="badge status-ack" data-testid="writeback-status" title="write-back pending">write-back: {attempt.action} pending</span>|]
                "failed" -> [hsx|<span class="badge status-firing" data-testid="writeback-status" title={fromMaybe "" attempt.lastError}>write-back: {attempt.action} failed</span>|]
                "done" -> [hsx|<span class="badge status-resolved" data-testid="writeback-status">write-back: {attempt.action} synced</span>|]
                _ -> mempty
