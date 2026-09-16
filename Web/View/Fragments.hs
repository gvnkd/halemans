module Web.View.Fragments (
    alertRowHtml,
    alertRowHtmlCols,
    alertRowDomId,
    alertStatusBadgeHtml,
    alertStatusDomId,
    timelineGroupHtml,
    timelineGroupDomId,
    timelineDomId,
    groupRowHtml,
    groupRowDomId,
    groupHeaderHtml,
    groupHeaderDomId,
    cmdbPanelHtml,
    cmdbPanelDomId,
    assetsPanelHtml,
    assetsPanelDomId,
    alertDetailsCardHtml,
    alertDetailsDomId,
    jiraLinksHtml,
    jiraLinksDomId,
    writeBackChipHtml,
    writeBackChipDomId,
    llmPanelHtml,
    llmPanelDomId,
    eventSummary,
    filterMultiSelect,
    filterTextInput,
    pageHeaderHtml,
    sectionHeaderHtml,
    inlinePostFormHtml,
    editDeleteActionsHtml,
    enabledBadgeHtml,
    stateBadgeHtml,
    statusBadgeHtml,
    severityBadgeHtml,
    panelHtml,
    detailsJsonHtml,
    externalLinkFooterHtml,
    RollupCard (..),
    rollupCardHtml,
    alertBaseColumns,
    alertStaticColumns,
    attachColumnFilter,
    alertSeverityOptions,
    alertStatusOptions,
    alertListColumns,
    groupedAlertsTableHtml,
) where

import Application.Helper.DashboardConfig (CardSize (..), alertSortNaturalDir, defaultAlertListColumns, validAlertSortColumns)
import Application.Pipeline.Grouping (AlertField (..), alertFieldText, effectiveFieldText)
import Application.Service.Assets.Attrs (configuredAttrNames, objectAttributes)
import Application.Service.DynTable (ColumnFilter (..), FilterKind (..), TableColumn (..))
import Application.Service.Timeline (TimelineGroup (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
import qualified Data.List as List
import qualified Data.Text as Text
import Web.View.Prelude

-- Pre-rendered HSX fragments shared by initial page renders and the
-- websocket broadcaster (milestone_1.md §7: no client-side rendering).

alertRowDomId :: Alert -> Text
alertRowDomId alert = "alert-row-" <> tshow (get #id alert)

alertRowHtml :: Alert -> Html
alertRowHtml = alertRowHtmlCols Nothing defaultAlertListColumns

-- | Column-aware alert row for the /alerts dynamic table: one <td> per
-- visible column key. groupKey is this alert's group key, looked up by the
-- caller only when the (hidden by default) group column is visible.
alertRowHtmlCols :: Maybe Text -> [Text] -> Alert -> Html
alertRowHtmlCols groupKey cols alert =
    [hsx|
    <tr data-fingerprint={alert.fingerprint} class={rowClass} id={alertRowDomId alert}>
        {forEach cols cellHtml}
    </tr>
|]
  where
    rowClass :: Text
    rowClass = if alert.suppressed then "alert-row suppressed" else "alert-row"
    cellHtml col = [hsx|<td>{cellContent col}</td>|]
    cellContent :: Text -> Html
    cellContent "status" = [hsx|{statusBadgeHtml alert.status}{suppressedMarker}|]
    cellContent "severity" = severityBadgeHtml alert.severity Nothing
    cellContent "title" = [hsx|<a href={ShowAlertAction (get #id alert)}>{alert.title}</a>{groupBadge}|]
    cellContent "env" = textCell (effectiveFieldText FieldEnv alert)
    cellContent "host" = textCell (effectiveFieldText FieldHost alert)
    cellContent "service" = textCell (effectiveFieldText FieldService alert)
    cellContent "occurrences" = [hsx|{alert.occurrences}|]
    cellContent "last_seen_at" = utcTimeHtml alert.lastSeenAt
    cellContent "group" = groupCell
    cellContent _ = mempty
    textCell value = [hsx|{fromMaybe "" value}|]
    suppressedMarker =
        if alert.suppressed
            then [hsx|<span class="badge status-suppressed" title={suppressedTitle}>muted</span>|]
            else mempty
    suppressedTitle :: Text
    suppressedTitle = case alert.suppressedBy of
        Just "source" -> "muted at source"
        _ -> "under blackout"
    groupBadge = case alert.groupId of
        Just groupId -> [hsx| <a href={ShowGroupAction groupId} class="badge group-badge" data-testid="group-badge">group</a>|]
        Nothing -> mempty
    groupCell = case (alert.groupId, groupKey) of
        (Just groupId, Just key) -> [hsx|<a href={ShowGroupAction groupId}>{key}</a>|]
        _ -> [hsx|<span class="text-muted">-</span>|]

-- | Base column set for every alerts table (/alerts, env page, group card,
-- dashboard cards): labels, sortability and natural directions. Pages that
-- support filtering attach ColumnFilters via attachColumnFilter.
alertBaseColumns :: [TableColumn]
alertBaseColumns =
    [ col "status" "Status"
    , col "severity" "Severity"
    , col "title" "Title"
    , col "env" "Env"
    , col "host" "Host"
    , col "service" "Service"
    , col "occurrences" "Occurrences"
    , col "last_seen_at" "Last seen"
    , TableColumn{colKey = "group", colLabel = "Group", colSortable = False, colNaturalDir = "asc", colFilter = Nothing}
    ]
  where
    col key label =
        TableColumn
            { colKey = key
            , colLabel = label
            , colSortable = key `elem` validAlertSortColumns
            , colNaturalDir = alertSortNaturalDir key
            , colFilter = Nothing
            }

-- | Non-sortable, filterless variant for embedded tables (dashboard cards):
-- plain headers, no interactivity.
alertStaticColumns :: [TableColumn]
alertStaticColumns = map (\col -> col{colSortable = False}) alertBaseColumns

attachColumnFilter :: Text -> ColumnFilter -> [TableColumn] -> [TableColumn]
attachColumnFilter key cf = map (\col -> if col.colKey == key then col{colFilter = Just cf} else col)

alertSeverityOptions :: [Text]
alertSeverityOptions = ["critical", "high", "warning", "info"]

alertStatusOptions :: [Text]
alertStatusOptions = ["firing", "ack", "resolved", "stalled", "closed"]

-- | Full filterable column set for the standalone alert list pages
-- (/alerts and the env page flat view): multi-selects for
-- severity/status/env, text inputs with datalist suggestions for
-- host/service/title, the numeric "min occurrences" and relative "seen
-- within" windows, and the group-key filter on the hidden group column.
-- envOptions = Nothing drops the env filter (env page: env is implicit).
alertListColumns :: Maybe [Text] -> [Alert] -> [TableColumn]
alertListColumns envOptions alerts =
    attachColumnFilter "severity" (multiFilterFor "severity" alertSeverityOptions)
        . attachColumnFilter "status" (multiFilterFor "status" alertStatusOptions)
        . attachColumnFilter "title" (textFilterFor "q" "title contains" titleSuggestions)
        . envFilter
        . attachColumnFilter "host" (textFilterFor "host" "host" hostSuggestions)
        . attachColumnFilter "service" (textFilterFor "service" "service" serviceSuggestions)
        . attachColumnFilter "occurrences" (textFilterFor "occ_min" "min N" [])
        . attachColumnFilter "last_seen_at" (textFilterFor "seen" "e.g. 24h" [])
        . attachColumnFilter "group" (textFilterFor "group" "group key" [])
        $ alertBaseColumns
  where
    envFilter = case envOptions of
        Just names -> attachColumnFilter "env" (multiFilterFor "env" names)
        -- `id` is ambiguous here (generated record field selectors).
        Nothing -> \cols -> cols
    multiFilterFor param options = ColumnFilter{cfParam = param, cfKind = FilterMulti, cfPlaceholder = param, cfOptions = options}
    textFilterFor param placeholder suggestions = ColumnFilter{cfParam = param, cfKind = FilterText, cfPlaceholder = placeholder, cfOptions = suggestions}
    hostSuggestions = List.sort (nub (mapMaybe (effectiveFieldText FieldHost) alerts))
    serviceSuggestions = List.sort (nub (mapMaybe (effectiveFieldText FieldService) alerts))
    titleSuggestions = List.sort (nub (map (\alert -> alert.title) alerts))

-- | Grouped alerts table (env page grouped view): expandable group rows,
-- one section per group. Flat tables use Web.View.DynTable.dynTableHtml.
groupedAlertsTableHtml :: Maybe Text -> Text -> [(AlertGroup, [Alert])] -> Html
groupedAlertsTableHtml testId tbodyId groups =
    [hsx|
    <table class="table" data-testid={testId}>
        <thead>
            <tr>
                <th>Status</th>
                <th>Worst severity</th>
                <th>Group</th>
                <th></th>
            </tr>
        </thead>
        <tbody id={tbodyId}>
            {forEach groups groupRowHtml}
        </tbody>
    </table>
|]

alertStatusDomId :: Alert -> Text
alertStatusDomId alert = "alert-status-" <> tshow (get #id alert)

alertStatusBadgeHtml :: Alert -> Html
alertStatusBadgeHtml alert =
    [hsx|
    <span id={alertStatusDomId alert} class={"badge status-badge status-" <> alert.status} data-testid="alert-status">{alert.status}</span>
|]

timelineDomId :: Text
timelineDomId = "alert-timeline"

-- Stable per aggregated run (anchor = oldest event of the run): the live
-- broadcaster replaceOrPrepend's the leading group by this id.
timelineGroupDomId :: TimelineGroup -> Text
timelineGroupDomId group = "timeline-group-" <> tshow (get #id group.tgAnchor)

timelineGroupHtml :: TimelineGroup -> Html
timelineGroupHtml group =
    [hsx|
    <li class="timeline-event" id={domId} data-kind={event.kind}>
        <span class="timeline-kind">{event.kind}</span>
        <span class="timeline-time">{utcTimeHtml event.createdAt}</span>
        <span class="timeline-summary">{eventSummary event}</span>
        {countBadge}
        {payloadDetails event}
    </li>
|]
  where
    event = group.tgLatest
    domId :: Text
    domId = timelineGroupDomId group
    countBadge =
        if group.tgCount > 1
            then [hsx|<span class="badge text-bg-secondary timeline-count" data-testid="timeline-count">{countText}</span>|]
            else mempty
    countText :: Text
    countText = "×" <> tshow group.tgCount

-- Human-readable summary for the kind-aware timeline (milestone_3.md §8):
-- external actions render with source-side attribution ("acked in zabbix by
-- admin"), enrichment/write-back failures with their subsystem/error.
eventSummary :: AlertEvent -> Text
eventSummary event = case event.kind of
    "external" -> case (payloadText "action", payloadText "source", payloadText "actor") of
        (Just action, Just source, actor) -> actionLabel action <> " in " <> source <> " by " <> fromMaybe "?" actor
        _ -> ""
    "created" -> maybe "" ("from " <>) (payloadText "source")
    "notified" -> maybe "" ("via " <>) (payloadText "rule")
    "resolved" -> case (payloadText "from", payloadText "to") of
        (Just from, Just to) -> from <> " → " <> to
        _ -> ""
    "stalled" -> "no source updates" <> maybe "" (\from -> " (was " <> from <> ")") (payloadText "from")
    "writeback_failed" -> "write-back failed" <> maybe "" (\err -> ": " <> err) (payloadText "error")
    "enrichment_failed" -> "enrichment failed" <> maybe "" (\s -> " (" <> s <> ")") (payloadText "subsystem")
    "llm_failed" -> "LLM analysis failed" <> maybe "" (\err -> ": " <> err) (payloadText "error")
    "llm_skipped" -> "LLM analysis skipped" <> maybe "" (\err -> ": " <> err) (payloadText "error")
    _ -> ""
  where
    payloadText :: Text -> Maybe Text
    payloadText key = parseMaybe (Aeson.withObject "payload" (\o -> o Aeson..: Key.fromText key)) event.payload
    actionLabel = \case
        "ack" -> "acked"
        "unack" -> "unacked"
        other -> other

payloadDetails :: AlertEvent -> Html
payloadDetails event = case event.payload of
    Aeson.Object o | KeyMap.null o -> mempty
    _ ->
        [hsx|
        <details class="timeline-payload">
            <summary>payload</summary>
            <pre class="json-viewer">{payloadText}</pre>
        </details>
    |]
  where
    payloadText :: Text
    payloadText = cs (Aeson.encode event.payload)

-- Group fragments (milestone_2.md §9): the env page grouped view and the
-- group card share these with the websocket broadcaster.

groupRowDomId :: AlertGroup -> Text
groupRowDomId group = "group-row-" <> tshow (get #id group)

-- | One expandable group row in the env page grouped view.
groupRowHtml :: (AlertGroup, [Alert]) -> Html
groupRowHtml (group, members) =
    [hsx|
    <tr class="group-row" id={groupRowDomId group} data-group-key={group.groupKey}>
        <td>{statusBadgeHtml group.status}</td>
        <td>{severityBadgeHtml group.worstSeverity Nothing}</td>
        <td>
            <a href={ShowGroupAction (get #id group)}>{group.title}</a>
            <span class="badge group-member-count" data-testid="group-member-count">{group.memberCount}</span>
        </td>
        <td>{memberDetails}</td>
    </tr>
|]
  where
    memberDetails =
        if null members
            then mempty
            else
                [hsx|
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
groupHeaderHtml group =
    [hsx|
    <div id={groupHeaderDomId group} data-testid="group-header">
        <h1>{group.title}</h1>
        <p>
            <code>{group.groupKey}</code>
            {statusBadgeHtml group.status}
            {severityBadgeHtml group.worstSeverity Nothing}
            <span class="badge group-member-count">{group.memberCount} members</span>
        </p>
    </div>
|]

-- Context panels on the alert card (milestone_3.md §8), shared by the
-- initial render and the websocket broadcaster (kinds enriched/writeback).

-- Main alert facts card (milestone 10): the alert's core fields rendered
-- with the common panel scaffolding — label/value grid, badges for
-- facet-overridden fields, timestamps via the local-time widget. Rendered by
-- the alert Show view and re-rendered by the WS broadcaster on alert events.
alertDetailsDomId :: Text
alertDetailsDomId = "alert-details-panel"

alertDetailsCardHtml :: Alert -> Html
alertDetailsCardHtml alert = panelHtml "alert-details-panel" (Just alertDetailsDomId) "Alert details" badges body
  where
    badges =
        [hsx|
            {severityBadgeHtml alert.severity Nothing}
            {suppressedBadge}
        |]
    suppressedBadge =
        if alert.suppressed
            then [hsx|<span class="badge status-suppressed" data-testid="alert-suppressed" title={suppressedTitle}>suppressed</span>|]
            else mempty
    suppressedTitle :: Text
    suppressedTitle = case alert.suppressedBy of
        Just "source" -> "muted at source"
        _ -> "under blackout"
    body =
        [hsx|
            <dl class="alert-details-grid" data-testid="alert-details">
                {field "fingerprint" "Fingerprint" fingerprintValue}
                {field "env" "Env" (fieldCell FieldEnv)}
                {field "host" "Host" (fieldCell FieldHost)}
                {field "service" "Service" (fieldCell FieldService)}
                {field "check" "Check" checkValue}
                {field "occurrences" "Occurrences" occurrencesBadge}
                {field "started-at" "Started at" (maybeUtcTimeHtml alert.startedAt)}
                {field "last-seen" "Last seen" (utcTimeHtml alert.lastSeenAt)}
                {field "resolved-at" "Resolved at" (maybeUtcTimeHtml alert.resolvedAt)}
            </dl>
            {sourceFooter}
            <h6>Description</h6>
            <p data-testid="alert-description">{alert.description}</p>
        |]
    fingerprintValue = [hsx|<code>{alert.fingerprint}</code>|]
    checkValue = [hsx|{fromMaybe "-" alert.checkName}|]
    occurrencesBadge = [hsx|<span class="badge" data-testid="alert-occurrences">{alert.occurrences}</span>|]
    field :: Text -> Text -> Html -> Html
    field key label value =
        [hsx|
            <div class="alert-details-field" data-testid={"alert-field-" <> key}>
                <dt>{label}</dt>
                <dd>{value}</dd>
            </div>
        |]
    sourceFooter = case alert.sourceUrl of
        Just url -> [hsx|<p class="source-link mb-2" data-testid="alert-source-link"><a href={url} target="_blank">source: {url}</a></p>|]
        Nothing -> mempty
    -- Effective value (facet override wins); the raw column is shown
    -- alongside when they differ, for provenance.
    fieldCell :: AlertField -> Html
    fieldCell alertField = case (effectiveFieldText alertField alert, alertFieldText alertField alert) of
        (Just eff, Just raw) | eff /= raw -> [hsx|<span class="badge" data-testid="field-override">{eff}</span> <span class="text-muted" data-testid="field-override-raw">(raw: {raw})</span>|]
        (Just eff, _) -> [hsx|<span class="badge">{eff}</span>|]
        (Nothing, _) -> [hsx|<span class="text-muted">-</span>|]

cmdbPanelDomId :: Text
cmdbPanelDomId = "cmdb-panel"

cmdbPanelHtml :: Alert -> Maybe CmdbEntry -> Html
cmdbPanelHtml alert entry = panelHtml "cmdb-panel" (Just cmdbPanelDomId) "CMDB" refreshButton body
  where
    refreshButton = inlinePostFormHtml (pathTo (RefreshCmdbAction (get #id alert))) "Refresh" "btn btn-sm btn-outline-secondary" (Just "cmdb-refresh") False
    body = case entry of
        Nothing -> [hsx|<p class="text-muted" data-testid="cmdb-empty">No CMDB entry (no host/service subject, or lookup pending).</p>|]
        Just cached
            | isNothing cached.pageId -> [hsx|<p class="text-muted" data-testid="cmdb-negative">No Confluence page found for this subject (cached miss).</p>|]
            | otherwise ->
                [hsx|
                    <div data-testid="cmdb-entry">
                        <p><strong>{cached.title}</strong></p>
                        <p data-testid="cmdb-excerpt">{cached.excerpt}</p>
                        {externalLinkFooterHtml cached.url "Open in Confluence" "cached" cached.fetchedAt (Just "cmdb-link")}
                    </div>
                |]

jiraLinksDomId :: Text
jiraLinksDomId = "jira-links"

-- Assets panel (milestone_8.md §5): one card per linked cached asset.
-- Renders from assets_objects only — never calls Assets at render time.
-- attribute_names on the config picks which flattened attributes show.

assetsPanelDomId :: Text
assetsPanelDomId = "assets-panel"

assetsPanelHtml :: Alert -> [(AssetAlertLink, AssetsObject, AssetsConfig)] -> Html
assetsPanelHtml alert linked = panelHtml "assets-panel" (Just assetsPanelDomId) "Assets" refreshButton body
  where
    refreshButton = inlinePostFormHtml (pathTo (RefreshAssetsAction (get #id alert))) "Refresh" "btn btn-sm btn-outline-secondary" (Just "assets-refresh") False
    body = case linked of
        [] -> [hsx|<p class="text-muted" data-testid="assets-empty">No linked assets (no info source configured, or lookup pending).</p>|]
        entries -> [hsx|<div>{forEach entries assetEntryHtml}</div>|]

assetEntryHtml :: (AssetAlertLink, AssetsObject, AssetsConfig) -> Html
assetEntryHtml (_, object, config) =
    [hsx|
    <div class="asset-entry mb-2" data-testid="asset-entry">
        <p>
            {iconImg}
            <strong data-testid="asset-label">{object.label_}</strong>
            <span class="badge status-badge" data-testid="asset-key">{object.objectKey}</span>
            <span class="badge" data-testid="asset-type">{object.objectTypeName}</span>
            {statusBadge}
        </p>
        <dl class="asset-attrs mb-1">
            {forEach displayAttrs attrRow}
        </dl>
        {externalLinkFooterHtml object.sourceUrl "Open in Jira Assets" "fetched" object.fetchedAt (Just "asset-link")}
    </div>
|]
  where
    attrs = objectAttributes object
    iconImg =
        if Text.null object.iconUrl
            then mempty
            else [hsx|<img src={ShowAssetIconAction (get #id object)} alt="" class="asset-icon" width="16" height="16"/>|]
    statusBadge = case lookup "Status" attrs of
        Nothing -> mempty
        Just status -> [hsx|<span class={"badge " <> statusClass} data-testid="asset-status">{status}</span>|]
    statusClass :: Text
    statusClass = case lookup "StatusCategory" attrs of
        Just "0" -> "status-firing"
        Just "2" -> "status-ack"
        _ -> "status-resolved"
    displayAttrs =
        [ (name, value)
        | name <- configuredAttrNames config
        , Just value <- [lookup name attrs]
        , not (Text.null value)
        ]
    attrRow (name, value) =
        [hsx|
            <div data-testid={"asset-attr-" <> name}><dt class="d-inline text-muted">{name}: </dt><dd class="d-inline">{value}</dd></div>
        |]

-- Icon/avatar URLs are stored verbatim (assets-api.md §8.4) and rendered
-- through the app-side cache route (ShowAssetIconAction), never against the
-- Jira origin.

-- Jira links (milestone_3.md §5) plus LLM-filtered related tasks
-- (milestone 10): origin 'related' rows render in their own subsection of
-- the same WS-replaceable container.
jiraLinksHtml :: Alert -> [JiraLink] -> Html
jiraLinksHtml alert links =
    [hsx|
    <div id={jiraLinksDomId}>
        <ul data-testid="jira-links">
            {forEach linked (jiraLinkItem alert)}
        </ul>
        {relatedSection}
    </div>
|]
  where
    linked = filter (\link -> link.origin /= "related") links
    related = filter (\link -> link.origin == "related") links
    relatedSection =
        if null related
            then mempty
            else
                [hsx|
                <h6 class="mt-2" data-testid="jira-related-heading">Related tasks</h6>
                <ul data-testid="jira-related">
                    {forEach related (jiraLinkItem alert)}
                </ul>
            |]

jiraLinkItem :: Alert -> JiraLink -> Html
jiraLinkItem alert link =
    [hsx|
    <li data-testid="jira-link">
        <a href={link.url} target="_blank">{link.ticketKey}</a>
        <span class="badge status-badge" data-testid="jira-status">{link.status}</span>
        <span class="badge" data-testid="jira-origin">{link.origin}</span>
        {link.summary}
        {unlinkForm}
    </li>
|]
  where
    unlinkForm =
        if link.origin == "manual"
            then inlinePostFormHtml (pathTo (DeleteJiraLinkAction (get #id alert) (get #id link))) "unlink" "btn btn-sm btn-outline-danger" (Just "jira-unlink") True
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

-- LLM analysis panel (milestone_4.md §7). Latest analysis wins; older rows
-- are expandable history. Feedback is per analysis; the `feedback` list
-- carries the viewing user's own votes (empty on the websocket path, which
-- renders the neutral state). Markdown renders server-side via cmark
-- (Application.Helper.View.markdownHtml).

llmPanelDomId :: Text
llmPanelDomId = "llm-panel"

llmPanelHtml :: Alert -> [LlmAnalysis] -> [LlmFeedback] -> [(Id LlmAnalysis, Text)] -> [LlmAgentRole] -> Html
llmPanelHtml alert analyses feedback jobErrors roles = panelHtml "llm-panel" (Just llmPanelDomId) "LLM analysis" reanalyzeButton bodyWithHistory
  where
    bodyWithHistory = [hsx|{body}{historyBlock}|]
    reanalyzeButton =
        [hsx|
            <form method="POST" action={ReanalyzeAlertAction (get #id alert)} class="d-inline">
                {roleSelect}
                <button type="submit" class="btn btn-sm btn-outline-secondary" data-testid="llm-reanalyze">Re-analyze</button>
            </form>
        |]
    -- Agent-role choice (milestone_8.md §7): empty = the is_default role
    -- (or legacy behaviour when no role is marked default).
    roleSelect = case roles of
        [] -> mempty
        _ ->
            [hsx|
                <select name="roleId" class="form-select form-select-sm d-inline-block w-auto" data-testid="llm-role-select">
                    <option value="">{defaultLabel}</option>
                    {forEach roles roleOption}
                </select>
            |]
    defaultLabel :: Text
    defaultLabel = case find (.isDefault) roles of
        Just defaultRole -> "role: " <> defaultRole.name <> " (default)"
        Nothing -> "role: default"
    roleOption role = [hsx|<option value={tshow (get #id role)}>{role.name}</option>|]
    body = case analyses of
        [] -> [hsx|<p class="text-muted" data-testid="llm-empty">No analysis yet.</p>|]
        _ -> [hsx|{pendingNote}{llmAnalysisHtml alert shownAnalysis feedback (lookup (get #id shownAnalysis) jobErrors)}|]
    -- A queued/running re-analysis (milestone_5.md §7 retrigger,
    -- milestone_8.md §7 role re-run) must not hide the last terminal
    -- analysis: show the newest done/failed row and mark the pending
    -- one. Exception: a pending row whose JOB failed (internal error)
    -- surfaces its error instead of the stale result.
    shownAnalysis = case analyses of
        -- Unreachable: llmPanelHtml is only rendered when at least one
        -- analysis row exists (the call site guards on non-empty).
        [] -> error "llmPanelHtml: no analyses"
        allRows@(newest : _)
            | newest.status `elem` ["done", "failed"] -> newest
            | isJust (lookup (get #id newest) jobErrors) -> newest
            | otherwise -> case [a | a <- allRows, a.status `elem` ["done", "failed"]] of
                (terminal : _) -> terminal
                [] -> newest
    pendingNote = case analyses of
        (newest : _)
            | get #id newest /= get #id shownAnalysis ->
                [hsx|<p class="text-muted" data-testid="llm-pending-note">re-analysis pending…</p>|]
        _ -> mempty
    historyBlock = case [a | a <- analyses, get #id a /= get #id shownAnalysis] of
        [] -> mempty
        older ->
            [hsx|
                <details data-testid="llm-history">
                    <summary>History ({length older})</summary>
                    {forEach older olderAnalysis}
                </details>
            |]
    olderAnalysis analysis = llmAnalysisHtml alert analysis feedback (lookup (get #id analysis) jobErrors)

llmAnalysisHtml :: Alert -> LlmAnalysis -> [LlmFeedback] -> Maybe Text -> Html
llmAnalysisHtml alert analysis feedback jobError = case analysis.status of
    "done" ->
        [hsx|
        <div class="llm-analysis" data-testid="llm-analysis">
            {dedupedBadge}
            <div class="llm-markdown" data-testid="llm-markdown">{markdownHtml (fromMaybe "" analysis.resultMd)}</div>
            {structuredBlock}
            <p class="text-muted llm-footer" data-testid="llm-footer">
                provider {analysis.provider} · model {analysis.model} · template v{versionText} · {utcTimeHtml analysis.updatedAt}
            </p>
            {llmFeedbackHtml alert analysis feedback}
        </div>
    |]
      where
        versionText :: Text
        versionText = maybe "-" tshow analysis.promptVersion
        dedupedBadge =
            if isJust analysis.dedupedFrom
                then [hsx|<span class="badge status-ack" data-testid="llm-deduped">deduped copy</span>|]
                else mempty
        structuredBlock = case analysis.result of
            Nothing -> mempty
            Just result ->
                [hsx|
                    <div class="llm-structured" data-testid="llm-structured">
                        <p data-testid="llm-probable-cause"><strong>Probable cause:</strong> {fieldText "probable_cause" result}</p>
                        {confidenceBadge result}
                        {actionsList result}
                        {referencesList result}
                    </div>
                |]
    "failed" -> llmUnavailableHtml (fromMaybe "" analysis.errorMessage)
    "running" -> [hsx|<p class="text-muted" data-testid="llm-running">analysis running…</p>|]
    _ -> case jobError of
        Just err -> llmUnavailableHtml err
        Nothing -> [hsx|<p class="text-muted" data-testid="llm-pending">analysis pending…</p>|]

llmUnavailableHtml :: Text -> Html
llmUnavailableHtml message =
    [hsx|
    <div data-testid="llm-unavailable">
        <span class="badge status-firing" title={message}>analysis unavailable</span>
        <span class="text-muted"> {message}</span>
    </div>
|]

fieldText :: Text -> Aeson.Value -> Text
fieldText key value = fromMaybe "" (parseMaybe (Aeson.withObject "result" (\o -> o Aeson..: Key.fromText key)) value)

fieldTexts :: Text -> Aeson.Value -> [Text]
fieldTexts key value = fromMaybe [] (parseMaybe (Aeson.withObject "result" (\o -> o Aeson..: Key.fromText key)) value)

fieldDouble :: Text -> Aeson.Value -> Maybe Double
fieldDouble key value = parseMaybe (Aeson.withObject "result" (\o -> o Aeson..: Key.fromText key)) value

confidenceBadge :: Aeson.Value -> Html
confidenceBadge result = case fieldDouble "confidence" result of
    Nothing -> mempty
    Just confidence -> [hsx|<span class="badge status-badge" data-testid="llm-confidence">confidence {confidenceText confidence}</span>|]
  where
    confidenceText :: Double -> Text
    confidenceText confidence = tshow (round (confidence * 100) :: Int) <> "%"

actionItem :: Text -> Html
actionItem action = [hsx|<li>{action}</li>|]

actionsList :: Aeson.Value -> Html
actionsList result = case fieldTexts "suggested_actions" result of
    [] -> mempty
    actions ->
        [hsx|
        <div data-testid="llm-actions">
            <strong>Suggested actions:</strong>
            <ul>{forEach actions actionItem}</ul>
        </div>
    |]

referencesList :: Aeson.Value -> Html
referencesList result = case fieldTexts "references" result of
    [] -> mempty
    references ->
        [hsx|
        <div data-testid="llm-references">
            <strong>References:</strong>
            <ul>{forEach references referenceItem}</ul>
        </div>
    |]
  where
    referenceItem reference =
        if "http" `Text.isPrefixOf` reference
            then [hsx|<li><a href={reference} target="_blank">{reference}</a></li>|]
            else [hsx|<li>{reference}</li>|]

llmFeedbackHtml :: Alert -> LlmAnalysis -> [LlmFeedback] -> Html
llmFeedbackHtml alert analysis feedback =
    [hsx|
    <div class="llm-feedback" data-testid="llm-feedback">
        <form method="POST" action={LlmFeedbackAction (get #id alert) (get #id analysis)} class="d-inline">
            <input type="hidden" name="score" value="1"/>
            <button type="submit" class={upClass} data-testid="llm-feedback-up">👍</button>
        </form>
        <form method="POST" action={LlmFeedbackAction (get #id alert) (get #id analysis)} class="d-inline">
            <input type="hidden" name="score" value="-1"/>
            <button type="submit" class={downClass} data-testid="llm-feedback-down">👎</button>
        </form>
    </div>
|]
  where
    vote = case [f | f <- feedback, f.analysisId == get #id analysis] of
        (own : _) -> Just own.score
        [] -> Nothing
    upClass :: Text
    upClass = if vote == Just 1 then "btn btn-sm btn-success" else "btn btn-sm btn-outline-secondary"
    downClass :: Text
    downClass = if vote == Just (-1) then "btn btn-sm btn-danger" else "btn btn-sm btn-outline-secondary"

-- Checkbox dropdown multi-select shared by the /alerts and /env/:name
-- filter panels. The menu stays open while options are toggled
-- (data-bs-auto-close="outside", no per-checkbox submit); app.js submits
-- the enclosing form once when the dropdown closes after a change.
filterMultiSelect :: Text -> Text -> [Text] -> [Text] -> Html
filterMultiSelect name label options selected =
    [hsx|
    <div class="col-auto dropdown" data-testid={"filter-" <> name} data-filter-dropdown="true">
        <button class="btn btn-sm btn-outline-secondary dropdown-toggle" type="button" data-bs-toggle="dropdown" data-bs-auto-close="outside">{buttonLabel}</button>
        <div class="dropdown-menu p-2">
            {forEach options optionItem}
        </div>
    </div>
|]
  where
    buttonLabel :: Text
    buttonLabel = label <> ": " <> if null selected then "any" else tshow (length selected)
    optionItem value =
        [hsx|
            <div class="form-check">
                <input class="form-check-input" type="checkbox" name={name} value={value} id={name <> "-" <> value} checked={value `elem` selected}/>
                <label class="form-check-label" for={name <> "-" <> value}>{value}</label>
            </div>
        |]

-- Text filter input with a datalist auto-suggest fed from the alerts
-- currently rendered in the table below the filter panel.
filterTextInput :: Text -> Text -> Maybe Text -> [Text] -> Html
filterTextInput name placeholder value suggestions =
    [hsx|
    <div class="col-auto">
        <input name={name} class="form-control form-control-sm" placeholder={placeholder} value={fromMaybe "" value} list={listId} autocomplete="off" data-autosubmit=""/>
        <datalist id={listId}>
            {forEach suggestions suggestionOption}
        </datalist>
    </div>
|]
  where
    listId :: Text
    listId = "filter-suggestions-" <> name
    suggestionOption suggestion = [hsx|<option value={suggestion}></option>|]

-- Shared rollup widget (v1.22.0): the overview env cards and custom-dashboard
-- "summary": true cards render through this one renderer so counts, badges,
-- hourly buckets and tooltips never diverge. rcLink, when set, turns the
-- whole card into a stretched link (custom dashboard card-alerts page).
data RollupCard = RollupCard
    { rcTitle :: Html
    , rcWorstSeverity :: Maybe Text
    , rcFiring :: Int
    , rcAcked :: Int
    , rcResolved :: Int
    , rcStalled :: Int
    , rcSuppressed :: Int
    , rcHourly :: [(UTCTime, Int)]
    , rcLink :: Maybe Text
    , rcSize :: Maybe CardSize
    }

rollupCardHtml :: RollupCard -> Html
rollupCardHtml RollupCard{..} =
    [hsx|
    <div class={cardClasses} style={heightStyle}>
        <div class="card-body">
            <h5 class="card-title">
                {rcTitle}
                {rollupSeverityBadge rcWorstSeverity}
            </h5>
            <div class="env-counts">
                <span class="count status-firing" data-testid="count-firing">{rcFiring} firing</span>
                <span class="count status-ack" data-testid="count-ack">{rcAcked} ack</span>
                <span class="count status-resolved" data-testid="count-resolved">{rcResolved} resolved</span>
                <span class="count status-stalled" data-testid="count-stalled">{rcStalled} stalled</span>
                {rollupSuppressedBadge rcSuppressed}
            </div>
            <div class="env-hourly" title="alerts per hour (last 24h)">
                {hourlyContent}
            </div>
            {rollupLink}
        </div>
    </div>
|]
  where
    cardClasses :: Text
    cardClasses =
        "card env-card "
            <> rollupSeverityClass rcWorstSeverity
            <> if isJust rcLink then " position-relative" else ""
    -- Height override from the card's "size" config; Nothing = CSS default.
    heightStyle :: Maybe Text
    heightStyle = (\size -> "height: " <> tshow size.csHeight <> "px") <$> rcSize
    rollupLink = case rcLink of
        Nothing -> mempty
        Just href -> [hsx|<a href={href} class="stretched-link" data-testid="summary-link"></a>|]
    hourlyContent =
        if null rcHourly
            then [hsx|<span class="text-muted" data-testid="hourly-empty">No events in the last 24 hours.</span>|]
            else forEach rcHourly rollupHourBucket

rollupSeverityClass :: Maybe Text -> Text
rollupSeverityClass = \case
    Just severity -> "env-card-" <> severity
    Nothing -> "env-card-ok"

rollupSeverityBadge :: Maybe Text -> Html
rollupSeverityBadge Nothing = mempty
rollupSeverityBadge (Just severity) = severityBadgeHtml severity Nothing

rollupSuppressedBadge :: Int -> Html
rollupSuppressedBadge count
    | count > 0 = [hsx|<span class="count status-suppressed" data-testid="count-suppressed" title="muted by blackout">{count} suppressed</span>|]
    | otherwise = mempty

-- Hour bucket as `HH [count]`: the count sits in a colored rounded square
-- so pairs don't run together visually. The hour goes through the same
-- client-side localizer as utcTimeHtml, with data-tz-format="hour" keeping
-- the compact HH shape in the user's/browser timezone.
rollupHourBucket :: (UTCTime, Int) -> Html
rollupHourBucket (hour, count) =
    [hsx|
    <span class="hourly-bucket">
        <time class="utc-time" datetime={iso} data-tz-format="hour">{fallback}</time>
        <span class="hourly-count-badge">{count}</span>
    </span>
|]
  where
    iso :: Text
    iso = cs (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" hour)
    fallback :: Text
    fallback = cs (formatTime defaultTimeLocale "%H" hour)

-- Generic widgets shared across CRUD/list views (v1.23.0 cleanup): page
-- headers, one-button POST forms, row actions, badges, card panels and JSON
-- viewers. All data-testids stay at the call sites so Playwright selectors
-- never move.

-- | Status pill for alert/group status values.
statusBadgeHtml :: Text -> Html
statusBadgeHtml status = [hsx|<span class={"badge status-badge status-" <> status}>{status}</span>|]

-- | Severity pill; optional data-testid (omitted when Nothing).
severityBadgeHtml :: Text -> Maybe Text -> Html
severityBadgeHtml severity testId = [hsx|<span class={"badge severity-badge severity-" <> severity} data-testid={testId}>{severity}</span>|]

-- | enabled/disabled state pill without testids (admin list rows).
enabledBadgeHtml :: Bool -> Html
enabledBadgeHtml enabled
    | enabled = [hsx|<span class="badge status-resolved">enabled</span>|]
    | otherwise = [hsx|<span class="badge bg-secondary">disabled</span>|]

-- | enabled/disabled state pill with per-state testids derived from a base
-- name (<base>-enabled / <base>-disabled).
stateBadgeHtml :: Bool -> Text -> Html
stateBadgeHtml enabled base
    | enabled = [hsx|<span class="badge status-resolved" data-testid={base <> "-enabled"}>enabled</span>|]
    | otherwise = [hsx|<span class="badge bg-secondary" data-testid={base <> "-disabled"}>disabled</span>|]

-- | List-page header: title left, action buttons right.
pageHeaderHtml :: Text -> Html -> Html
pageHeaderHtml title actions =
    [hsx|
    <div class="d-flex justify-content-between align-items-center">
        <h1>{title}</h1>
        {actions}
    </div>
|]

-- | Same shape for h2-level sections inside a page (LlmAdmin).
sectionHeaderHtml :: Text -> Html -> Html
sectionHeaderHtml title actions =
    [hsx|
    <div class="d-flex justify-content-between align-items-center mt-4">
        <h2>{title}</h2>
        {actions}
    </div>
|]

-- | Inline one-button POST form (row toggles, refreshes, deletes). jsDelete
-- adds the JS confirm hook class used by destructive actions.
inlinePostFormHtml :: Text -> Text -> Text -> Maybe Text -> Bool -> Html
inlinePostFormHtml actionPath label buttonClass testId jsDelete =
    [hsx|
    <form method="POST" action={actionPath} class={formClass}>
        <button type="submit" class={buttonClass} data-testid={testId}>{label}</button>
    </form>
|]
  where
    formClass :: Text
    formClass = "d-inline" <> if jsDelete then " js-delete" else ""

-- | Standard row actions: Edit link + Delete form.
editDeleteActionsHtml :: Text -> Text -> Text -> Html
editDeleteActionsHtml editPath deletePath editTestId =
    [hsx|
    <a href={editPath} class="btn btn-sm btn-outline-secondary" data-testid={editTestId}>Edit</a>
    {inlinePostFormHtml deletePath "Delete" "btn btn-sm btn-outline-danger" Nothing False}
|]

-- | Card panel scaffolding (card > card-body > title + header action).
-- sectionId is the live-update DOM id when the panel is WS-replaceable.
panelHtml :: Text -> Maybe Text -> Text -> Html -> Html -> Html
panelHtml testId sectionId title titleAction body =
    [hsx|
    <section class="card mb-3" id={sectionId} data-testid={testId}>
        <div class="card-body">
            <h5 class="card-title">{title} {titleAction}</h5>
            {body}
        </div>
    </section>
|]

-- | Collapsible JSON viewer block.
detailsJsonHtml :: Text -> Text -> Text -> Html
detailsJsonHtml testId summary json =
    [hsx|
    <details data-testid={testId}>
        <summary>{summary}</summary>
        <pre class="json-viewer">{json}</pre>
    </details>
|]

-- | "Open in X · cached/fetched at <time>" footer of context panels.
externalLinkFooterHtml :: Text -> Text -> Text -> UTCTime -> Maybe Text -> Html
externalLinkFooterHtml url label verb time linkTestId =
    [hsx|
    <p>
        <a href={url} target="_blank" data-testid={linkTestId}>{label}</a>
        <span class="text-muted"> · {verb} {utcTimeHtml time}</span>
    </p>
|]
