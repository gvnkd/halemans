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
, assetsPanelHtml
, assetsPanelDomId
, jiraLinksHtml
, jiraLinksDomId
, writeBackChipHtml
, writeBackChipDomId
, llmPanelHtml
, llmPanelDomId
, eventSummary
, filterMultiSelect
, filterTextInput
, pageHeaderHtml
, sectionHeaderHtml
, inlinePostFormHtml
, editDeleteActionsHtml
, enabledBadgeHtml
, stateBadgeHtml
, statusBadgeHtml
, severityBadgeHtml
, panelHtml
, detailsJsonHtml
, externalLinkFooterHtml
, RollupCard (..)
, rollupCardHtml
, AlertsTable (..)
, alertsTableHtml
, nextSortDir
) where

import Web.View.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as Text
import Application.Service.Assets.Attrs (objectAttributes, configuredAttrNames)
import Application.Helper.DashboardConfig (CardSize (..), alertSortNaturalDir)
import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)

-- Pre-rendered HSX fragments shared by initial page renders and the
-- websocket broadcaster (milestone_1.md §7: no client-side rendering).

alertRowDomId :: Alert -> Text
alertRowDomId alert = "alert-row-" <> tshow (get #id alert)

alertRowHtml :: Alert -> Html
alertRowHtml alert = [hsx|
    <tr data-fingerprint={alert.fingerprint} class={rowClass} id={alertRowDomId alert}>
        <td>
            {statusBadgeHtml alert.status}
            {suppressedMarker}
        </td>
        <td>{severityBadgeHtml alert.severity Nothing}</td>
        <td><a href={ShowAlertAction (get #id alert)}>{alert.title}</a>{groupBadge}</td>
        <td>{fromMaybe "" (effectiveFieldText FieldEnv alert)}</td>
        <td>{fromMaybe "" (effectiveFieldText FieldHost alert)}</td>
        <td>{alert.occurrences}</td>
        <td>{utcTimeHtml alert.lastSeenAt}</td>
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

-- | Sortable alerts table shared by /alerts and the dashboard card detail
-- page. atSortUrl builds the href for a header click (page-specific query
-- params, e.g. via nextSortDir); atSort/atDir drive the ▲/▼ indicator.
-- RecordWildCards pattern-match: the function field breaks HasField
-- selector magic.
data AlertsTable = AlertsTable
    { atTestId :: Text
    , atTbodyId :: Text
    , atLiveScope :: Maybe Text
    , atSort :: Text
    , atDir :: Text
    , atSortUrl :: Text -> Text
    , atAlerts :: [Alert]
    }

alertsTableHtml :: AlertsTable -> Html
alertsTableHtml AlertsTable { .. } = [hsx|
    <table class="table" data-testid={atTestId} data-live-scope={atLiveScope}>
        <thead>
            <tr>
                {sortableTh "status" "Status"}
                {sortableTh "severity" "Severity"}
                {sortableTh "title" "Title"}
                {sortableTh "env" "Env"}
                {sortableTh "host" "Host"}
                {sortableTh "occurrences" "Occurrences"}
                {sortableTh "last_seen_at" "Last seen"}
            </tr>
        </thead>
        <tbody id={atTbodyId}>
            {forEach atAlerts alertRowHtml}
        </tbody>
    </table>
|]
    where
        sortableTh :: Text -> Text -> Html
        sortableTh column label = [hsx|
            <th><a href={atSortUrl column} class="text-decoration-none" data-testid={"sort-" <> column}>{label}{indicator}</a></th>
        |]
            where
                indicator = if atSort == column
                    then [hsx|<span class="sort-indicator">{arrow}</span>|]
                    else mempty
                arrow :: Text
                arrow = if atDir == "asc" then " ▲" else " ▼"

-- | Direction for a header click: toggles on the active column, otherwise
-- the column's natural direction (matches /alerts).
nextSortDir :: Text -> Text -> Text -> Text
nextSortDir currentSort currentDir column
    | currentSort == column = if currentDir == "asc" then "desc" else "asc"
    | otherwise = alertSortNaturalDir column

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
        <span class="timeline-time">{utcTimeHtml event.createdAt}</span>
        <span class="timeline-summary">{eventSummary event}</span>
        {payloadDetails event}
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
    "created" -> maybe "" ("from " <>) (payloadText "source")
    "notified" -> maybe "" ("via " <>) (payloadText "rule")
    "resolved" -> case (payloadText "from", payloadText "to") of
        (Just from, Just to) -> from <> " → " <> to
        _ -> ""
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
    _ -> [hsx|
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
groupRowHtml (group, members) = [hsx|
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
            {statusBadgeHtml group.status}
            {severityBadgeHtml group.worstSeverity Nothing}
            <span class="badge group-member-count">{group.memberCount} members</span>
        </p>
    </div>
|]

-- Context panels on the alert card (milestone_3.md §8), shared by the
-- initial render and the websocket broadcaster (kinds enriched/writeback).

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
                | otherwise -> [hsx|
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
assetEntryHtml (_, object, config) = [hsx|
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
        iconImg = if Text.null object.iconUrl
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
        attrRow (name, value) = [hsx|
            <div data-testid={"asset-attr-" <> name}><dt class="d-inline text-muted">{name}: </dt><dd class="d-inline">{value}</dd></div>
        |]

-- Icon/avatar URLs are stored verbatim (assets-api.md §8.4) and rendered
-- through the app-side cache route (ShowAssetIconAction), never against the
-- Jira origin.

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
-- renders the neutral state). Markdown renders as escaped pre text (no
-- markdown library in the dependency set).

llmPanelDomId :: Text
llmPanelDomId = "llm-panel"

llmPanelHtml :: Alert -> [LlmAnalysis] -> [LlmFeedback] -> [(Id LlmAnalysis, Text)] -> [LlmAgentRole] -> Html
llmPanelHtml alert analyses feedback jobErrors roles = panelHtml "llm-panel" (Just llmPanelDomId) "LLM analysis" reanalyzeButton bodyWithHistory
    where
        bodyWithHistory = [hsx|{body}{historyBlock}|]
        reanalyzeButton = [hsx|
            <form method="POST" action={ReanalyzeAlertAction (get #id alert)} class="d-inline">
                {roleSelect}
                <button type="submit" class="btn btn-sm btn-outline-secondary" data-testid="llm-reanalyze">Re-analyze</button>
            </form>
        |]
        -- Agent-role choice (milestone_8.md §7): empty = the is_default role
        -- (or legacy behaviour when no role is marked default).
        roleSelect = case roles of
            [] -> mempty
            _ -> [hsx|
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
            [] -> headEx []
            allRows@(newest:_)
                | newest.status `elem` ["done", "failed"] -> newest
                | isJust (lookup (get #id newest) jobErrors) -> newest
                | otherwise -> case [a | a <- allRows, a.status `elem` ["done", "failed"]] of
                    (terminal:_) -> terminal
                    [] -> newest
        pendingNote = case analyses of
            (newest:_) | get #id newest /= get #id shownAnalysis ->
                [hsx|<p class="text-muted" data-testid="llm-pending-note">re-analysis pending…</p>|]
            _ -> mempty
        historyBlock = case [a | a <- analyses, get #id a /= get #id shownAnalysis] of
            [] -> mempty
            older -> [hsx|
                <details data-testid="llm-history">
                    <summary>History ({length older})</summary>
                    {forEach older olderAnalysis}
                </details>
            |]
        olderAnalysis analysis = llmAnalysisHtml alert analysis feedback (lookup (get #id analysis) jobErrors)

headEx :: [a] -> a
headEx (x:_) = x
headEx [] = error "headEx: empty list"

llmAnalysisHtml :: Alert -> LlmAnalysis -> [LlmFeedback] -> Maybe Text -> Html
llmAnalysisHtml alert analysis feedback jobError = case analysis.status of
    "done" -> [hsx|
        <div class="llm-analysis" data-testid="llm-analysis">
            {dedupedBadge}
            <pre class="llm-markdown" data-testid="llm-markdown">{fromMaybe "" analysis.resultMd}</pre>
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
            dedupedBadge = if isJust analysis.dedupedFrom
                then [hsx|<span class="badge status-ack" data-testid="llm-deduped">deduped copy</span>|]
                else mempty
            structuredBlock = case analysis.result of
                Nothing -> mempty
                Just result -> [hsx|
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
llmUnavailableHtml message = [hsx|
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
    actions -> [hsx|
        <div data-testid="llm-actions">
            <strong>Suggested actions:</strong>
            <ul>{forEach actions actionItem}</ul>
        </div>
    |]

referencesList :: Aeson.Value -> Html
referencesList result = case fieldTexts "references" result of
    [] -> mempty
    references -> [hsx|
        <div data-testid="llm-references">
            <strong>References:</strong>
            <ul>{forEach references referenceItem}</ul>
        </div>
    |]
    where
        referenceItem reference = if "http" `Text.isPrefixOf` reference
            then [hsx|<li><a href={reference} target="_blank">{reference}</a></li>|]
            else [hsx|<li>{reference}</li>|]

llmFeedbackHtml :: Alert -> LlmAnalysis -> [LlmFeedback] -> Html
llmFeedbackHtml alert analysis feedback = [hsx|
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
            (own:_) -> Just own.score
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
filterMultiSelect name label options selected = [hsx|
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
        optionItem value = [hsx|
            <div class="form-check">
                <input class="form-check-input" type="checkbox" name={name} value={value} id={name <> "-" <> value} checked={value `elem` selected}/>
                <label class="form-check-label" for={name <> "-" <> value}>{value}</label>
            </div>
        |]

-- Text filter input with a datalist auto-suggest fed from the alerts
-- currently rendered in the table below the filter panel.
filterTextInput :: Text -> Text -> Maybe Text -> [Text] -> Html
filterTextInput name placeholder value suggestions = [hsx|
    <div class="col-auto">
        <input name={name} class="form-control form-control-sm" placeholder={placeholder} value={fromMaybe "" value} list={listId} autocomplete="off" onchange="this.form.submit()"/>
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
    , rcSuppressed :: Int
    , rcHourly :: [(UTCTime, Int)]
    , rcLink :: Maybe Text
    , rcSize :: Maybe CardSize
    }

rollupCardHtml :: RollupCard -> Html
rollupCardHtml RollupCard { .. } = [hsx|
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
        cardClasses = "card env-card " <> rollupSeverityClass rcWorstSeverity
            <> if isJust rcLink then " position-relative" else ""
        -- Height override from the card's "size" config; Nothing = CSS default.
        heightStyle :: Maybe Text
        heightStyle = (\size -> "height: " <> tshow size.csHeight <> "px") <$> rcSize
        rollupLink = case rcLink of
            Nothing -> mempty
            Just href -> [hsx|<a href={href} class="stretched-link" data-testid="summary-link"></a>|]
        hourlyContent = if null rcHourly
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

-- Hour bucket as `<time> [count]`: the count sits in a colored rounded
-- square so pairs don't run together visually.
rollupHourBucket :: (UTCTime, Int) -> Html
rollupHourBucket (hour, count) = [hsx|
    <span class="hourly-bucket">
        <span class="hourly-hour">{formatTime defaultTimeLocale "%H:%M" hour}</span>
        <span class="hourly-count-badge">{count}</span>
    </span>
|]

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
pageHeaderHtml title actions = [hsx|
    <div class="d-flex justify-content-between align-items-center">
        <h1>{title}</h1>
        {actions}
    </div>
|]

-- | Same shape for h2-level sections inside a page (LlmAdmin).
sectionHeaderHtml :: Text -> Html -> Html
sectionHeaderHtml title actions = [hsx|
    <div class="d-flex justify-content-between align-items-center mt-4">
        <h2>{title}</h2>
        {actions}
    </div>
|]

-- | Inline one-button POST form (row toggles, refreshes, deletes). jsDelete
-- adds the JS confirm hook class used by destructive actions.
inlinePostFormHtml :: Text -> Text -> Text -> Maybe Text -> Bool -> Html
inlinePostFormHtml actionPath label buttonClass testId jsDelete = [hsx|
    <form method="POST" action={actionPath} class={formClass}>
        <button type="submit" class={buttonClass} data-testid={testId}>{label}</button>
    </form>
|]
    where
        formClass :: Text
        formClass = "d-inline" <> if jsDelete then " js-delete" else ""

-- | Standard row actions: Edit link + Delete form.
editDeleteActionsHtml :: Text -> Text -> Text -> Html
editDeleteActionsHtml editPath deletePath editTestId = [hsx|
    <a href={editPath} class="btn btn-sm btn-outline-secondary" data-testid={editTestId}>Edit</a>
    {inlinePostFormHtml deletePath "Delete" "btn btn-sm btn-outline-danger" Nothing False}
|]

-- | Card panel scaffolding (card > card-body > title + header action).
-- sectionId is the live-update DOM id when the panel is WS-replaceable.
panelHtml :: Text -> Maybe Text -> Text -> Html -> Html -> Html
panelHtml testId sectionId title titleAction body = [hsx|
    <section class="card mb-3" id={sectionId} data-testid={testId}>
        <div class="card-body">
            <h5 class="card-title">{title} {titleAction}</h5>
            {body}
        </div>
    </section>
|]

-- | Collapsible JSON viewer block.
detailsJsonHtml :: Text -> Text -> Text -> Html
detailsJsonHtml testId summary json = [hsx|
    <details data-testid={testId}>
        <summary>{summary}</summary>
        <pre class="json-viewer">{json}</pre>
    </details>
|]

-- | "Open in X · cached/fetched at <time>" footer of context panels.
externalLinkFooterHtml :: Text -> Text -> Text -> UTCTime -> Maybe Text -> Html
externalLinkFooterHtml url label verb time linkTestId = [hsx|
    <p>
        <a href={url} target="_blank" data-testid={linkTestId}>{label}</a>
        <span class="text-muted"> · {verb} {utcTimeHtml time}</span>
    </p>
|]
