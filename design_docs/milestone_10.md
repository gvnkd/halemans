# Milestone 10 — integration scopes, related Jira tasks, alert details card

## 1. Multi-scope integrations

`jira_configs` / `cmdb_configs` are new DB-resident connection tables
(Admin → Integrations, provisionable via the `jiraConfigs`/`cmdbConfigs`
sections). Each row carries a connection (`base_url`, `token_env` — env var
NAME only) plus a JSONB string array of search scopes: `projects` for Jira,
`spaces` for Confluence. An empty array means "no scope clause" — the search
spans everything the token can see.

- `Application.Service.Jira.DbConfig` / `Application.Service.Cmdb.DbConfig`
  resolve configs DB-first (all enabled rows), falling back to the legacy
  per-source env config (`HALEMANS_JIRA_URL` + `sources.config.jiraProject`
  etc.) when no rows exist — pre-milestone-10 installs behave unchanged.
- Searches fan out over every enabled connection and all its scopes:
  `project in (A, B)` JQL / `space in ("A", "B")` CQL. Only a total failure
  (every connection errors) is reported as an enrichment failure.
- JiraSync tries every configured connection per link.
- LLM tools `jira_search`/`cmdb_lookup` search all configured scopes.

## 2. Related Jira tasks

`Application.Service.Jira.Related.relatedTasksForAlert` runs inside
EnrichAlertJob AFTER the assets step:

1. Candidates = Jira issue search across all configured projects (no
   `statusCategory != Done` filter — historical tickets are the point)
   UNION tickets connected to the alert's linked assets objects
   (`objectconnectedtickets`, "linked tasks").
2. Tickets already linked (origin auto/manual) are excluded; candidates are
   capped at 15.
3. The configured LLM (`currentLlmConfig`) gets the alert plus the candidate
   list and answers with a fenced ```json {"relevant": [keys]}``` verdict;
   only those keys survive. No LLM configured or call/parse failure →
   candidates are kept unfiltered (soft-fail). Candidates lacking a summary
   (Assets connected tickets may carry none) are first enriched via
   `GET /issue/{key}`.

   The filter prompt is admin-editable like alert enrichment: the agent role
   `jira-related-filter` (seeded) points at the active
   `jira_related_filter` prompt template (slots: the usual `{{alert.*}}`
   bindings plus `{{candidates}}`) and carries the tool whitelist — seeded
   with `jira_issue_details`, the read-only tool fetching a task's summary,
   status, labels, description (plain text, ADF-extracted on v3) and recent
   comments. The verdict format contract is appended by the code, not stored
   in the template. No role/template → built-in fallback prompt; LLM call
   runs through the shared tool loop (`runWithToolLoop`, now in
   `Application.Service.Llm.Tools`).
4. Survivors are upserted as `jira_links` rows with `origin = 'related'` and
   render in the Jira card's "Related tasks" section (WS-live like the other
   panels). Related rows the LLM no longer selects are deleted on the next
   run.

Failures of the whole related step are logged as
`AlertEvent(enrichment_failed, subsystem=jira-related)` but never re-enqueue
the enrichment run (advisory data).

## 5. Auto-analysis gate

Automatic LLM analyses (new-alert enqueue at ingest, enrichment retrigger)
pass through `Application.Service.Llm.AutoAnalyze.autoAnalyzeAllowed`: the
singleton `llm_auto_analyze_configs` row (Admin → LLM → "Auto-analysis")
whitelists statuses and severities. Defaults when no row exists: enabled,
statuses `firing`+`ack`, all severities — i.e. stalled/resolved alerts are
not auto-analyzed out of the box. Manual re-analyze from the alert card is
never gated.

## 3. Writable Jira gating

The "Create Jira ticket" card + `CreateJiraTicketAction` require the alert's
source to carry `config.jiraWritable = true` (checkbox on the source form).
Without it Jira stays read-only for that source: links and related tasks
still render.

## 4. Alert details card

The main alert facts (fingerprint, env/host/service with facet-override
badges, check, occurrences, timestamps, source link, description) render as
a card via `alertDetailsCardHtml` (Web.View.Fragments), reusing `panelHtml`
scaffolding + `.alert-details-grid` CSS. The WS broadcaster re-renders it on
alert events (`alertDetailsDomId`) so resolved_at and co. update live.

## 6. LLM tool cache

Every agent tool call (`executeToolCall`) goes through
`Application.Service.Llm.ToolCache.cachedToolCall`, memoized in
`llm_tool_cache` keyed by (tool, raw arguments). TTL and on/off live in the
singleton `llm_tool_cache_configs` (Admin → LLM → Tool cache); no row =
enabled with 300s, ttl 0 or disabled = bypass. Failure texts
(`isFailureText` prefixes) are never cached so a recovering integration is
seen immediately; rows past the TTL are evicted on write.
