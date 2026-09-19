# Milestone 3 — Context (Phase 3)

Goal: add the context layer from `01_highlevel.md` §17 Phase 3 on top of the milestone-2 correlation stack: Confluence CMDB lookup with TTL cache, Jira link/create integration, hybrid write-back to sources (ack/close propagation), user-defined dashboards, and theme packs.

Builds on the milestone-2 stack: same devenv processes, same smoke harness. LLM auto-enrichment stays Phase 4 and is out of scope — but CMDB excerpts and Jira links are persisted in the shapes the Phase-4 prompt builder will consume, so no rework there.

## 1. Deliverables

| # | Deliverable | Acceptance |
|---|---|---|
| D1 | Phase-3 schema delta (cmdb_entries, jira_links, dashboards, write_back_attempts) | Migrations apply clean; `build/Generated/Types.hs` regenerated |
| D2 | Confluence CMDB read-only client + `CmdbEntry` TTL cache per §10 | Host/service resolution populates cache; TTL (default 6h) respected; manual refresh button works |
| D3 | `EnrichAlertJob` real implementation (was pipeline step-8 stub): CMDB + Jira lookups per new alert | New alert → CMDB panel + Jira links appear on card when lookups succeed; soft-fail otherwise |
| D4 | Jira integration: JQL search on alert creation → `JiraLink` rows; `JiraSyncJob` status refresh; manual "Create Jira ticket" action | Links render with live status; ticket creation prefills summary/body and stores the link; no auto-create |
| D5 | Write-back service per §10: Zabbix `event.acknowledge`, Alertmanager/Grafana silence on local ack/close | Ack in Halemans UI propagates to source (verifiable via source API); failures retry with backoff and surface on the alert card |
| D6 | External reconciliation: pollers mirror source-side ack/close into Halemans (status mirror + `AlertEvent(external)`) | Ack in Zabbix → Halemans alert shows acked with external attribution; last-writer-wins, both actions in history |
| D7 | User dashboards: saved env+filter sets, reorderable, per user, one default; team-driven defaults per §7 | Dashboard CRUD + reorder + set-default; new user lands on team default when set |
| D8 | Theme packs per §11: CSS-variable packs (Catppuccin latte/frappe/macchiato, Dracula, light/dark defaults + brand halemans-dark/halemans-light), `data-theme` switch without reload | Theme picker on profile; no full page reload; Playwright screenshot snapshots per pack |
| D9 | Dev fixtures: mock Confluence + Jira servers in devenv (seeded pages/tickets) | Smoke/integration run without external services |
| D10 | Smoke/Playwright suites extended; `nix flake check --impure` green | Check green |

## 2. Schema delta (§3 of highlevel, Phase-3 slice)

Migrations in `Application/Migration/$(date +%s)-*.sql` per project convention. Same Schema.sql parser limits as before (columns inline in `CREATE TABLE`; FKs as top-level `ALTER ... ADD CONSTRAINT`).

New tables:
- `cmdb_entries` — host ref XOR service ref (nullable both), confluence page id, title, excerpt text, url, fetched_at. Unique partial index on host ref and on service ref (one cache row per subject).
- `jira_links` — alert ref, ticket key, summary, status, url, origin (`auto|manual`), synced_at, created_at. Unique (alert ref, ticket key).
- `dashboards` — user ref, name, config jsonb (`[{env, filters}]` ordered), position, is_default bool. Partial unique index: one default per user.
- `write_back_attempts` — alert ref, action (`ack|unack|close`), target source ref, status (`queued|done|failed`), attempts int, last_error, created/updated. Powers the retry/backoff loop and the card's write-back status indicator.

Modified tables:
- `hosts`/`services` gain nullable `cmdb_page_id` text (Confluence page id from reconciliation; set by CMDB lookup, editable in admin later).
- `teams` gains nullable `default_dashboard_config` jsonb — template used when a team member has no own dashboard yet (§7 "dashboard defaults").
- `users.settings` jsonb gains `theme` key (no schema change; documented key).

Deferred (still NOT created): llm_analyses + prompt templates (Phase 4), retention config table (Phase 5).

## 3. Enrichment pipeline (extends `Application/Helper/Ingest.hs` step 8)

Milestone-2 step 8 was a stub enqueue; now real:

- **On new alert only** (not dedupe hits): enqueue `EnrichAlertJob(alertId)`.
- Job: (a) CMDB lookup — if alert has host/service subject, CQL-search Confluence for the subject page; upsert `cmdb_entries` (fresh fetched_at); (b) Jira search — JQL over subject labels/check name for open tickets; upsert `jira_links` rows with origin `auto`.
- Soft-fail per subsystem: Confluence down → CMDB panel shows "unavailable", Jira still runs; failures logged as `AlertEvent(enrichment_failed)` with subsystem tag, job retry with backoff (max 3).
- Lookup results publish on `halemans_events` (alert scope) so an open alert card live-updates its CMDB/Jira panels (same fragment pattern as milestone 2).

## 4. Confluence CMDB

- `Application/Service/Cmdb.hs` — read-only REST client. Config: base URL + token via env (`${CONFLUENCE_TOKEN}` pattern, never in DB plaintext — §14).
- Subject resolution: CQL `text ~ "<fqdn|service name>" AND type = page` in a configured space; best match by exact title, else first hit. Negative results cached too (shorter TTL, 30m) to avoid hammering on unknown auto-created stubs.
- Cache: `cmdb_entries`, TTL 6h (config per source/env later if needed). Stale entries are served while a refresh runs (no card blocking).
- Card panel: owner, description excerpt, runbook links, "Open in Confluence" deep link, manual refresh button (`manage`-less — any `view` user may refresh).
- The excerpt is stored in a shape the Phase-4 LLM prompt builder reads directly (plain text, truncated to a documented budget).

## 5. Jira

- `Application/Service/Jira.hs` — REST v3 client, env-config (`${JIRA_TOKEN}`).
- **Auto-link** (from `EnrichAlertJob`): JQL `project = <cfg> AND statusCategory != Done AND (labels ~ host OR text ~ check)`; top N (5) open tickets → `jira_links` origin `auto`.
- **Sync**: `JiraSyncJob` (5 min periodic) refreshes status/summary of all links whose alert is not closed.
- **Manual create**: "Create Jira ticket" button on the alert card (`ack` privilege) → form (project, issue type, prefilled summary = alert title, body = description + source link + event summary) → POST create → `jira_links` origin `manual`. v1 never auto-creates tickets (§10).
- Card panel lists links with live status badge, origin tag, unlink action (manual links only).

## 6. Write-back (hybrid, §10)

`Application/Service/WriteBack.hs` + `WriteBackJob`:

- **Trigger**: local ack / unack / close actions (Actions controller hooks) insert a `write_back_attempts` row and enqueue `WriteBackJob`.
- **Per source type**:
  - Zabbix: `event.acknowledge` with message (`"<action> by <user> via Halemans"`), close → severity/close action where the API allows; unack via `event.acknowledge` unack flag.
  - Alertmanager: create a silence matching the alert's labelset for the ack duration (close = 24h default, configurable); unack/expire → delete the silence. Silence id stored on the attempt row for deletion.
  - Grafana: same silence flow against its embedded Alertmanager endpoint (`/api/alertmanager/grafana/api/v2/silences`).
  - Generic webhook sources: no write-back (attempt recorded `done` with note `unsupported`).
- **Retry**: exponential backoff (1m/5m/15m, max 5 attempts); terminal failure → status `failed`, card shows a warning chip, `AlertEvent(writeback_failed)`.
- **Reconciliation (reverse direction)**: pollers map source-side state to local mirrors —
  - Zabbix poller reads event acknowledged flag/users → local ack with `AlertEvent(external, actor=source user)` when not already acked; source unack → local unack.
  - Alertmanager/Grafana: active silence matching the fingerprint → suppressed-equivalent ack mirror; silence expiry reverts.
  - Source-resolved already works (milestone 1/2) and stays the authority for resolve.
- **Conflicts**: last-writer-wins by timestamp; both actions recorded in history (§10). Pollers never overwrite a newer local action with an older source state (compare event timestamps).

## 7. Dashboards & themes

### User dashboards (§11)
- `dashboards` CRUD under `/dashboards`: each dashboard = ordered list of `{env, filters}` cards rendered like the overview but user-composed. Reorder via admin-style position field (no drag JS in v1).
- One `is_default` per user (transactional unset-others); `/` redirects to the user's default dashboard when set, else the milestone-1 overview.
- Team default: when a user has zero dashboards, the overview falls back to `teams.default_dashboard_config` of their first team (§7); copying it to a personal dashboard is one click ("Save as my dashboard").
- Dashboard page subscribes to WS scopes per included environment (existing broadcaster, `env:<name>` scope set).

### Themes (§11)
- CSS-variable packs: `latte`, `frappe`, `macchiato`, `dracula`, `light`, `dark`, plus the brand packs `halemans-dark`/`halemans-light` (design_docs/halemans-brand-tokens.txt). Single `theme.css` with `[data-theme="…"]` variable blocks; no per-theme stylesheets.
- Picker on profile page; stored in `users.settings.theme`; applied by setting `data-theme` on `<html>` — vanilla JS swaps the attribute + persists via fetch, no reload.
- Severity/status colors are variables consumed everywhere (fix the few hardcoded colors left from milestone 1).
- Playwright screenshot snapshots per pack on the alert card page (§16).

## 8. UI additions (server-rendered HSX)

- **Alert card**: CMDB panel, Jira links panel + create-ticket form, write-back status chip (pending/failed with last error), external-action attribution on the timeline (`AlertEvent(external)` rendering: "acked in Zabbix by …").
- **Admin → sources**: per-source integration toggles (write-back enabled, CMDB space, Jira project key) in source config.
- **Admin → integrations**: Confluence/Jira connection test buttons (env var presence + API ping), cache stats.
- **Dashboards**: list/new/edit/reorder pages; default star.
- **Profile**: theme picker with live preview.

## 9. Background jobs

| Job | Schedule | Purpose |
|---|---|---|
| `PollZabbixJob` / `PollGrafanaJob` | per-source interval | extended: external ack/close reconciliation (§6) |
| `EnrichAlertJob` (new, real) | on alert creation | CMDB + Jira lookups (§3) |
| `WriteBackJob` (new) | enqueued on local ack/unack/close | source write-back with backoff (§6) |
| `JiraSyncJob` (new) | 5 min periodic | refresh linked ticket statuses (§5) |
| `EscalationJob`, `AutoCloseJob`, `PushNotificationJob` | unchanged | — |

## 10. Dev fixtures & mocks

- devenv gains two lightweight mock servers (python `http.server`-based, seeded JSON): mock Confluence (CQL endpoint answering seeded pages for dev hosts/services) and mock Jira (search + issue create/get). Ports allocated from the free range (check MEMORIES.md squatters first; candidates 18081/18082 — verify before use).
- `seed-halemans` (and the inline copy in smoke-check.sh — keep both in sync per project convention) seeds source config pointing at the mocks, plus a couple of `cmdb_entries`/`jira_links` for the seeded demo alert.
- `stack-status` reports mock health alongside the other sources.

## 11. Testing

- Unit: CQL/JQL query construction from subjects, silence matcher construction from labelsets, TTL/negative-cache logic, LWW reconciliation decision (pure timestamp compare), backoff schedule, dashboard config encode/decode round-trip, theme key validation.
- Integration: full write-back loop against the mocks (local ack → attempt row → mock shows ack/silence); reverse reconcile (ack in mock Zabbix → next poll mirrors it); conflict case (source older than local → no overwrite); enrich job partial failure (Confluence down, Jira up); JiraSyncJob status drift.
- Playwright: CMDB panel renders + manual refresh; create Jira ticket flow end-to-end against mock; dashboard CRUD/reorder/set-default + team-default fallback for a fresh user; theme switch without reload + screenshot snapshot per pack; write-back failure chip visible after forced mock 500.
- Smoke: extend `tests/smoke/run.sh` — ack via UI/API → assert silence exists in alertmanager mock and ack flag in zabbix mock; resolve-at-source still reconciles.
- Canonical gate: `nix flake check --impure`.

## 12. Acceptance checklist (end of Milestone 3)

- [ ] New alert on a seeded host → CMDB panel shows owner/excerpt/runbook links from cache; second alert same host hits cache (no extra Confluence call)
- [ ] Manual CMDB refresh bypasses TTL and updates the excerpt
- [ ] Jira auto-link finds seeded open ticket; `JiraSyncJob` reflects a status change made in the mock
- [ ] "Create Jira ticket" from alert card creates the ticket in mock Jira and links it (origin `manual`)
- [ ] Ack in Halemans → silence present in alertmanager and `event.acknowledge` visible in zabbix mock; unack removes the silence
- [ ] Ack at the source → next poll mirrors it locally with `AlertEvent(external)`; older source state never clobbers a newer local action
- [ ] Write-back failure retries on backoff, then shows the failed chip + timeline event
- [ ] User dashboard: create/reorder/set-default; `/` lands on default; fresh user sees team default
- [ ] Theme picker switches pack without reload; snapshots for all six packs
- [ ] `nix flake check --impure` green (unit + integration + smoke + Playwright)
- [ ] No secrets committed; mock tokens only in `.devenv/state/`

## 13. Decisions

- **Mocks over recorded fixtures for dev/smoke**: live HTTP mocks in devenv keep the write-back and reconcile paths exercised end-to-end; recorded-response stubs stay for Haskell unit tests of the clients.
- **Write-back is attempt-row driven, not job-payload driven**: `write_back_attempts` survives worker restarts and gives the card its status chip for free; the job is a thin executor.
- **Silence-based write-back for AM/Grafana** (no native ack exists there): ack duration defaults to the ack timeout or 24h; documented on the card so SREs aren't surprised by a silence they didn't create in the source UI.
- **LWW with timestamp guard only in the reverse direction**: local actions always win over older source state; source state newer than the last local action mirrors in. No vector clocks — the poller is the only reverse writer.
- **Dashboard reorder stays server-side position fields** (no drag-and-drop JS) per the milestone-2 "form not framework" UI stance.
- **CMDB negative caching** (30m) included from day one — auto-created subject stubs would otherwise trigger a CQL search per alert storm.
- **No auto-ticket creation** (§10) and no LLM consumption of CMDB/Jira data yet — Phase 4 reads these tables as-is.
