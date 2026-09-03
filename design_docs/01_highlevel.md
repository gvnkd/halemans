# Halemans — High-Level Design

- Halemans :: Home ALErt MANagement System
- Single-node (initially) client-server system that aggregates alerts from external monitoring systems, correlates and deduplicates them, enriches them with CMDB/Jira/LLM context, and presents a unified operational picture to SRE teams.
- Server: Haskell + IHP, PostgreSQL, server-rendered HSX UI, background workers for polling/LLM/escalation.
- Client: browser; server-rendered pages plus desktop notifications (Web Push / Notification API).

## 1. Goals and Non-Goals

### Goals
- Single pane of glass for alerts from Zabbix, Grafana, Prometheus Alertmanager, and generic webhooks.
- Deterministic alert lifecycle management (state machine, ack/close/escalate, audit trail).
- Grouping/deduplication to cut notification noise.
- Automatic LLM enrichment of incoming alerts (probable cause, suggested runbook steps, related history).
- Context enrichment from Confluence CMDB and Jira.
- Teams, roles, on-call-style notification and escalation rules.
- Blackouts (maintenance windows) per environment/host/service.

### Non-Goals (v1)
- Halemans is not a metrics store; it does not replace Zabbix/Grafana/Prometheus.
- No automatic remediation. LLM output is advisory enrichment, never executes actions.
- No multi-datacenter HA. Single instance with a single PostgreSQL.
- No phone/SMS notifications. Browser desktop notifications only; channel layer is extensible.

## 2. Architecture Overview

```
                 ┌─────────────┐  poll (API)   ┌──────────────────┐
                 │   Zabbix    │◄──────────────│                  │
                 ├─────────────┤               │                  │
                 │   Grafana   │◄──────────────│  Ingestion       │
                 ├─────────────┤               │  Workers         │
                 │ Alertmanager│──webhook─────►│  (IHP jobs)      │
                 ├─────────────┤               │                  │
                 │ Generic hook│──webhook─────►│                  │
                 └─────────────┘               └────────┬─────────┘
                                                        │ normalized events
                                                        ▼
┌────────────┐   context   ┌──────────────┐    ┌─────────────────────┐
│ Confluence │◄────────────│ Enrichment   │◄───│  Alert Pipeline     │
│   (CMDB)   │             │ Workers      │    │  normalize→dedupe→  │
├────────────┤             │  (CMDB/Jira/ │    │  group→blackout→    │
│    Jira    │◄────────────│  LLM)        │    │  state→notify       │
└────────────┘             └──────────────┘    └─────────┬───────────┘
       ▲                                                  │
       │ write-back (hybrid)                              │ persist + events
┌──────┴───────┐                                  ┌───────▼────────┐
│   Sources    │                                  │   PostgreSQL   │
│  (ack/sync)  │                                  └───────┬────────┘
└──────────────┘                                          │
                                          ┌───────────────▼───────────────┐
                                          │  Web UI (IHP, HSX, server-    │
                                          │  rendered) + WebSocket live   │
                                          │  updates + Web Push for       │
                                          │  desktop notifications        │
                                          └───────────────────────────────┘
```

### Components
| Component | Tech | Responsibility |
|---|---|---|
| Web app | IHP (HSX, controllers) | UI, auth, alert actions, configuration UI, webhook endpoints |
| Ingestion workers | IHP jobs (`WorkerMain.hs`) | Poll Zabbix/Grafana APIs; receive webhooks via web app |
| Alert pipeline | Pure Haskell core + worker | Dedup/group/blackout/state transitions/notification dispatch |
| Enrichment workers | IHP jobs | CMDB lookup, Jira lookup, LLM analysis (async, per alert) |
| Notification service | Internal module | Channel abstraction; v1 channel: Web Push to browsers |
| Live updates | WebSocket endpoint | Pushes alert/dashboard changes to connected browsers in real time |
| Write-back service | Internal module | Push ack/close/silence back to source systems where supported |
| DB | PostgreSQL | All state; IHP migrations |

Process topology: one IHP web process, one worker process (`WorkerMain`), one PostgreSQL. All async work is DB-backed job queue (IHP jobs) — no extra broker.

## 3. Domain Model

Entities (PostgreSQL tables, IHP-generated models). Naming follows IHP conventions.

### Identity & access
- **User** — login (email), password hash (IHP auth), display name, settings JSON (theme, dashboard prefs), web-push subscriptions (1:N).
- **Role** — name, privilege set. Privileges: `view`, `ack`, `close`, `escalate`, `manage_blackouts`, `manage_rules`, `manage_users`, `manage_sources`, `admin`.
- **UserRole** — M:N user↔role.
- **Team** — name, description, notification/escalation defaults.
- **TeamMember** — M:N user↔team, with per-team role hint (member/lead).
- **OnCallSchedule** (stub) — team ref, ordered member list, rotation params. Schema and data-flow hooks exist from day one; no scheduling logic in v1 — `currentOnCall` returns the first member. Notification/escalation routing calls this function so a real rotation later drops in without touching callers.

### Inventory
- **Environment** — logical grouping (e.g. `prod`, `staging`, `dc1`); top-level unit for dashboards, blackouts, rules.
- **Host** — fqdn/name, environment ref, CMDB ref (Confluence page id), labels (jsonb).
- **Service** — name, environment ref, owning team ref, host refs (M:N), labels.

### Sources & ingestion
- **Source** — type (`zabbix | grafana | alertmanager | webhook`), name, base URL, credentials ref (encrypted/env), poll interval, enabled flag, last sync cursor/timestamp, environments it feeds.
- **WebhookToken** — per-source bearer token for inbound webhooks.
- **RawEvent** — immutable append-only log of every inbound payload (source ref, received_at, payload jsonb). Basis for replay, debugging, and dedupe keys. Retention-limited.

### Alerts
- **Alert** — the core entity:
  - identity: `fingerprint` (dedupe key), source ref, external id
  - subject: environment, host, service (nullable refs), check name
  - severity: `critical | high | warning | info` (normalized from source severities via per-source mapping)
  - status: state machine field, see §5
  - title, description, labels jsonb, annotations jsonb, source_url (deep link back to Zabbix/Grafana)
  - timestamps: started_at (from source), first_seen_at, last_seen_at, resolved_at
  - ack: acknowledged_by/at, ack comment
  - closed_by/at, close reason
  - occurrence counter (bumped on dedupe hits)
- **AlertEvent** — append-only audit log per alert: state transitions, acks, comments, escalations, notifications sent, enrichment results. Powers the "history" on the alert card.
- **AlertGroup** — grouping container: `group_key`, title, environment, status rollup, member alerts (1:N). Rules in §6.
- **Comment** — free-text SRE comments on alert (author, created_at).

### Blackouts
- **Blackout** — scope: environment XOR host XOR service; start/end (or cron-like recurring window v2); reason; created_by. During a blackout, matching alerts are ingested and stored but suppressed from notifications and flagged `suppressed`.

### Rules
- **GroupingRule** — ordered; match expression over alert fields/labels; produces `group_key` template. First match wins; fallback = alert stands alone.
- **NotificationRule** — match expression + severity threshold → target (team/user) + channel config + throttle (min interval per fingerprint/group).
- **EscalationPolicy** — ordered steps: `{ after: duration, target: team/user, unless: status in (ack, closed) }`. Attached to notification rules or environments.

### Enrichment
- **LlmAnalysis** — alert ref, provider/model, prompt template version, status (`queued|running|done|failed`), result markdown, structured fields jsonb (probable_cause, confidence, suggested_actions[]), created/updated. New analysis appended on retrigger; card shows latest.
- **JiraLink** — alert ref, ticket key, summary, status; synced periodically.
- **CmdbEntry** — cached CMDB record per host/service: Confluence page id, title, excerpt, url, fetched_at (TTL-cached).

## 4. Ingestion

### 4.1 Polling connectors (Zabbix, Grafana)
- IHP job per source, scheduled by interval. Job: fetch problems since cursor → map to normalized form → upsert `RawEvent` + hand to pipeline → advance cursor transactionally.
- Zabbix: JSON-RPC API (`problem.get`, `event.get`, `host.get` for subject resolution). Token auth.
- Grafana: Alerting API (`/api/alertmanager/grafana/api/v2/alerts` for unified alerting; legacy `/api/alerts` optionally).
- Cursor strategy prevents missed/duplicate ranges; overlapping window (e.g. refetch last 5 min) + fingerprint dedupe makes polling idempotent.
- Connector failures → alert-internal event (`source_health`) surfaced on dashboard; exponential backoff.

### 4.2 Push connectors (Alertmanager, generic webhook)
- IHP controller endpoints: `POST /hooks/alertmanager/:token`, `POST /hooks/generic/:token`.
- Token identifies the source; payload validated against per-type schema, stored as `RawEvent`, pipeline triggered inline (cheap) with heavy work deferred to jobs.
- Alertmanager: map `status=firing|resolved`, labels/annotations; `fingerprint` field reused as dedupe key when present.

### 4.3 Normalization
Each connector implements:
```haskell
class Connector c where
  normalize :: c -> ByteString -> Either NormalizeError [NormalizedEvent]

data NormalizedEvent = NormalizedEvent
  { neFingerprint :: Text, neExternalId :: Maybe Text
  , neStatus      :: SourceStatus           -- Firing | Resolved
  , neSeverity    :: Severity
  , neTitle       :: Text, neDescription :: Text
  , neEnvironment :: Maybe Text, neHost :: Maybe Text, neService :: Maybe Text
  , neLabels      :: HashMap Text Text, neAnnotations :: HashMap Text Text
  , neStartedAt   :: UTCTime, neSourceUrl :: Maybe Text
  , neRaw         :: Value }
```
Fingerprint default: `source:type:environment:host:service:check` hash; overridable per connector.

## 5. Alert Pipeline & State Machine

### 5.1 Pipeline stages (per NormalizedEvent)
1. **Persist raw** — `RawEvent` insert (always, even if later dropped).
2. **Dedupe** — lookup active alert by fingerprint:
   - found + still firing → bump `occurrences`, `last_seen_at`, append `AlertEvent(repeated)`.
   - found + resolved event → transition to `resolved` (see state machine).
   - not found → create alert.
3. **Subject resolution** — map env/host/service names to inventory refs (create host/service stubs if unknown, flagged `auto_created` for later CMDB reconciliation).
4. **Blackout check** — if covered by an active blackout → status stays `suppressed`, skip notification, still visible in UI (muted styling).
5. **Grouping** — evaluate ordered `GroupingRule`s → assign to `AlertGroup` (existing or new).
6. **State transition** — apply state machine.
7. **Notification dispatch** — evaluate `NotificationRule`s; respect throttle and group-level batching; enqueue notification jobs.
8. **Enrichment enqueue** — enqueue LLM analysis job (auto-enrichment), CMDB lookup, Jira search (async, results attached when ready).

### 5.2 State machine
```
                 ┌───────────┐   resolved event    ┌───────────┐
   new alert ───►│   FIRING  │────────────────────►│ RESOLVED  │──(auto-close after T)──►┐
                 └─────┬─────┘                     └─────┬─────┘                         │
                       │ ack                             │ refires (same fingerprint)    │
                       ▼                                 ▼                               ▼
                 ┌───────────┐   unack / timeout   ┌───────────┐                   ┌──────────┐
                 │    ACK    │────────────────────►│  FIRING   │                   │  CLOSED  │
                 └─────┬─────┘                     └───────────┘                   └──────────┘
                       │ close (manual)
                       ▼
                 ┌───────────┐
                 │  CLOSED   │ (terminal; refire with same fingerprint creates a NEW alert)
                 └───────────┘

   SUPPRESSED: parallel overlay — alert under blackout keeps underlying state but is
   notification-muted and visually muted. Blackout expiry restores normal behavior.
```
- Every transition writes an `AlertEvent` (actor: user id or `system`).
- Auto-close of resolved alerts after configurable TTL (default 24h).
- Ack can carry an optional timeout ("ack for 2h") → auto-unack job.

### 5.3 Severity normalization
Per-source mapping table (stored in `Source.config`): e.g. Zabbix `Disaster→critical, High→high, Warning→warning, Info/Average→info`. Unknown → `warning`.

## 6. Grouping / Deduplication

- **Dedupe** (exact): same fingerprint while active → same alert, occurrence++.
- **Grouping** (rule-based): rules match on labels/fields and emit `group_key`, e.g.:
  - `env + host` → "host X is having a bad day" (roll up all checks on one host)
  - `env + service + check` → same check across cluster nodes
- Group card shows worst severity, member count, aggregated state. Group notifications replace per-alert notifications when a group matched (throttle at group level).
- Rules are DB-backed, ordered, editable via admin UI, versioned (keep `GroupingRule.version`; alerts record which version grouped them — enables replay).

## 7. RBAC, Teams

- Privileges checked in controllers and in pipeline actions (escalation target visibility etc.).
- Team membership drives: notification routing, escalation targets, dashboard defaults, filter presets.
- On-call routing is a stub: all notification/escalation lookups go through `currentOnCall :: TeamId -> IO (Maybe UserId)` backed by the `OnCallSchedule` table, currently returning the team's first member. Rotation logic is intentionally unimplemented in v1.
- Roles are data (DB rows), not code — admins can compose privilege sets. A fixed set of privileges is enforced by code.

## 8. Notifications & Escalation

### 8.1 Channel abstraction
```haskell
class NotificationChannel c where
  send :: c -> Notification -> IO (Either SendError Receipt)
```
- v1 channel: **BrowserPush** — Web Push (VAPID) to the user's registered subscriptions; frontend subscribes via Push API, `PushSubscription` rows per user/browser. Fallback in-page: websocket-driven notification banner + Notification API when push is unavailable or permission denied.
- Notification payload: alert/group title, severity, env, deep link to alert card, action buttons (Ack / Open).
- Future channels (Telegram, Email, Slack) plug into the same class; rules store `channel` discriminator + config jsonb.

### 8.2 Escalation
- When a notification fires, an `EscalationTracker` row is created with the policy steps.
- Worker wakes on step deadlines: if alert still `firing` (not acked/closed/suppressed), escalate to next target; each escalation is an `AlertEvent` + notification.
- Unack or severity upgrade restarts escalation.

## 9. LLM Subsystem (auto-enrichment)

- **Trigger**: every new alert (not dedupe hits) enqueues an `LlmAnalysisJob`. Also manual "Re-analyze" button on the alert card.
- **Provider abstraction**:
```haskell
class LlmProvider p where
  complete :: p -> Prompt -> IO (Either LlmError Completion)
```
  v1 implementations: OpenAI-compatible HTTP endpoint (covers local llama.cpp/vLLM and hosted APIs). Provider config (endpoint, model, key) in env/config, selectable per analysis type.
- **Prompt construction**: template (DB-stored, versioned) filled with: alert fields, recent `AlertEvent`s, CMDB excerpt, similar past alerts (same fingerprint/check, last N resolved with their resolutions), linked Jira tickets. Hard token budget; truncation strategy documented per field.
- **Output**: markdown summary + structured jsonb: `probable_cause`, `confidence`, `suggested_actions[]`, `references[]`. Rendered in the alert card under "LLM analysis" with provider/model/timestamp and a feedback widget (👍/👎 stored for prompt tuning).
- **Safety**: advisory only — LLM output never triggers state changes, notifications, or external calls. Failures are soft: card shows "analysis unavailable", retry with backoff.
- **Cost/rate control**: per-provider rate limiter + daily budget counter; dedupe of identical prompt hashes within a window (same refire storm → one analysis).

## 10. External Context Integrations

### Confluence CMDB
- Read-only client (REST, CQL search). On subject resolution, look up host/service page; cache in `CmdbEntry` with TTL (default 6h). Manual refresh button.
- Purpose: show owner, description, runbook links on alert card; feed excerpt into LLM prompt.

### Jira
- Read + create-link. On alert creation, search for open tickets matching subject labels (JQL) → `JiraLink` rows, status synced periodically.
- Manual action: "Create Jira ticket" from alert card (posts via Jira API with prefilled summary/body, stores link). v1 does not auto-create tickets.

### Write-back (hybrid)
- Halemans UI is the primary place to ack/close. Write-back worker propagates to sources **where the API allows**:
  - Zabbix: `event.acknowledge` with message.
  - Alertmanager: create silence matching the alert's labels for the ack/close duration.
  - Grafana unified alerting: silence via its Alertmanager endpoint.
- Sources remain authoritative: pollers reconcile state — if an alert is acked/closed at the source, the corresponding Halemans alert is updated (status mirror + `AlertEvent(external)`).
- Conflicts: last-writer-wins with both actions recorded in history; write-back failures retry with backoff and surface on the alert card.

## 11. Web UI (server-rendered IHP)

HSX views, standard IHP controllers, form posts; small vanilla-JS for: desktop notification permission/subscription, theme switching without reload, and the websocket client.

### Live updates (WebSocket)
- All server→client UI updates flow over a single websocket per browser tab (`/ws`), authenticated by session cookie.
- Server side: `wai-websockets` endpoint; alert mutations publish to PostgreSQL `LISTEN/NOTIFY`; a broadcaster thread fans out to subscribed connections filtered by scope (environment, alert id, dashboard filter hash).
- Update model: server sends pre-rendered HSX fragments (`alert-row`, `env-card`, `badge`) + event metadata; client swaps them into the DOM by id. No client-side rendering, no SPA state.
- Reconnect with backoff; on reconnect the client re-fetches its current page fragment set to close any gap (WS is an optimization, HTTP render is the source of truth).
- In-page notification banners for the current user also arrive on this socket (fallback when Web Push is unavailable).

### Pages
- **Login / profile** — IHP auth; profile: theme, push subscription management, default dashboard.
- **Overview dashboard** (`/`) — environments as cards: status rollup (worst severity, counts by status), sparkline of alert volume. Click-through to environment page.
- **Environment page** (`/env/:name`) — alerts table (filter by severity/status/host/service/text), blackouts indicator, group view toggle.
- **Alert card** (`/alerts/:id`) — full detail: status timeline (`AlertEvent`s), comments, actions (ack/ack-with-timeout/close/escalate/create-Jira/re-analyze), source deep link, CMDB info, Jira links, LLM analysis, raw payload viewer.
- **Group card** — group header + member alerts.
- **Dashboards** — user-defined: saved sets of environments + filters, reorderable; stored per user; one marked default.
- **Blackouts** — list + create/edit (scope picker, schedule).
- **Admin** — sources (with health status), grouping rules, notification rules, escalation policies, teams, users/roles, LLM prompt templates.

### Theming
- CSS-variable theme packs: Catppuccin (latte/frappe/macchiato), Dracula, + light/dark defaults. User setting; applied via `data-theme` attribute.

### UX rules
- Every destructive/status action is one click + optional inline comment; no modals for ack.
- Severity/status colors consistent across all pages; suppressed alerts muted with blackout icon.
- All timestamps local-time with UTC tooltip.

## 12. API Surface (summary)

UI controllers (HTML) as above, plus:
- `POST /hooks/alertmanager/:token`, `POST /hooks/generic/:token` — ingestion.
- `POST /api/push/subscribe`, `DELETE /api/push/subscribe` — web push.
- v1 has no public REST API for alerts; add a read-only JSON API (`/api/v1/alerts`) only if automation demands it (likely v1.1 for CLI tooling).

## 13. Background Workers

All as IHP jobs run by `WorkerMain`:
| Job | Schedule | Purpose |
|---|---|---|
| `PollSourceJob` | per-source interval | Zabbix/Grafana polling |
| `EnrichAlertJob` | on alert creation | CMDB + Jira lookups |
| `LlmAnalysisJob` | on alert creation / manual | LLM enrichment |
| `EscalationJob` | periodic scan (30s) | fire due escalation steps |
| `AutoCloseJob` | periodic (5m) | close stale resolved alerts, expire acks, expire blackouts |
| `WriteBackJob` | on local ack/close | sync to source systems |
| `JiraSyncJob` | periodic (5m) | refresh linked ticket statuses |
| `RetentionJob` | daily | prune old `RawEvent`s per retention config |

## 14. Configuration & Secrets

- App config via IHP `Config.hs` + env vars: DB, source credentials, LLM endpoints/keys, VAPID keys, base URL.
- Secrets never in DB in plaintext: env-var references in `Source.config` (`${ZABBIX_TOKEN}`), resolved at runtime.
- Dev environment: `flake.nix` devenv provides PostgreSQL; fixtures in `Application/Fixtures.sql` seed demo env/sources/users.

## 15. Non-Functional Requirements

- **Auditability**: every mutation (user or system) lands in `AlertEvent`; raw payloads retained (default 30d).
- **Reliability**: polling idempotent via cursor+fingerprint; webhook ingestion at-least-once with dedupe; jobs retry with backoff; no in-memory-only state.
- **Performance targets**: ingestion→visible < 5s (webhook) / < poll interval + 5s (polled); websocket fan-out latency < 1s; dashboard p95 < 300ms at 10k active alerts (proper indexes on fingerprint, status, env, group).
- **Security**: session auth (IHP), CSRF on forms, per-source webhook tokens, role checks on every mutating endpoint, VAPID for push, rate limits on webhook endpoints.
- **Observability**: structured logs per job/alert id; source-health internal alerts; job failure metrics surfaced on admin page.

## 16. Testing Strategy

- Unit: normalization per connector (golden fixtures from real Zabbix/Grafana/Alertmanager payloads), state machine transitions (property tests: no illegal transitions), grouping rule evaluation, severity mapping.
- Integration: ingestion→pipeline→DB with test PostgreSQL; webhook endpoints end-to-end; write-back against mocked source APIs; LLM against a stub provider (recorded responses).
- **Web UI: Playwright** — end-to-end browser tests against a running app + test PostgreSQL: login, dashboard rendering, alert actions (ack/close/escalate), blackout creation, filters, websocket live-update behavior (trigger server-side change, assert DOM update), push-subscription flow. Playwright suite runs in CI via `nix flake check --impure` alongside the Haskell tests; screenshot snapshots for theme packs.
- Controller-level tests (IHP test helpers) for auth and RBAC edge cases complement Playwright where a full browser is overkill.
- Canonical check: `nix flake check --impure`.

## 17. Roadmap

- **Phase 1 — Core**: DB schema, auth/RBAC, Zabbix + generic webhook ingestion, pipeline (dedupe/state/blackout), environments, alert card, overview dashboard, websocket live updates, browser push notifications.
- **Phase 2 — Correlation & teams**: grouping rules, teams, notification rules, escalation, blackouts UI, Grafana + Alertmanager connectors.
- **Phase 3 — Context**: Confluence CMDB, Jira (link/create), write-back to sources, user dashboards, themes.
- **Phase 4 — LLM**: provider abstraction, auto-enrichment, prompt templates, feedback loop, budget controls.
- **Phase 5 — Hardening**: retention, source-health alerting, performance, audit exports.

## 18. Decisions (resolved open questions)

- **Recurring blackouts**: not needed. v1 implements one-shot start/end windows only; schema does not preclude adding recurrence later.
- **On-call schedules**: no real rotation in v1, but the data flow exists as a stub — `OnCallSchedule` table + `currentOnCall` lookup used by all notification/escalation routing, returning the team's first member. Real rotations slot in behind that interface.
- **Multi-tenancy**: not needed. Single organization per instance; no `tenant_id` columns, no tenancy assumptions in rules or routing.
- **Cross-source dedupe**: explicitly out of scope. The same incident reported by both Zabbix and Grafana yields two separate alerts; fingerprints are always source-scoped.
