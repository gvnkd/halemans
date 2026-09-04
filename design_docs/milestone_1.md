# Milestone 1 — Core Product (Phase 1)

Goal: turn the milestone-0 ingestion slice into the usable core from `01_highlevel.md` §17 Phase 1: real domain schema, auth/RBAC, deterministic alert lifecycle (dedupe, state machine, blackouts), environment inventory, a proper alert card + overview dashboard, websocket live updates, and browser push notifications.

Builds directly on the milestone-0 stack: same devenv processes, same smoke harness, same three connectors (Zabbix poller, Alertmanager + generic webhooks). Grafana poller and Alertmanager-as-source are Phase 2 and stay out of scope.

## 1. Deliverables

| # | Deliverable | Acceptance |
|---|---|---|
| D1 | Full Phase-1 schema delta (users/roles, environments/hosts/services, alert_events, comments, blackouts, push_subscriptions) | Migrations apply clean; `build/Generated/Types.hs` regenerated |
| D2 | IHP auth + privilege checks | Login required for all UI; mutating endpoints check role privileges |
| D3 | Alert state machine (firing/ack/resolved/closed/suppressed overlay) per §5.2 | Unit + property tests: no illegal transitions; every transition writes `AlertEvent` |
| D4 | Dedupe + resolve pipeline hardening | Refire bumps `occurrences`; resolved event resolves; closed refire creates new alert |
| D5 | Subject resolution → inventory refs with `auto_created` stubs | Alerts link to environments/hosts/services rows |
| D6 | Blackouts: table + pipeline suppression (one-shot windows) | Covered alert ingested, `suppressed`, no notification, muted in UI; expiry restores |
| D7 | Alert card: timeline, comments, actions (ack / ack-with-timeout / close), raw payload viewer | Playwright scenario passes |
| D8 | Overview dashboard `/`: environment cards with status rollup + counts | Playwright scenario passes |
| D9 | WebSocket `/ws` live updates via PG LISTEN/NOTIFY, pre-rendered HSX fragments | DOM updates without reload when alert changes server-side |
| D10 | Browser push (VAPID Web Push) + in-page banner fallback | Push received on new critical alert; subscribe/unsubscribe endpoints work |
| D11 | Smoke/Playwright suites extended for all of the above in `nix flake check --impure` | Check green |

## 2. Schema delta (§3 of highlevel, Phase-1 slice)

Migrations in `Application/Migration/$(date +%s)-*.sql` per project convention. Mind the Schema.sql parser limits (no inline `--` in CREATE TABLE, FKs only as top-level ALTER ... ADD CONSTRAINT).

New tables:
- `users` (IHP auth: email, password_hash, display name, settings jsonb), `roles`, `user_roles`. Fixed privilege set enforced in code: `view, ack, close, escalate, manage_blackouts, manage_rules, manage_users, manage_sources, admin`. Seeded dev roles: `admin`, `sre` (view/ack/close/escalate), `viewer`.
- `environments` (name unique), `hosts` (fqdn, env ref, labels, auto_created flag), `services` (name, env ref, labels, auto_created). `alerts` gains nullable `environment_id`/`host_id`/`service_id` refs alongside the existing text fields (text kept as ingest-time raw value).
- `alert_events` — append-only audit: alert ref, actor (`user_id` nullable = system), kind (`created|repeated|ack|unack|resolved|closed|suppressed|unsuppressed|escalated|notified|external|comment`), payload jsonb, created_at.
- `comments` — alert ref, author ref, body, created_at. Also mirrored as `AlertEvent(comment)`.
- `blackouts` — scope: exactly one of environment/host/service ref; starts_at/ends_at; reason; created_by.
- `push_subscriptions` — user ref, endpoint (unique), p256dh/auth keys, user agent, created_at.
- Ack fields on `alerts`: `acknowledged_by/at`, `ack_comment`, `ack_expires_at`; `closed_by/at`, `close_reason`.

Deferred to later milestones (schema NOT created yet): teams, on_call_schedules, grouping_rules, alert_groups, notification_rules, escalation_policies, llm_analyses, jira_links, cmdb_entries, dashboards.

## 3. Pipeline (extends `Application/Helper/Ingest.hs`)

Current ingest does: raw persist → dedupe → upsert. Extend per §5.1:

1. Persist `RawEvent` (unchanged).
2. Dedupe by fingerprint among active (firing/ack/suppressed) alerts — refire while `closed` creates a new alert.
3. Subject resolution: upsert `environments`/`hosts`/`services` by name; unknown host/service → stub with `auto_created = true`.
4. Blackout check against active windows → `suppressed` overlay flag, skip notification.
5. State machine transition (single pure function, see §4).
6. Notification dispatch — Phase-1 simplified: no rules engine yet. Notify all users with `view` privilege + a push subscription on new `critical`/`high` alerts (and on resolve of critical), throttled per fingerprint (min interval configurable, default 5m). Full `NotificationRule`s land in Phase 2.
7. Publish PG `NOTIFY` for websocket fan-out (§7).

Connector failure → `source_health` internal alert: deferred to Phase 5; keep existing job retry/backoff.

## 4. State machine

Pure core (`Application/Pipeline/StateMachine.hs`), transitions exactly per §5.2 diagram:

- `firing --ack--> ack --unack/timeout--> firing`; `ack --close--> closed`; `firing --resolved event--> resolved --refire--> firing`; `resolved --auto-close after T--> closed` (T configurable, default 24h, via new `AutoCloseJob`); `closed` terminal.
- `suppressed` is an overlay boolean/flag, not a state: blackout expiry clears it.
- Ack timeout: `ack_expires_at` set → `AutoCloseJob` also unacks expired acks.
- Illegal transition attempts are no-ops with an `AlertEvent` note, never errors.
- Property tests (hspec/hedgehog or manual generator): arbitrary event sequences never reach a state outside the diagram and always preserve `AlertEvent` audit ordering.

## 5. Auth & RBAC

- IHP built-in auth (`IHP.LoginSupport`): sessions, login page, password hashing. Seed dev users: `admin@dev` / `sre@dev` / `viewer@dev` with fixed dev passwords via seed script (env-var driven, gitignored like other tokens).
- `currentUser` in every controller; privilege helper `requirePrivilege #ack` etc.; 403 page for denied.
- Webhook ingestion endpoints stay token-authed, not session-authed.
- RBAC unit tests via controller-level tests (IHP test helpers) per §16.

## 6. UI (server-rendered HSX)

- **Layout**: nav with environments, blackouts, admin(sources), user menu; severity/status color tokens as CSS variables (theme packs deferred to Phase 3 — ship one dark + one light default now).
- **Overview `/`**: environment cards — worst severity, counts by status, alert volume (simple count-by-hour table is fine; sparkline optional). Click-through to env page.
- **Environment page `/env/:name`**: alerts table with severity/status/host/service/text filters; suppressed rows muted with blackout indicator.
- **Alert card `/alerts/:id`**: status timeline from `alert_events`, comments thread + add-comment, action buttons (ack, ack-with-timeout, close with reason), source deep link, labels/annotations, raw payload (collapsed JSON viewer). One click + optional inline comment, no modals (§11 UX rules).
- **Blackouts**: list + create/edit (scope picker env/host/service, start/end, reason). Gated by `manage_blackouts`.
- **Profile**: push subscription management, default dashboard placeholder.
- Existing `Sources` index gains nothing yet (admin CRUD is Phase 2).

Timestamps rendered local-time with UTC tooltip.

## 7. WebSocket live updates

Per §11 of highlevel:
- `/ws` endpoint (`wai-websockets`), session-cookie authenticated, one socket per tab.
- Alert/group mutations publish `NOTIFY halemans_events, '<json>'` (payload: alert id, env, kind) from the pipeline; a broadcaster thread in the web process LISTENs and fans out.
- Server renders HSX fragments (`alert-row`, `env-card`) and sends them with target ids; vanilla-JS client swaps by id. No client-side rendering.
- Reconnect with backoff; on reconnect the client reloads the page fragment set (HTTP render stays source of truth).
- Scope filter: subscribe message carries the current page (dashboard / env / alert id); broadcaster filters accordingly.

## 8. Browser push

- `Application/Service/Push.hs`: VAPID-signed Web Push (check Hackage for a maintained `web-push` library before hand-rolling JWS/encryption; VAPID keys generated at seed time into `.devenv/state/halemans/vapid.json`, public key exposed to the frontend via config).
- `POST /api/push/subscribe`, `DELETE /api/push/subscribe` → `push_subscriptions` rows.
- Dispatch from pipeline §3 step 6; each send recorded as `AlertEvent(notified)`.
- Fallback: if push permission denied/unavailable, the websocket client shows an in-page banner + Notification API where possible.
- Dead subscriptions (410 Gone) are deleted on send failure.

## 9. Background jobs

New/changed IHP jobs (all in `WorkerMain`):

| Job | Schedule | Purpose |
|---|---|---|
| `PollSourceJob` (exists as `PollZabbix`) | per-source interval | unchanged |
| `AutoCloseJob` | 5 min (`queuePollInterval` override) | auto-close resolved (TTL), unack expired acks, unsuppress expired blackouts |
| `PushNotificationJob` | enqueued by pipeline | deliver web push with retry/backoff |

## 10. Testing

- Unit: state machine (incl. property tests), severity mapping (exists), subject resolution, blackout matching, throttle logic.
- Integration: pipeline scenarios against test PostgreSQL (refire/resolve/close-refire/blackout/ack-timeout) reusing the milestone-0 smoke harness boot.
- Playwright: new suite `tests/playwright/` — login as each role, dashboard render, env filter, alert actions (ack/close), blackout create → alert muted, websocket DOM update on server-side change, push subscribe flow. Wired into `nix flake check --impure` alongside `checks.smoke`.
- Canonical gate: `nix flake check --impure`.

## 11. Acceptance checklist (end of Milestone 1)

- [ ] `devenv up` → login as seeded `sre@dev` → dashboard shows `dev` environment card with live counts
- [ ] Fire zabbix/alertmanager/grafana test alerts → arrive, dedupe on refire (occurrences++), resolve propagates, card timeline shows every step
- [ ] Ack (with timeout) and close work from the alert card; closed refire opens a new alert
- [ ] Blackout on `dev` env → new alerts suppressed + muted, no push sent; expiry restores
- [ ] Dashboard/env page update over websocket without reload (Playwright assertion)
- [ ] Push notification delivered to subscribed browser on critical alert; banner fallback works
- [ ] `viewer@dev` cannot ack/close (403); webhook endpoints still token-only
- [ ] `nix flake check --impure` green (smoke + Playwright + unit)
- [ ] No secrets committed; VAPID + dev passwords under `.devenv/state/`

## 12. Decisions

- **No grouping/teams/escalation in this milestone** — notification routing is the simplified §3 step 6; the dispatch call site is a single function so `NotificationRule`s (Phase 2) replace it without touching the pipeline.
- **`suppressed` as overlay flag** on `alerts`, matching §5.2 — keeps the core state machine four-state.
- **Web Push library**: prefer a Hackage library for VAPID + message encryption; hand-roll only if none is maintained (record choice in §13).

## 13. Implementation notes (as built)

(To be filled during implementation, like milestone_0.md §10.)
