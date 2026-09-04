# Milestone 2 — Correlation & Teams (Phase 2)

Goal: add the correlation layer from `01_highlevel.md` §17 Phase 2 on top of the milestone-1 core: grouping rules + `AlertGroup`s, teams and team-driven routing, a real `NotificationRule` engine replacing the Phase-1 simplified dispatch, escalation policies, the blackouts edit UI, sources admin CRUD, and the Grafana poller / hardened Alertmanager connectors.

Builds on the milestone-1 stack: same devenv processes, same smoke harness. LLM, CMDB/Jira, write-back, user dashboards, and theme packs stay Phase 3/4 and are out of scope.

## 1. Deliverables

| # | Deliverable | Acceptance |
|---|---|---|
| D1 | Phase-2 schema delta (teams, team_members, on_call_schedules, grouping_rules, alert_groups, notification_rules, escalation_policies, escalation_trackers) | Migrations apply clean; `build/Generated/Types.hs` regenerated |
| D2 | Grouping rule engine (ordered, first match wins, versioned) per §6 | Unit tests: match, group_key templating, ordering, fallback standalone |
| D3 | `AlertGroup` container + rollup (worst severity, member count, aggregated status) | Group card renders; env page group view toggle works (Playwright) |
| D4 | Teams admin UI + membership; `currentOnCall` stub per §7 | All routing goes through `currentOnCall`; returns team's first member |
| D5 | `NotificationRule` engine replacing `Application/Service/Notify.hs` simplified dispatch | Rules evaluated per pipeline step 7; throttle at fingerprint AND group level |
| D6 | Escalation policies + `EscalationTracker` + `EscalationJob` per §8.2 | Unacked firing alert escalates on schedule; each step = `AlertEvent(escalated)` + notification |
| D7 | Blackouts edit UI (milestone-1 deferred item) | Edit scope/window/reason of existing blackout; `manage_blackouts` gated |
| D8 | Sources admin CRUD UI with health status | Source create/edit/enable-disable; gated by `manage_sources` |
| D9 | Grafana poller connector (`PollGrafanaJob`) per §4.1 | Polled firing/resolved states reconcile with webhook path; no duplicate alerts (fingerprint dedupe) |
| D10 | Alertmanager connector hardening (§8 below) | Multi-alert payloads, missing `EndsAt`, externalURL deep links covered by tests |
| D11 | Smoke/Playwright suites extended; `nix flake check --impure` green | Check green |

## 2. Schema delta (§3 of highlevel, Phase-2 slice)

Migrations in `Application/Migration/$(date +%s)-*.sql` per project convention. Same Schema.sql parser limits as milestone 1 (columns inline in `CREATE TABLE`; FKs as top-level `ALTER ... ADD CONSTRAINT`).

New tables:
- `teams` (name unique, description, notification/escalation defaults jsonb), `team_members` (user ref, team ref, team-role hint `member|lead`).
- `on_call_schedules` (team ref, ordered member list jsonb, rotation params jsonb). Schema lands now; rotation logic stays a stub — `currentOnCall` returns the first member (highlevel §18).
- `grouping_rules` — position (ordering), name, enabled, version, match jsonb (§4), group_key template text, created_by/at.
- `alert_groups` — group_key (unique), title, environment ref, status rollup, worst severity, member count (maintained by pipeline), created_at, resolved_at.
- `alerts` gains nullable `group_id` ref + `grouped_by_version` int (highlevel §6 replay support).
- `notification_rules` — position, name, enabled, match jsonb, severity threshold, target (`team_id` XOR `user_id`), channel discriminator + channel config jsonb (v1: `browser_push` only), throttle seconds, escalation_policy ref nullable.
- `escalation_policies` — name, steps jsonb: ordered `[{after_seconds, target_team_id | target_user_id, unless_status}]`.
- `escalation_trackers` — alert ref, policy ref, current step index, next deadline, status (`active|done|cancelled`), created/updated.

Deferred (still NOT created): llm_analyses, jira_links, cmdb_entries, dashboards, push channels beyond browser_push.

## 3. Pipeline changes (extends `Application/Helper/Ingest.hs`)

Milestone-1 stages stay; two stages go from stubs to real:

- **Step 5 grouping** (was absent): evaluate enabled `grouping_rules` in `position` order → first match wins → assign alert to existing `AlertGroup` by `group_key` or create one; record `grouped_by_version`. Fallback = alert stands alone (no group). Group rollup fields recomputed on member add/resolve/state change; group resolves when all members resolve.
- **Step 7 notification dispatch** (was §3-step-6 simplified): evaluate enabled `notification_rules` in order; all matching rules fire (union of targets), each respecting its own throttle — throttled per fingerprint for ungrouped alerts, per group_key for grouped ones (group notifications replace per-alert ones per §6). Resolve-to-team via `currentOnCall`; fall back to all team members when the schedule row is missing.

Dispatch call site remains a single function (`Application/Service/Notify.hs` rewritten; the milestone-1 simplified rule becomes a seeded default `notification_rule`: severity >= high → all `sre`-team members, throttle 5m) so seeded dev behavior is unchanged until rules are edited.

Blackout interplay (unchanged semantics): suppressed alerts skip dispatch and never create escalation trackers; blackout expiry does not retro-notify.

## 4. Grouping rule engine

`Application/Pipeline/Grouping.hs` — pure core, DB rows as input:

```haskell
data MatchExpr = MatchExpr
  { meFieldEquals :: [(AlertField, Text)]   -- env/host/service/check/severity/status
  , meLabelGlobs  :: [(Text, Text)]         -- label name -> glob
  }                                          -- conjunction only, no boolean algebra in v1

matchAlert :: MatchExpr -> Alert -> Bool
groupKey   :: Template -> Alert -> Text     -- "{env}/{host}" placeholders; missing subject -> "-"
```

- Template placeholders: `{env} {host} {service} {check} {severity}` + `{label:name}`.
- Versioning: any edit bumps `version`; alerts keep `grouped_by_version` for future replay (replay itself is Phase 5).
- Group title defaults to the rule's template rendered on the first member.
- Property tests: any alert + any rule set → at most one group assignment; disabled rules never match; ordering respected.

## 5. Notification rules & channels

- Rule shape per §2. Evaluation: severity threshold (alert severity >= rule threshold, normalized ordering `critical > high > warning > info`) + same `MatchExpr` as grouping.
- Channel layer keeps the milestone-1 `BrowserPush` implementation behind a discriminator; `channel_config` jsonb is opaque per channel (browser_push config: none needed — targets resolve to users' `push_subscriptions`). Future channels (Phase 3+) add discriminators without schema change.
- Throttle anchors stay `AlertEvent(notified)` rows, now keyed by (rule, fingerprint|group_key).
- In-page websocket banner fallback unchanged.

## 6. Escalation

Per highlevel §8.2:

- When a notification for a rule with an attached `escalation_policy` fires, an `escalation_trackers` row is created (step 0, first deadline = now + step.after_seconds).
- `EscalationJob` (self-rescheduling, 30s cadence, `queuePollInterval` override per project pattern): scan `active` trackers past deadline → if alert still `firing` and not suppressed, notify step target (via `currentOnCall` for team targets), append `AlertEvent(escalated)`, advance step or mark `done`.
- Ack/close/resolve of the alert cancels the tracker (`cancelled`). Unack re-activates from step 0.
- Severity upgrade restarts escalation: out of scope for v1 (alerts don't mutate severity post-creation) — noted for Phase 5.

## 7. Teams & routing

- Teams CRUD under `/admin/teams` (name, description, member picker with member/lead hint). Gated by `manage_users`.
- `currentOnCall :: Id' "teams" -> IO (Maybe (Id' "users"))` in `Application/Service/Notify.hs`: reads `on_call_schedules`, returns first member; missing row → `Nothing` and callers fall back to all team members. Rotation params stored but unused (highlevel §18 decision).
- Team membership drives: notification/escalation targets (this milestone), dashboard defaults (Phase 3 — not yet).

## 8. Connectors

- **Grafana poller** (`PollGrafanaJob`, per-source interval like `PollZabbixJob`): fetch `/api/alertmanager/grafana/api/v2/alerts` (firing + resolved), normalize to the same fingerprint scheme as the webhook path (`grafana:<rule-uid>:<labelset hash>` — must match `Application/Helper/Ingest.hs` generic-hook fingerprints so polled and pushed events dedupe onto one alert). Cursor on `sources.last_sync_cursor` with 5-min overlap window. Purpose: reconcile states missed by push (downtime, dropped webhook); webhook stays the low-latency path.
- **Alertmanager hardening**: multi-alert webhook payloads (loop `data[]`), missing/zero `EndsAt` treated as firing-with-unknown-end (no auto-resolve), deep link from payload `externalURL` + `generatorURL` into `alerts.source_url`, per-receiver source attribution via token (already in place).

## 9. UI (server-rendered HSX, extends milestone-1 set)

- **Group card** `/groups/:id` — header (group_key, worst severity, member count, rollup status) + member alert table; group ack acts on all firing members (single confirm-less action per UX rules).
- **Environment page**: group view toggle (flat table ↔ grouped: group rows expandable), filter additions for group.
- **Admin**: grouping rules (ordered list, drag/order field, enable toggle, match editor = field-equals + label-glob rows, template input, test-against-recent-alerts preview), notification rules (same match editor + target picker + throttle + escalation policy attach), escalation policies (step list editor), sources CRUD (name/type/URL/env/poll interval/enabled + last sync cursor + health from last job result).
- **Blackouts**: add edit action to the milestone-1 list/create/delete.
- Group badge in alert-row fragment; websocket fan-out extended: group mutations publish on `halemans_events` too (same broadcaster, new fragment targets `group-row`).

## 10. Background jobs

| Job | Schedule | Purpose |
|---|---|---|
| `PollZabbixJob` | per-source interval | unchanged |
| `PollGrafanaJob` (new) | per-source interval | §8 reconcile poller |
| `EscalationJob` (new) | 30s self-rescheduling | §6 due escalation steps |
| `AutoCloseJob` | 5 min | unchanged (also cancels trackers on auto-close) |
| `PushNotificationJob` | enqueued | unchanged (now carries rule/group context for throttle keying) |

## 11. Testing

- Unit: `MatchExpr` matching + globs, group_key templating, rule ordering/first-match, rollup recomputation, severity threshold ordering, escalation step scheduling (pure deadline arithmetic), currentOnCall stub fallback.
- Integration: pipeline scenarios — two alerts same host → grouped under `env+host` rule; refire bumps group member occurrence not new alert; group notification throttling; rule edit → version bump; escalation fires on schedule and cancels on ack; grafana poller + webhook same alert dedupe.
- Playwright: create grouping rule via admin → fire matching alerts → group card renders and env group view rolls up; blackout edit; sources CRUD round-trip; RBAC (`viewer` 403 on all admin pages).
- Smoke: extend `tests/smoke/run.sh` — grafana scenario asserts BOTH webhook arrival and poller reconcile path (kill webhook delivery, verify poller still resolves).
- Canonical gate: `nix flake check --impure`.

## 12. Acceptance checklist (end of Milestone 2)

- [ ] Seeded default notification rule reproduces milestone-1 behavior on a fresh stack (fire critical → push to subscribed users)
- [ ] Grouping rule `env+host` → two checks firing on `dev-host-01` roll into one group; group card shows worst severity + 2 members; group notification sent once (throttled), not twice
- [ ] Rule edit bumps version; previously grouped alerts keep their group and version
- [ ] Escalation: firing critical unacked → step 1 notifies lead after configured delay → ack cancels; `AlertEvent(escalated)` per step on the timeline
- [ ] `currentOnCall` stub returns first team member; missing schedule falls back to all members
- [ ] Blackout edit changes window without recreate
- [ ] Sources admin: disable grafana source → poller stops, webhook still accepts; re-enable resumes from cursor
- [ ] Grafana poller reconciles a resolve missed by webhook (no duplicate alert, single fingerprint)
- [ ] Alertmanager payload with 3 alerts → 3 alerts created; missing `EndsAt` handled
- [ ] `nix flake check --impure` green (unit + integration + smoke + Playwright)
- [ ] No secrets committed; all new admin endpoints privilege-gated

## 13. Decisions

- **Match expressions are conjunctions only** (field-equals + label globs). No boolean algebra, no regex beyond glob — keeps the admin UI a form instead of a query language. Escape hatch: multiple rules.
- **All matching notification rules fire** (union of targets), unlike grouping's first-match-wins. Overrides/exclusivity deferred until a real need shows up.
- **Severity upgrade restart of escalation deferred** — alerts are immutable-severity in v1; noted in §6.
- **Grafana stays dual-path** (webhook fast path + poller reconcile) rather than poller-only: keeps milestone-0/1 ingestion latency while gaining exactly-once-ish semantics via shared fingerprints.
- **On-call rotation still stubbed** per highlevel §18 — schema lands now so Phase 3+ rotation needs no callers changed.
