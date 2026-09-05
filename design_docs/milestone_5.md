# Milestone 5 — Hardening (Phase 5)

Goal: deliver the Phase-5 slice from `01_highlevel.md` §17: raw-event retention, source-health alerting, performance validation at the §15 targets, and audit exports. Also picks up the deferred M4 polish: re-triggering LLM analysis when context enrichment lands after the first analysis ran without it.

Builds on the milestone-4 stack: same devenv processes, same smoke harness, no new external services. All work is internal to the app: one new periodic job family, source-health wiring in the existing pollers/webhooks, read-only export endpoints, and index/perf work validated by a repeatable harness.

## 1. Deliverables

| # | Deliverable | Acceptance |
|---|---|---|
| D1 | Phase-5 schema delta (retention_configs, audit_exports, perf-relevant indexes) | Migrations apply clean; `build/Generated/Types.hs` regenerated |
| D2 | `RetentionJob` per §13: daily prune of old `raw_events` per retention config | With retention set to N days, rows older than N are gone after the job runs; nothing newer touched; job is idempotent and resumable (batched delete) |
| D3 | Source-health alerting per §4.1/§15: connector failures and webhook silence produce internal `source_health` alerts surfaced on dashboard + admin sources page | Kill a source → internal alert appears on dashboard and admin page within one poll interval × backoff; source recovers → alert resolves |
| D4 | Exponential backoff on connector failures per §4.1, with per-source health state persisted | Repeated failures lengthen the poll interval up to a cap; recovery restores the configured interval; backoff state visible on the admin sources page |
| D5 | Audit export: downloadable CSV/JSONL of `alert_events` (optionally scoped by alert/environment/time range), exports recorded | Export matches DB content for the selected scope; every export lands a row in `audit_exports` (who/when/scope/row count); privilege-gated |
| D6 | Performance validation against §15 targets: dashboard p95 < 300ms at 10k active alerts, WS fan-out < 1s | Repeatable harness (seeded 10k-alert dataset) reports p95; indexes on fingerprint/status/env/group verified by EXPLAIN in the harness output |
| D7 | M4-deferred polish: LLM re-analysis after enrichment lands | Alert whose first analysis ran before CMDB/Jira context existed gets one automatic re-analysis when `EnrichAlertJob` completes; prompt-hash dedupe does not suppress it (context changed → hash differs); at most one enrichment-triggered re-analysis per alert |
| D8 | Job failure metrics on the admin page per §15 (failed/retried counts per job type, recent failures) | Admin page shows per-job-type counters and the last N failures with error text |
| D9 | Smoke/Playwright suites extended; `nix flake check --impure` green | Check green |

## 2. Schema delta (§3 of highlevel, Phase-5 slice)

Migrations in `Application/Migration/$(date +%s)-*.sql` per project convention. Same Schema.sql parser limits as before (columns inline in `CREATE TABLE`; FKs as top-level `ALTER ... ADD CONSTRAINT`).

New tables:
- `retention_configs` — singleton-style config row (enforced by a bool `active` partial-unique index or fixed-id upsert): `raw_events_days` int (default 30 per §15), `alert_events_days` int nullable (null = keep forever — audit trail default), `enabled` bool, updated_at/updated_by. v1 prunes `raw_events` only; the alert-events knob is schema-reserved for a later phase so the shape doesn't change again.
- `audit_exports` — user ref, scope jsonb (`{alert_id | environment | from, to}`), format text (`csv|jsonl`), row_count int, created_at. Records every export (§15 auditability); the exported bytes themselves are streamed, not stored.

Modified tables:
- `sources` — add `consecutive_failures` int (default 0), `last_error` text nullable, `next_poll_at` timestamptz nullable (backoff cursor; null = poll on schedule). Drives D3/D4 display and scheduling.
- Indexes (D6, verified by EXPLAIN): composite indexes supporting the hot dashboard/env queries — alerts `(status, environment)`, alerts `(fingerprint) WHERE status <> closed` partial, alerts `(group)`; alert_events `(alert, created_at)`. Final list fixed by the perf harness results, not guessed.

Deferred (still NOT created): on-call rotation tables beyond the existing stub; recurring blackouts (§18).

## 3. Retention (§13, §15)

`Application/Job/RetentionJob`:
- Schedule: daily (self-rescheduling IHP job, like `JiraSyncJob`); also enqueued once at boot if overdue (last-run marker in the config row or job table).
- Deletes `raw_events` where `received_at < now() - raw_events_days`, batched (`DELETE ... WHERE id IN (SELECT ... LIMIT N)` loop) so a 30-day backlog prune doesn't hold one giant transaction; each batch commits; job is re-runnable after interruption.
- Emits a summary log line (batches, rows, duration) and an `AlertEvent`-style internal record only on failure — retention is housekeeping, not alertable signal.
- `enabled = false` → job logs and exits (config knob for environments where compliance requires keeping raw payloads).
- Retention of `llm_budget_counters`, `audit_exports`, `cmdb_entries` (TTL already handled by M3 refresh): explicitly out of scope; `audit_exports` rows are never auto-pruned.

## 4. Source health & backoff (§4.1, §15)

- **Failure detection**: poller jobs (`PollZabbixJob`, `PollGrafanaJob`) catch connector errors per source; webhook path detects *silence* — a push source with no event for `3 ×` its expected interval (configurable per source, disabled when unset) via the retention-scan cadence or a lightweight periodic check.
- **State**: on failure, `sources.consecutive_failures++`, `last_error` set, `next_poll_at = now + backoff(failures)`; on success, all three reset. Backoff: `interval × 2^failures` capped at 30min, jittered ±10%.
- **Internal alert**: first failure creates a normal alert through the existing pipeline with fingerprint `halemans:source-health:<source_id>`, severity `warning`, title from source name — so dedupe, grouping, notifications, and the dashboard all work unmodified. Recovery (first successful poll / inbound webhook) posts a resolved event for the same fingerprint. Escalates to `high` after 5 consecutive failures (severity upgrade restarts escalation per §8.2 — free).
- **Surfacing**: admin sources page shows per-source health (state, consecutive failures, last error, next poll time); dashboard shows the internal alerts like any other. No new WS kind needed — reuse the existing alert fragments.
- **Self-DoS guard**: the source-health alerts bypass notification throttling only at creation; refires are ordinary dedupe hits (occurrences++), so a flapping source can't flood push notifications.

## 5. Audit export

- `GET /admin/audit/export?from&to&environment&alert&format=csv|jsonl` — `admin` privilege; streams `alert_events` joined to alert title/environment, ordered by created_at. CSV: fixed column set; JSONL: the full row + joins. Content-Disposition with timestamped filename.
- Scope is at most one of alert/environment plus a time range (default: last 7 days) — keeps exports bounded; a full-history export is an ops task, not a UI button.
- Every request inserts an `audit_exports` row before streaming (row count filled after; failure mid-stream still leaves the attempt recorded with partial count).
- Admin page lists recent exports (who/when/scope/count) — export of the export log itself is just another scope.

## 6. Performance validation (§15)

- `nix/scripts/perf-harness.sh` (dev tooling, not a devenv service): builds a scratch DB, seeds 10k active alerts across ≥5 environments with realistic distribution (mixed severities, ~30% grouped, alert_events ~20/alert, cmdb/jira/llm rows on a subset), then times: overview dashboard, environment page, alert card, and a WS broadcast fan-out to N connected clients (default 10).
- Asserts §15 numbers: dashboard p95 < 300ms, WS fan-out < 1s. Runs against the prod binaries; output is a table of p50/p95/p99 per endpoint, plus EXPLAIN (ANALYZE off) plans for the dashboard rollup queries proving the D1 indexes are used.
- Wired into `nix flake check` as a non-blocking report check only if runtime stays small; otherwise documented as a manual gate before shipping M5. Not part of the smoke suite — 10k seeds would make every check slow.
- Any query fixed during this work lands as an index or a query change in the normal migration path — no "perf-only" local tweaks.

## 7. M4-deferred polish: enrichment-triggered re-analysis

- `EnrichAlertJob` completion checks: does the alert have a `done` `llm_analyses` row created *before* the enrichment landed? If yes and no enrichment-triggered re-analysis ran yet, insert one fresh analysis + enqueue `LlmAnalysisJob` (same path as manual re-analyze, actor `system`).
- Guard: a nullable flag/derived marker (e.g. `llm_analyses.error_message = 'enrichment_retrigger'` convention or a jsonb flag in the prompt-hash row) so exactly one such re-analysis happens per alert — enrichment jobs must not start an analysis loop.
- Prompt-hash dedupe does not suppress it legitimately: the new CMDB/Jira context changes the rendered prompt (§5 of M4 doc) → different hash. Test asserts the hash actually differs (regression guard against a prompt builder that stops including context).

## 8. Job failure observability (§15)

- Admin page section: per-job-type counters (failed, retried, succeeded in last 24h) from the IHP job tables, plus the last 20 failed rows with error text and updated_at. Read-only; no schema change (job tables already carry the data).
- No metrics exporter in v1 — the admin page is the §15 surface; Prometheus-style `/metrics` is a v1.1 candidate.

## 9. Background jobs

| Job | Schedule | Purpose |
|---|---|---|
| `RetentionJob` (new) | daily, self-rescheduling | prune `raw_events` per `retention_configs` (§3) |
| `PollZabbixJob` / `PollGrafanaJob` | unchanged cadence; honoring `next_poll_at` | backoff state respected (§4) |
| `LlmAnalysisJob` | unchanged | additional enqueue source: enrichment-triggered (§7) |
| `EnrichAlertJob` | unchanged + §7 hook | triggers one re-analysis when landing late context |
| `WriteBackJob`, `JiraSyncJob`, `EscalationJob`, `AutoCloseJob`, `PushNotificationJob` | unchanged | — |

## 10. Dev fixtures & mocks

- No new mock servers. Zabbix/Grafana/Alertmanager/mocks from M0–M4 suffice: source-health tests kill/restart the existing mock or zabbix process, or fire the mock-llm `/debug/fail` pattern analog where applicable.
- `seed-halemans` (and the inline copy in smoke-check.sh — keep both in sync per project convention) seeds the default `retention_configs` row (30 days, enabled).
- Perf dataset generator lives in `nix/scripts/perf-harness.sh` + a Haskell seeder module — deliberately separate from demo fixtures so the dev DB stays small.
- `stack-status` unchanged (retention job has no health endpoint; its last-run status shows on the admin job-metrics section from D8).

## 11. Testing

- Unit: backoff schedule computation (doubles, caps at 30min, jitter bounds); retention batch-delete boundary (cutoff date exact, keeps newer rows); source-health fingerprint/severity logic (first failure → warning internal alert, 5th → high, recovery → resolved event); export CSV/JSONL serialization round-trip; re-analysis guard (fires once, not twice).
- Integration: seed old `raw_events` → run `RetentionJob` → gone, newer kept, re-run is a no-op; kill mock source → internal alert created after backoff ticks, restore → resolved; export endpoint returns exactly the scoped rows and inserts an `audit_exports` row; enrichment-after-analysis → exactly one new analysis with a different prompt hash; retention disabled → job exits without deleting.
- Playwright: dashboard shows the source-health alert without reload (existing alert WS path); admin sources page shows failure state and recovery; admin audit export downloads a file and the export log row appears; admin job-metrics section renders counters.
- Smoke: extend `tests/smoke/run.sh` — after the source scenarios, stop the alertmanager webhook flow for a window (or directly mark a source failed via SQL) and assert the internal alert appears; run retention manually and assert pruned counts.
- Perf: harness run documented in the milestone close-out; indexes verified via its EXPLAIN output.
- Canonical gate: `nix flake check --impure`.

## 12. Acceptance checklist (end of Milestone 5)

- [ ] Set retention to 1 day, insert backdated raw events, run `RetentionJob` → old rows gone, recent kept, second run deletes nothing
- [ ] Retention disabled in config → job runs, deletes nothing, logs skip
- [ ] Stop a polled source → `halemans:source-health:*` alert on the dashboard within the backoff window; admin sources page shows consecutive failures + next poll time
- [ ] Restore the source → internal alert resolves; backoff state resets
- [ ] Flapping source over 5 failures → severity upgrades to `high` once; no push-notification flood
- [ ] Export last-24h alert events as CSV and JSONL → contents match the DB for that scope; `audit_exports` row records user, scope, format, row count
- [ ] Non-admin hits the export endpoint → denied
- [ ] Perf harness at 10k active alerts: dashboard p95 < 300ms, WS fan-out < 1s, EXPLAIN shows the new indexes used
- [ ] Alert analyzed before enrichment lands → exactly one automatic re-analysis after `EnrichAlertJob` completes, prompt hash differs, card shows the newer analysis
- [ ] Admin page shows per-job-type failure counters and recent failure details
- [ ] `nix flake check --impure` green (unit + integration + smoke + Playwright)
- [ ] No secrets committed; perf dataset generator produces no tracked artifacts

## 13. Decisions

- **Source-health signals are ordinary alerts** (fingerprint `halemans:source-health:<source_id>`) rather than a parallel status panel: dedupe, grouping, notification rules, history, and WS updates come for free, and §4.1's "surfaced on dashboard" is literally true. The cost is they can be grouped/acked like real alerts — acceptable; a grouping-rule exclusion note goes in the seeded rules.
- **Webhook silence detection only where an expected interval is configured**: pure alertmanager sources have no poller (MEMORIES: no reverse reconcile exists), so absence-based detection needs an explicit per-source expectation or it would false-positive constantly.
- **Retention deletes in batches with per-batch commits** instead of one `DELETE`: a 30-day first-run prune can be large; batching keeps vacuum/lock pressure down and makes the job crash-safe without a checkpoint table.
- **`audit_exports` stores metadata, not bytes**: exports are derived data; re-running the scope reproduces them. Storing files would invent a blob-storage problem for zero audit value.
- **Perf harness is dev tooling, not a flake check gate** (unless fast): a 10k-seed check on every `nix flake check` would punish every unrelated iteration. It runs before M5 sign-off and on demand; §15 targets are asserted by it, not by CI.
- **Enrichment re-analysis is capped at exactly one per alert**: the M4 doc left ordering loose deliberately; an unbounded "re-analyze whenever context changes" rule would loop analysis jobs on chatty CMDB/Jira refreshes. Manual re-analyze remains available for anything beyond that.
- **Job metrics are read from the IHP job tables, not a new metrics store**: §15 asks for failure metrics "surfaced on admin page" — the rows already exist; a `/metrics` exporter is deliberately v1.1.
