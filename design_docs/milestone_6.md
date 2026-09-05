# Milestone 6 — Public API & observability (v1.1)

Goal: deliver the two v1.1 candidates named in `01_highlevel.md` and deferred by M5: a read-only JSON API for automation/CLI tooling (§12: `GET /api/v1/alerts`, "likely v1.1") and a Prometheus-style `/metrics` exporter (M5 §8 decision). Adds the token-authentication substrate both need.

Builds on the milestone-5 stack: same devenv processes, same smoke harness, no new external services, no new mocks. All work is internal: one new table, a new controller namespace serving JSON, and a scrape endpoint reading tables that already exist (job tables per M5 D8, `llm_budget_counters` per M4, `sources` health columns per M5).

## 1. Deliverables

| # | Deliverable | Acceptance |
|---|---|---|
| D1 | v1.1 schema delta (`api_tokens`) | Migration applies clean; `build/Generated/Types.hs` regenerated |
| D2 | API token management: create/list/revoke in profile UI, admin revoke-any | Token shown once at creation; revoked token → 401 immediately; `last_used_at` updates on use |
| D3 | `GET /api/v1/alerts` — list with filters (environment, status, severity, fingerprint, host, since/until) and cursor pagination | Filtered results match DB content exactly; stable ordering; page traversal returns every matching row exactly once |
| D4 | `GET /api/v1/alerts/:id` — alert detail incl. subject refs, group membership, latest LLM analysis summary, and full `alert_events` timeline | Response shape documented; 404 for unknown id; timeline ordered by created_at |
| D5 | `GET /api/v1/environments` — environment rollup (worst severity, counts by status) mirroring the overview dashboard query | Counts match the dashboard HTML for the same data |
| D6 | `GET /metrics` — Prometheus text exposition, token-gated | Scrapes expose the §5 metric set; values match DB counts; invalid/absent token → 401 |
| D7 | Per-token rate limiting on `/api/v1/*` and `/metrics` | Exceeding the limit → 429 + `Retry-After`; normal cadence unaffected |
| D8 | Smoke suite extended (curl-level API + metrics assertions); `nix flake check --impure` green | Check green |

## 2. Schema delta

Migration in `Application/Migration/$(date +%s)-*.sql` per project convention. Same Schema.sql parser limits as before (columns inline in `CREATE TABLE`; FKs as top-level `ALTER ... ADD CONSTRAINT`).

New table:
- `api_tokens` — user ref, name text (user-chosen label), token_hash text (sha256 of the bearer secret; plaintext never stored), prefix text (first 8 chars, for display/identification in logs), scopes text array (`alerts:read`, `metrics`), last_used_at timestamptz nullable, expires_at timestamptz nullable, revoked_at timestamptz nullable, created_at. Unique index on token_hash. Index on (user ref) for the profile list.

Modified tables: none. No other schema — the API and metrics read existing tables.

Deferred (still NOT created): on-call rotation tables, recurring blackouts, `alert_events` pruning (the `retention_configs.alert_events_days` knob stays reserved).

## 3. Read-only JSON API (§12)

- Controller namespace `Web.Controller.Api` under `/api/v1`; responses `application/json` via aeson. Explicit encoder functions (`Application/Service/Api/Encode.hs`), not `ToJSON` instances on Generated types — the wire shape must not drift when the schema does. Field naming: snake_case, timestamps ISO-8601 UTC.
- **Auth**: `Authorization: Bearer <token>`; token resolves via hash lookup to user + scopes. Endpoints require scope `alerts:read` AND the underlying user's `view` privilege — a token never outlives or widens its owner's access (privilege re-checked per request, so demoting a user constrains their tokens). Revoked/expired → 401 with a JSON error body `{error, message}`.
- **`GET /api/v1/alerts`**: query params `environment` (name), `status`, `severity`, `fingerprint` (exact), `host`, `service`, `since`/`until` (on `last_seen_at`), `limit` (default 100, max 500), `cursor`. Ordering `(last_seen_at desc, id desc)`; cursor = opaque base64 of the last row's key — stable under concurrent inserts, no offset drift. Response: `{alerts: [...], next_cursor: ... | null}`.
- **`GET /api/v1/alerts/:id`**: full alert row + environment/host/service names, group id + title when grouped, latest `done` LLM analysis (markdown + structured fields, provider/model/timestamp — advisory metadata is exactly what CLI tooling wants for triage), and the `alert_events` timeline. Jira links + CMDB refs included as already stored (no live upstream calls — the API never fans out to external systems).
- **`GET /api/v1/environments`**: the dashboard rollup as JSON (worst severity, per-status counts, suppressed count) — same query module the dashboard uses, so the numbers agree by construction.
- **Versioning**: additive-only evolution within v1 (new fields/endpoints allowed; renames/removals require v2). Documented in the endpoint's error body `error` codes being stable strings.
- Errors: JSON `{error: code, message}`; 400 on bad params (with the offending param named), 401/403 as above, 404 unknown id, 429 rate-limited, 500 generic. No HTML error pages under `/api/v1`.

## 4. Token management UI

- **Profile page** gains an "API tokens" section: create form (name + scope checkboxes; v1 scopes: `alerts:read`, `metrics`), plaintext shown exactly once post-creation (copy box), list with name/prefix/scopes/last_used_at/expires_at/revoke button. Users manage their own tokens; `admin` privilege can revoke any token from the admin users page.
- Token generation: 32 random bytes, base64url; stored as sha256 hash — DB leak doesn't leak usable tokens (same posture as password hashing per §14).
- `last_used_at` updated at most once per minute per token (guard on `now() - last_used_at > 1 min` before the UPDATE) so a hot polling loop doesn't write on every request.

## 5. Metrics exporter (`/metrics`)

- Prometheus text exposition format, hand-rolled (the format is trivial; no prom-client dependency). Gauges computed from the DB at scrape time — scrape cadence is tens of seconds, counts at 10k-alert scale are milliseconds (M5 perf data), so no metric caching layer.
- Metric set (v1):
  - `halemans_alerts{environment,status,severity}` gauge — active-alert counts (same rollup family as D5).
  - `halemans_source_consecutive_failures{source}` gauge, `halemans_source_healthy{source}` 0/1 — from `sources` M5 columns.
  - `halemans_job_runs_total{job,status}` counter-shaped gauge — cumulative counts from the 11 IHP job tables (statuses `job_status_*`, same read path as M5's JobMetrics.hs).
  - `halemans_llm_tokens_today{provider,direction=in|out}` gauge — from `llm_budget_counters`.
  - `halemans_ws_connections` gauge — live count from the Live.hs scope registry.
  - Build info: `halemans_build_info{version}` 1.
- **Auth**: `metrics`-scoped bearer token (§4) — Prometheus `authorization` config; no session-cookie path. Bind stays on the app port; no separate listener in v1.
- Counters that reset on restart are avoided deliberately: everything is derived from durable rows, so Prometheus sees monotonic series where the metric name says `total`.

## 6. Rate limiting

- Per-token token bucket, in-memory in the web process (single-node per §2 of highlevel — no distributed limiter needed): default 120 req/min per token for `/api/v1/*`, 6 req/min for `/metrics` (scrape-friendly). Over limit → 429 + `Retry-After`, JSON error body on the API.
- Bucket state is lost on restart — acceptable: limits are abuse guards, not quotas.

## 7. Background jobs

| Job | Change |
|---|---|
| All existing jobs | unchanged — metrics read their tables read-only |

No new jobs; no schedule changes.

## 8. Dev fixtures & mocks

- No new mock servers. Smoke creates a token via SQL insert of a known hash (test-only fixed secret) rather than driving the profile UI — the UI round-trip is covered by Playwright.
- `seed-halemans` (and the inline copy in smoke-check.sh — keep both in sync per project convention) seeds one demo token for the sre@dev user with both scopes.
- `stack-status` unchanged.

## 9. Testing

- Unit: cursor encode/decode round-trip; filter→SQL mapping per param; JSON encoders golden-tested against documented shapes (regression guard for wire-shape drift); token hash verification; rate-limiter bucket math; metrics text rendering (label escaping, monotonicity of job counters).
- Integration: seed known alert set → each filter returns exactly the matching rows; cursor pagination over 3 pages returns every row exactly once and terminates with `next_cursor: null`; revoked token → 401; demoted user's token → 403; token with only `metrics` scope → 403 on `/api/v1/alerts`; `/metrics` body parses as Prometheus text and gauge values match DB counts; rate limit → 429 with `Retry-After`.
- Playwright: profile page create-token flow (plaintext shown once, list row appears, last_used_at fills after an API call); revoke → subsequent API call 401s.
- Smoke: extend `tests/smoke/run.sh` — after the source scenarios, curl `/api/v1/alerts?environment=...` with the seeded token and assert the fired alert appears; curl `/metrics` and grep `halemans_alerts` + `halemans_source_healthy`.
- Canonical gate: `nix flake check --impure`.

## 10. Acceptance checklist (end of Milestone 6)

- [ ] Create a token in the profile UI → plaintext shown once; bearer call to `/api/v1/alerts` succeeds; `last_used_at` set
- [ ] Filter combinations (environment+status+severity, fingerprint, since/until) match psql counts for the same scope
- [ ] Paginate a >limit result set to exhaustion → every row exactly once, `next_cursor` ends null
- [ ] Alert detail returns timeline ordered, group membership, and latest LLM analysis when present
- [ ] `/api/v1/environments` counts equal the dashboard's counts for the same data
- [ ] Revoked/expired token → 401; `alerts:read`-only token hitting `/metrics` → 403 (and vice versa); demoted user's token → 403
- [ ] `/metrics` scrape: all §5 metric families present, values match DB, invalid token → 401
- [ ] Hammer an endpoint past the limit → 429 + `Retry-After`; waits → succeeds again
- [ ] Non-JSON-error invariant: no HTML error page under `/api/v1` or `/metrics` for any failure mode tested
- [ ] `nix flake check --impure` green (unit + integration + smoke + Playwright)
- [ ] No secrets committed; token plaintext never logged (grep server logs in smoke)

## 11. Decisions

- **Read-only v1 API** per §12: ack/close stay UI-only until automation proves the need; a write API also forces API-side CSRF/audit semantics that a read-only scope dodges. The token/scope substrate is write-ready (`alerts:write` slots in without schema change).
- **Explicit encoders over `ToJSON` on Generated types**: the wire contract must survive schema/codegen churn; golden unit tests pin the shapes.
- **Keyset (cursor) pagination over OFFSET**: alerts churn constantly; offset pagination skips/duplicates rows under concurrent ingestion, which is exactly the CLI-tooling use case this API serves.
- **Metrics computed at scrape time from durable tables**, not an in-process counter registry: restarts can't zero the series, and there is one source of truth (the DB) — consistent with the M5 decision to surface job metrics from job tables rather than a metrics store.
- **`/metrics` token-gated rather than localhost-only**: the single-node topology makes "localhost" ill-defined under container/remote-scrape setups, and the token substrate already exists for the API. Unauthenticated bind remains a deployment-level reverse-proxy choice.
- **Tokens inherit and re-check owner privileges**: simpler than detached API roles, and demotion/lockout propagates for free — matches §7's "roles are data" posture.
- **Rate limiting in-memory**: §2 single-node assumption; the limits are abuse guards, not billing quotas, so restart-amnesia is fine.
- **On-call rotation and recurring blackouts stay out** (§18 leftovers): they are scheduling-domain work with their own edge cases and get their own milestone; mixing them into the API milestone would double its surface for no coupling benefit.
