# Milestone 8 — Enrichment Phase 0 (Jira Assets + agent roles)

Goal: implement the Phase-0 scope of `design_docs/enrichment-tool-overview.md`:
a read-only Jira Assets (Insight) client per `design_docs/assets-api.md`, an
app-side asset cache, an asset info card on alerts, asset context in the LLM
prompt, an admin CRUD UI for info sources, an agent-role abstraction for LLM
enrichment (name + prompt template + tool set, selectable per enrichment run),
and a mock Assets server for tests.

Builds on the milestone-7 stack: same devenv processes, same smoke harness,
same mock conventions (mock-confluence :18082, mock-jira :18083, mock-llm
:18084 → mock-assets :18085). Existing LLM enrichment (M4) stays the default
path; agent roles wrap it rather than replace it.

Authoritative API contract: `design_docs/assets-api.md`. Read-only regression
reference: `design_docs/examples/jira_assets_api_readonly_test.sh`.

## 1. Deliverables

| # | Deliverable | Acceptance |
|---|---|---|
| D1 | Schema delta (assets_configs, assets_objects, asset_alert_links, llm_agent_roles) | Migrations apply clean; `build/Generated/Types.hs` regenerated |
| D2 | Assets REST client (`Application/Service/Assets.hs` + `Assets/Types.hs`, `Assets/Aql.hs`, `Assets/Errors.hs`) | All §4 read endpoints of assets-api.md callable; three error shapes classified per §3; no redirect following |
| D3 | Asset cache + alert linkage: `EnrichAlertJob` gains an assets step (match alert host/service per config query template, upsert cache, link alert) | New alert on a known host → `assets_objects` row + link; unknown host → negative cached (no card blocking); soft-fail on Assets outage |
| D4 | Asset info card panel on alert (object type, owner, cluster/db, IPs, datacenter, "open in Jira Assets" link, fetched_at) | Panel renders from cache; live-updates via WS like the CMDB panel |
| D5 | LLM prompt integration: `PromptInputs` gains an assets excerpt; truncation order updated | Rendered prompt contains owner/cluster/DC lines when a linked asset exists; budget respected |
| D6 | `assets_lookup` LLM tool in `Application/Service/Llm/Tools.hs` (AQL-backed, read-only) | Model can call it; result text soft-fails in-band on Assets errors, like cmdb_lookup |
| D7 | Admin CRUD for info sources (`/admin/assets`): assets_configs list/new/edit/toggle, connection test | Admin-only; enable/disable without delete; connection test reports reason |
| D8 | Agent roles: `llm_agent_roles` table + admin UI on `/admin/llm`; role = name + prompt template ref + tool whitelist + is_default flag | Manual "re-run analysis" offers an enabled-role dropdown; chosen role drives prompt template and tool set; no role → current behaviour |
| D9 | Mock Assets server `nix/mocks/mock_assets.py` (:18085, python stdlib, seeded dataset) | Reproduces the verified response shapes (mixed envelopes, Int ids, shape-A 404); smoke/integration run without external Jira |
| D10 | Unit + integration + smoke/Playwright suites extended; `nix flake check --impure` green | Check green |

Deferred (NOT in this milestone): Assets write operations (object create/update),
import pipeline endpoints, chat-conversation sources, per-source scheduling of
cache refresh jobs (cache is refreshed on-demand from the enrich job only),
provisioning of assets configs (can follow the M7 pattern in a later milestone).

## 2. Schema delta

Migrations in `Application/Migration/$(date +%s)-*.sql` per project convention
(columns inline in `CREATE TABLE`; FKs as top-level `ALTER ... ADD CONSTRAINT`;
never name a column `error`).

New tables:

- `assets_configs` — name (unique), base_url (pointing at
  `.../rest/assets/latest`), token_env (name of env var holding the bearer
  token — same `${...}` convention as other connectors, never plaintext in DB),
  auth_mode (`bearer|basic`, default bearer; basic needs jira_email_env),
  default_schema_name (AQL `objectSchema = "<name>"` value — the display name,
  not the key, assets-api.md §5.3), host_query_template (AQL template with a
  `{host}` placeholder, e.g. `objectSchema = "Capacity CMDB" AND Name like
  "{host}"`), enabled bool, created/updated.
- `assets_objects` — cache rows per enrichment-tool-overview §"Implementation
  details" (link to source + actualization timestamp): config ref,
  object_id int8 (JSON-number ids, §8.2), object_key, label,
  object_type_name, attributes jsonb (subset: flattened name → display value
  map plus status name), icon_url (stored verbatim, §8.4), source_url
  (object deep link), fetched_at, created/updated. Unique (config_id,
  object_id).
- `asset_alert_links` — alert ref, assets_object ref, matched_by (the query
  input that produced the link, e.g. host fqdn), created_at. Unique
  (alert_id, assets_object_id).
- `llm_agent_roles` — name (unique), description, prompt_template_name
  (references `llm_prompt_templates.name`, default `alert_enrichment`),
  tools jsonb (array of tool names; empty = no tools; known names:
  `cmdb_lookup`, `jira_search`, `assets_lookup`), enabled bool,
  is_default bool (partial unique index: one default), created/updated.

Modified tables:

- `llm_analyses` gains nullable `agent_role_id` (FK llm_agent_roles) recording
  which role produced the analysis. Nullable keeps existing rows valid; the
  generated field must not clash with Prelude (`agent_role_id`, not `role`).

## 3. Assets client (`Application/Service/Assets.hs`)

Per assets-api.md §9, adapted to existing conventions (wreq, tokens via env,
connection tests returning `Either Text ()`):

- `Assets/Types.hs` — `ObjectSchema`, `ObjectType`, `ObjectTypeAttribute`,
  `AssetObject`, `ObjectAttribute`, `ObjectAttributeValue`, `ObjectHistory`,
  `ObjectListResult`, `Ticket`, `StatusType`, `Icon`. IDs as `Int64` (server
  returns JSON numbers, §8.2). Mixed-envelope tolerance (§8.1): a helper
  decoder accepting bare arrays and the wrapped forms
  (`objectschemas`/`objectEntries`/`values`/...).
- `Assets/Aql.hs` — opaque `Aql` newtype over Text + safe builders only:
  `schemaEq`, `attrLike`, `attrEq`, `andAql`, `typeAndChildren`, with `\`/`"`
  escaping (§5.4). No raw string concatenation of user input into queries.
  `{host}` placeholders in config templates are filled through the escaper.
- `Assets/Errors.hs` — `AssetsError = AuthFailed | NotFound Int64 |
  Redirected | Upstream Int Text | InvalidResponse Int Text`. Classification
  per §3: 401 → AuthFailed; 404 with shape-A JSON containing the stable
  `NotFoundInsightException:` prefix → NotFound (match structure, not the
  Russian text); any 3xx → Redirected (login-page fallback); shape-B XML or
  HTML on a known route → InvalidResponse (log first 200 chars).
- **Redirect policy differs from the other connectors**: `Application/Service/Http`
  follows 301/302, but the Assets API 302s unknown/unauth paths to an HTML
  login page (§8.6) and following that hides auth failures. The Assets client
  uses wreq directly with `checkResponse` disabled and no redirect following;
  do NOT route it through `getFollowing`.
- Read operations per §10 canonical flow: `listSchemas`, `listTypesFlat`,
  `searchObjects` (GET `/aql/objects`, `qlQuery` URL-encoded, page 1-based,
  iterate while `toIndex < totalFilterCount`), `objectDetail`,
  `objectHistory`, `connectedTickets`, `statusType`, `icon`. Write endpoints
  ([SPEC] in §4) are not implemented.
- Config resolution: `assets_configs` row + token from `token_env`; timeout
  ~30 s (`managerResponseTimeout`), retry only on 5xx/timeout, max 2 with
  backoff — same shape as the LLM client.

## 4. Cache + alert linkage (`EnrichAlertJob` step c)

Extends the existing M3 enrich job (CMDB + Jira) with a third, independent
soft-fail step:

- For each enabled `assets_configs` row: resolve the alert's host (and, when
  present, service) subject, render the config's `host_query_template` with
  the escaped subject, `searchObjects`, take top N (5).
- Each hit: `objectDetail` fetch → upsert `assets_objects` (fresh fetched_at)
  + insert `asset_alert_links` on conflict do nothing.
- Negative results (no hits) are cached as a short-TTL marker on the link
  attempt (30 min, same idea as the M3 CMDB negative cache) to avoid
  re-querying for unknown auto-created stub hosts on every dedupe hit.
  Enqueue policy unchanged: enrich runs on new alerts only.
- Failures: `AlertEvent(enrichment_failed)` with subsystem tag `assets`; the
  other subsystems (CMDB, Jira, LLM) are unaffected.
- Results publish on `halemans_events` with a new WS kind `assets` on the
  alert scope (same fragment pattern as the `enriched` kind).

## 5. Asset card panel

- New fragment in `Web/View/Fragments.hs` (rendered to Text for both the card
  view and the WS broadcaster, needs `?request`/`?context` as existing
  fragments do).
- Content per enrichment-tool-overview Phase-0 target: object type, owner,
  cluster and/or database name, IP addresses, datacenter — extracted from the
  flattened `attributes` jsonb by a configurable attribute-name list on the
  config (comma-separated, with sane defaults covering the verified schema:
  `Owner`, `Cluster`, `Database`, `IP`, `Datacenter`). Unknown/absent
  attributes are simply omitted.
- Plus: label + icon, status badge (statustype category colour),
  `object_key` chip, "Open in Assets" deep link, `fetched_at` timestamp,
  manual refresh button (any `view` user, like the CMDB refresh).
- Live update: `contextPanelUpdates` in `Application/Service/Live.hs` learns
  the `assets` kind.

## 6. LLM integration

- Prompt: `PromptInputs` gains `piAssetsExcerpt`. `buildPromptForAlert`
  renders linked assets (label, object type, owner, cluster, IPs, datacenter,
  status) into the active template. Truncation order becomes: similar_alerts
  → events → assets → cmdb_excerpt → jira_links → description. Title,
  severity, labels, annotations still never truncated. Existing
  `alert_enrichment` template gains an `{{assets_excerpt}}` slot via a new
  template row revision; missing slot in old rows = slot silently empty
  (renderTemplate already tolerates absent bindings).
- Tool: `assets_lookup(term)` in `Llm/Tools.hs` — runs the default enabled
  config's template with the model-supplied term, returns a compact text
  summary (label, key, type, owner, status, top attributes) capped to the
  tool-result budget. Failures returned in-band as text (soft-fail), exactly
  like `cmdb_lookup`/`jira_search`. Read-only; nothing mutates alert state or
  calls Assets write endpoints.
- The prompt-hash dedupe (M4) automatically covers the new excerpt via the
  rendered-prompt hash — no changes needed there.

## 7. Agent roles

Abstraction per enrichment-tool-overview Phase-0 targets 5–6: a role bundles
role name + prompt template + set of available tools.

- `llm_agent_roles` rows are resolved DB-first alongside `llm_configs`
  (`Application/Service/Llm/DbConfig.hs` pattern). `LlmAnalysisJob` accepts an
  optional role id; when absent, the `is_default` role applies; when no roles
  exist at all, behaviour is exactly today's (template `alert_enrichment`,
  full built-in tool set when tools are enabled).
- Role application: the role's `prompt_template_name` replaces the hardcoded
  template name in `buildPromptForAlert`; the role's `tools` array filters
  `toolDefinitions` by name before they go into the chat-completion payload;
  the resulting `llm_analyses.agent_role_id` is recorded.
- Manual re-run (alert card, existing retrigger flow) gains a role dropdown
  populated from enabled roles; the choice is carried through the requeue.
  Automatic enrichment uses the default role.
- Admin UI: roles CRUD as a new section on `/admin/llm` (same privilege as
  llm configs), with a "set default" action that flips `is_default`
  transactionally (same unique-WHERE index trick as
  `llm_configs_enabled_idx`).

## 8. Admin UI for info sources

`/admin/assets` (admin privilege, same guard as `/admin/llm`):

- List of `assets_configs` with enabled toggle, base_url, schema, last cache
  stats (row count, newest fetched_at).
- New/Edit forms: name, base_url (trailing slash stripped like
  `Jira.apiUrl`/`Cmdb.apiUrl` do), token_env, auth_mode, default_schema_name,
  host_query_template, attribute display list.
- Connection test button: `listSchemas` against the config, result shown
  inline; failures logged via `Log.logWarn` with the reason (remember the
  `?context = ?context.frameworkConfig` shadow — `Request` has no `logger`).

## 9. Mock Assets server

`nix/mocks/mock_assets.py`, port 18085, python stdlib only, wired into the
devenv process list and `nix/checks.nix` like the existing mocks.

- Serves `/rest/assets/latest/...` for: `objectschema/list`,
  `objectschema/{id}/objecttypes/flat`, `aql/objects` (GET), `object/{id}`,
  `object/{id}/history`, `objectconnectedtickets/{id}/tickets`,
  `config/statustype/{id}`, `icon/{id}`.
- Reproduces the verified shapes faithfully: wrapped `objectschemas`,
  wrapped `objectEntries`, bare arrays everywhere else, Int ids, shape-A 404
  with a Russian `NotFoundInsightException:` message for unknown ids, 302 to
  an HTML page for unknown routes and missing auth.
- AQL evaluator: minimal subset — `objectSchema = "..."`, attribute
  `like`/`=`, `AND`, matching what the config templates generate; anything
  else → shape-A 400.
- Seeded dataset: one schema ("Capacity CMDB" / key `CHCMDB`), object types
  Host/Database/Cluster, objects for the dev fixtures (`dev-host-01` with
  owner/cluster/DC/IP attributes — the integration suite's known subject) so
  smoke matches alerts fired by the existing scenarios.
- Unauthenticated debug backdoors mirroring the other mocks:
  `POST /debug/reset`, `POST /debug/fail/{500}` for the soft-fail path.
- Env wiring: `HALEMANS_ASSETS_URL` + `ASSETS_TOKEN` in
  `.devenv/state/halemans/env.sh` (extend `ensureTokens`); smoke seeds an
  `assets_configs` row pointing at the mock — added in BOTH seed-halemans and
  smoke-check.sh's inline seeding.

## 10. Testing

- Unit (`Test/Main.hs`): envelope decoder (bare vs every wrapped form),
  AQL escaper (quotes, backslashes, Unicode/Cyrillic), error-shape
  classification fixtures from assets-api.md §3, pagination helper
  (toIndex < totalFilterCount), role→tools filtering, prompt assets excerpt
  truncation ordering.
- Integration (`Test/Integration.hs`): enrich flow end-to-end against the
  mock (alert for `dev-host-01` → cache row + link + card data); negative
  caching; soft-fail with `/debug/fail/500`; agent-role selection changes the
  rendered prompt and tool set; `assets_lookup` tool call against the mock.
- Smoke (`tests/smoke/run.sh` + `nix/scripts/smoke-check.sh`): assets
  scenario — fire a grafana/zabbix alert for a mock-known host, assert the
  card panel fields and that the LLM analysis text reflects owner/cluster;
  Playwright covers the `/admin/assets` CRUD and the role dropdown.
- Canonical gate: `nix flake check --impure`.

## 11. Rollout / ops notes

- Version bump per semver (feature → minor) in `Halemans.cabal`.
- New env vars are optional; without `HALEMANS_ASSETS_URL`/`ASSETS_TOKEN` the
  assets subsystem is simply absent (no configs → no enrich step), dev stack
  unaffected beyond the new mock process.
- Cache growth: `assets_objects` rows are small and only refreshed on enrich;
  retention (Phase 5 job) can pick them up later — no purge logic in this
  milestone.
- Live-instance verification is out of band: the read-only script
  `design_docs/examples/jira_assets_api_readonly_test.sh` (45 checks) stays
  the manual gate against `jira.officesvc.bz`; the app's smoke suite only
  ever talks to the mock.
