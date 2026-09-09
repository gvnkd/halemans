# Halemans project memories

## Versioning
- First release: v1.0.0. From now on, ANY code change bumps the version per semver (patch: fixes/internal, minor: features/compatible, major: breaking). Version lives in Halemans.cabal (`version:` field); releases are git tags `vX.Y.Z` pushed to both remotes (origin=gitea, github).

## IHP sources
- Local IHP checkout: `/home/pion/work/dev/ihp` — read it directly for framework internals (IHP.ModelSupport, IHP.Job.*, IHP.HSX, LoginSupport, etc). Do NOT grep /nix/store for IHP sources.

## What this is
IHP (Haskell) app aggregating alerts from Zabbix/Grafana/Alertmanager. Design: design_docs/01_highlevel.md; milestone 0 (dev/test env) done — see design_docs/milestone_0.md §10 for implementation notes. Milestone 2 (correlation & teams) done — see design_docs/milestone_2.md §14. Milestone 3 (context: CMDB/Jira/write-back/dashboards/themes) done — see design_docs/milestone_3.md. Milestone 9 (resolved facets + facet dashboards) done — see design_docs/milestone_9.md §11.

## Milestone 3 notes
- Mocks: mock-confluence :18082, mock-jira :18083 (python stdlib, nix/mocks/). Tokens CONFLUENCE_TOKEN/JIRA_TOKEN + HALEMANS_CONFLUENCE_URL/HALEMANS_JIRA_URL in env.sh (ensureTokens). Mock jira has unauthenticated test backdoor POST /debug/issue/{key}/status.
- Write-back retry timing overridable: HALEMANS_WRITEBACK_BACKOFF_SECONDS="0,0,0" + HALEMANS_WRITEBACK_MAX_ATTEMPTS (smoke sets them fast).
- EnrichAlertJob/WriteBackJob are one-shot event jobs; JiraSyncJob self-reschedules (5min) — in tests INSERT a fresh jira_sync_jobs row to trigger promptly. EnqueuePollers seeds JiraSyncJob.
- Real Atlassian servers vs mocks: Jira Server/DC has only REST v2 (`/rest/api/3` 302s to login.jsp) → `HALEMANS_JIRA_API_VERSION` (default "3", mock speaks v3) feeds `JiraConfig.apiVersion` via `Jira.apiUrl`. Trailing slashes in HALEMANS_*_URL must be stripped (`Text.dropWhileEnd (== '/')` in `Jira.apiUrl`/`Cmdb.apiUrl`) or the path join yields `//rest/...` → Confluence 404. Same applies to LLM_ENDPOINT (`Llm.apiUrl`). Connection tests (`connectionOk`) return `Either Text ()` and the controllers log the reason via Log.logWarn (Request has NO `logger` field — shadow `?context = ?context.frameworkConfig` first).
- Alert card panels (cmdb/jira/writeback chip) live-update via WS kinds "enriched"/"writeback" (fragments in Web/View/Fragments.hs, contextPanelUpdates in Live.hs). WS supports multi-scope: data-live-scope="env:a,env:b".

## Milestone 4 notes
- Mock LLM: mock-llm :18084 (nix/mocks/mock_llm.py, no auth). LLM_ENDPOINT/LLM_MODEL in env.sh (ensureTokens). Unauthenticated backdoors: POST /debug/fail/{429,500,malformed} (body {"times":N}), POST /debug/reset. Deterministic completions: "disk" in last user msg → disk analysis, else generic; fenced ```json block convention.

## Milestone 5 notes (hardening)
- wreq THROWS on connection failure (dead port) — connector `Either Text` only covers HTTP/parse errors. Pollers wrap calls in `try SomeException` before SourceHealth.recordFailure.
- Source-health: fingerprint `halemans:source-health:<source_id>`, warning → high at 5 failures (severity_upgraded event); recordSuccess posts Resolved + resets backoff. Backoff: `interval × 2^failures` cap 30min, deterministic ±10% jitter from fingerprint hash (no random dep). Pollers skip sources with next_poll_at > now.
- Webhook silence: SourceHealthJob (30s self-reschedule) checks sources with config.expectedIntervalSeconds; baseline = max(raw_events.received_at) or sources.created_at.
- Enrichment re-analysis marker: llm_analyses.error_message = 'enrichment_retrigger' caps at one per alert; dedupe legitimately suppresses when context didn't change the prompt hash. Integration test needs host dev-host-01 + check "halemans test trigger" (only subject the mocks carry context for).
- Audit export: GET /admin/audit/export (admin privilege), CSV/JSONL, scope ≤1 of environment/alert + time range (default 7d); audit_exports row inserted before streaming, row_count updated after. Job metrics page: /admin (JobMetrics.hs reads the 11 job tables; statuses job_status_*).
- Session cookie name is SESSION (wai-session). WS clients must send {"type":"env","name":...} subscribe frames (masked) before receiving broadcasts.
- typedSql: `${utctime}` params work (inferred from column/cast); `::timestamptz` cast on a Text param makes it infer UTCTime — pass UTCTime. `${uuid}` from Data.UUID. Multi-col results = labeled SqlRow, single-col = bare value (no `get #col`).
- Perf harness: nix/scripts/perf-harness.sh (manual gate, not a flake check). psql generate_series seeding (deviation from design's Haskell seeder — ORM seeding too slow). 10k alerts: dashboard p95 ~6ms, WS fan-out ~5ms. Composite alert_events(alert_id,created_at) was NOT picked by the planner (alert_events_alert_id_idx suffices at 10k) → dropped; final index list: alerts(environment_id,status), alerts(fingerprint) WHERE status<>'closed'.
- Playwright/smoke INSERT...RETURNING: grep-extract uuid (see inserted_id helper); M5 smoke cleans up its probe source with `UPDATE enabled=false` (alerts FK blocks DELETE).

## Milestone 4 notes (LLM enrichment)
- Never name a column `error`: the generated `LlmAnalysis.error` field clashes with Prelude.error in every module importing Generated.Types (phase-4 uses `error_message`).
- mock_llm.py STRICTLY validates chat-completion requests (OpenAI contract): explicit nulls, unknown top-level/message fields, bad tool/tool_calls shapes → 400 invalid_request_error; assistant content may be null only with tool_calls. Validation runs AFTER /debug/fail backdoors. Purpose: real servers (llama.cpp) 400 on `"tools": null` — the lenient mock hid that client bug. App payload builder is `chatCompletionPayload` (omits nulls); new request fields need the mock's KNOWN_TOP_LEVEL whitelist extended.
- LLM provider CRUD lives on /admin/llm (v1.9.0): llm_configs UI, enable flips others off transactionally (llm_configs_enabled_idx unique-WHERE). Dev-stack process-compose bakes mock_llm.py from the store — to run the working-tree mock: `process-compose process stop mock-llm` + `python3 nix/mocks/mock_llm.py 18084`.
- LlmAnalysisJob retries count `llm_analysis_jobs` ROWS per analysis (fresh requeued rows reset attempts_count — EnrichAlert's attemptsCount guard has the same unbounded-retry bug shape). Backoff via HALEMANS_LLM_BACKOFF_SECONDS="0,0,0" in checks; queuePollInterval 10s (default 60s makes 4-cycle retry tests time out).
- Prompt-hash dedupe runs in the JOB (not at ingest): hash needs the rendered prompt. Dedupe only ever hits for same-alert re-analysis — AlertEvent timestamps make cross-alert prompts unique by construction.
- Generic-hook alerts get fingerprint prefix `grafana:` (quirk, not `generic:`) → PollGrafana's absence-reconcile can resolve them mid-scenario: smoke/Playwright must NOT assert dedupe copies (integration suite covers dedupe deterministically with testSource alerts).
- LLM structured output = fenced ```json, parsed best-effort in Application/Service/Llm/Output.hs; card renders markdown as escaped <pre> (no markdown lib in deps).
- Record field selectors must be IMPORTED to get GHC's magic HasField (`import M (ParsedOutput (..))`), or cross-module `parsed.markdown` fails with Could-not-deduce-HasField.
- typedSql `${...}` interpolation can't lex `get #field rec` — bind to a local first. BIGINT columns need Int64 params. sqlExecTyped returns IO Int64 (rows affected), void it.
- wreq: disable status-check exceptions with `checkResponse .~ Just (\_ _ -> pure ())`, read code via `statusCode (resp ^. Wreq.responseStatus)` (http-types statusCode is a function, not a lens). No connect-timeout knob in this http-client version; only managerResponseTimeout.
- wreq/http-client only auto-follow 301/302 for GET/HEAD and STRIP the Authorization header on cross-host redirects → reverse proxies that 302 (http→https, short name→FQDN) break every connector. All connectors route through Application/Service/Http (getFollowing/postFollowing/deleteFollowing): manual 301/302/303/307/308 following, max 5 hops, full opts (headers/body) re-sent per hop, relative Location resolved via network-uri. Final non-2xx throws `HttpStatusError url code` UNLESS the caller set a custom checkResponse (LLM reads 429/500 bodies — those get the raw response).
- ghcWithPackages exposes ONLY packages listed in flake.nix haskellPackages (transitive deps get stub confs: exposed-modules listed but invisible to find-module). Test-only deps (warp for Test/HttpSpec) must be listed too. The devenv shell's ghc env does NOT rebuild on haskellPackages edits without a full stack restart; `nix build .#checks.x86_64-linux.tests` picks them up immediately.
- warp's module is `Network.Wai.Handler.Warp` (there is no `Network.Warp`).
- cache.digitallyinduced.com 500s on a narinfo block substitution entirely; the path is often already local → re-run `nix build ... --offline`.
- Pure alertmanager sources have NO reverse silence reconcile (no poller exists for them); only grafana (PollGrafana) and zabbix (PollZabbix) mirror source acks.
- users.settings.theme (validated against Application.Helper.Theme themes) drives `<html data-theme>`; app.js halemansApplyTheme persists via POST /profile/theme.

## Test-suite pitfalls (hard-won in M3)
- psql -c "INSERT ... RETURNING id" ALSO prints the command tag ("INSERT 0 1") on stdout — grep-extract the uuid, never capture raw.
- Turbolinks (turbolinksMorphdom) replaces body WITHOUT pushState after form POSTs → Playwright `wait_for_url` never fires; wait on element testids instead. Real `page.goto` navigations DO fire URL changes (server 302s land).
- Playwright: `context.new_page()` shares the context cookie jar — logging in a throwaway user overwrites the main page's session; re-login main page (or delete the throwaway user AND re-login) afterwards.
- IHP dev server serves its LAST compile's error page forever; `touch` a source file to force recompile. Stale typedSql introspection postmasters linger in /tmp/ihp-typed-sql--* — kill + rm when schema errors look wrong.
- Dev DB rebuild (when the dev DB is stale/partially migrated): terminate backends, DROP+CREATE DATABASE halemans, apply ihp-schema + Schema.sql + Fixtures, then `INSERT INTO schema_migrations` all revision numbers (table shape: `revision BIGINT NOT NULL UNIQUE`), rm seed.done, `process-compose process restart seed` (one process at a time; the multi-arg form prints usage).


## Milestone 6 notes (public API + metrics)
- Milestone 6 done: read-only JSON API (`/api/v1/alerts`, `/api/v1/alerts/:id`, `/api/v1/environments`), `/metrics` Prometheus exporter, `api_tokens` table, profile UI token mgmt + admin revoke-any (on `/admin`), per-token in-memory rate limit (Application/Service/Api/*).
- typedSql: `LIMIT (${n} + 1)` infers Int (NOT Int64); UNION ALL string-literal columns (`'x' AS job`) and `status::text` decode as **Maybe** Text/Int64 — `fromMaybe` them. `coalesce(...)` counts as NOT NULL.
- Token hash = sha256 hex via cryptonite `convertToBase Base16` (same as Llm/Prompt.hs); prefix = first 8 chars of plaintext. resolveToken touches last_used_at at most once/min.
- Rate limits: 120/min API, 6/min /metrics; env overrides HALEMANS_API_RATE_LIMIT / HALEMANS_METRICS_RATE_LIMIT (read per request, tests/smoke rely on defaults).
- Smoke token: checks.smoke exports HALEMANS_API_TOKEN="halemans-smoke-api-token-v1" (fixed test secret, sre@dev, both scopes); dev seed generates a random one into `.devenv/state/halemans/api-token` (name demo-cli). run.sh falls back to the state file.
- Dev stack live DB socket is `/run/user/1000/devenv-*/postgres` — the stale direnv `/tmp/devenv-*` path fails; the running app's DATABASE_URL is in /proc/<RunDevServer>/environ.
- Integration tests against the DEV db race the dev worker (it writes LLM analyses/events onto test alerts) — use future created_at (2999) and order-tolerant assertions; sandboxed check has no worker so it's deterministic there.
- Run subsets: `ghci -v0 Test/Main.hs -e 'main'` / `ghci -v0 Test/Integration.hs -e 'System.Environment.withArgs ["--match","milestone 6"] main'` with DATABASE_URL exported.
- Dev-DB smoke runs can fail pre-existing scenarios (write-back chip, audit csv header) from polluted dev state — only the sandboxed `nix flake check --impure` is authoritative.

## Milestone 7 notes (provisioning)
- Generated `LlmConfig` (llm_configs table) clashed with the M4 service record → service record renamed to `LlmProviderConfig`; DB-first resolution (`llmConfigFromDb`/`currentLlmConfig`) lives in `Application/Service/Llm/DbConfig.hs` — a separate module because both records share field names (providerName/endpoint/model/toolsEnabled) and local selectors shadow imported ones. Callers that use `.endpoint` etc must import `LlmProviderConfig (..)`.
- Config.hs provisioning hook: ConfigBuilder is evaluated before `ihpDefaultConfig`, so `DatabaseUrl` isn't in the tmap — call `defaultDatabaseUrl` + `withModelContext <url> noopLogger` inside `configIO`. Covers web + worker + dev server + run-script (all eval Config.hs). ghci `-e` exits 0 even when the evaluated expression throws — always capture stderr.
- typedSql: `${idParam}` of type `Id' "t"` needs the `Id` constructor in scope (`import IHP.ModelSupport (Id' (..))`) or you get a `GHC.Prim.coerce` not-in-scope error. pg void functions (pg_advisory_xact_lock): `SELECT 1 WHERE pg_advisory_xact_lock(hashtextextended(${cat}, 0)) IS NULL` (same IS NULL trick as pg_notify).
- Provision module `Application/Service/Provision.hs`: `applyProvisionConfig :: (?modelContext) => FilePath -> IO ()`, strict aeson parsers reject unknown keys; per-category advisory-locked transaction; strict delete FK violations are caught per row and rethrown as `ProvisionError` naming entity + blocking constraint. Strict-users/teams/sources integration tests build keep-lists from live DB rows (order-independent, dev-DB safe).
- v1.17.0: provision categories `fieldMappings` (upsert on UNIQUE(facet,rank); kind validated, field-kind keys checked against parseAlertField) and `dashboards` (upsert by userEmail+name, config validated via decodeDashboardConfig, isDefault flips the user's other dashboards; strict deletes are GLOBAL — keep-lists must replay live rows incl. config or they get clobbered). Examples in deploy/docker/provision.example.json.
- sources.name is now unique (migration 1788686328, also inline in Schema.sql).
- Smoke provision scenario (run.sh, last) is gated on `SMOKE_APP_MANAGED=1` — only smoke-check.sh sets it (exports SMOKE_APP_PID/SMOKE_APP_LOG; run.sh `restart_app` kill/relaunches `$RUN_PROD_SERVER` with HALEMANS_PROVISION_CONFIG). Dev-stack smoke skips it (process-compose owns the app there). Strict-teams pass keeps `sre` with its seeded members.
- halemans-gen-password (nix/lib.nix genPassword, script nix/scripts/gen-password.sh) prints `password:`/`passwordHash:` lines + a users.items JSON fragment; pwgen + genPassword are in devenv packages and checks.smoke nativeBuildInputs.

## Docker deployment (deploy/docker/)
- End-user flow needs NO nix: image carries RunProdServer/RunJobs/EnqueuePollers/GenPassword/psql/busybox-sh + schema bundle at /share/db-init (dockerTools links contents' bin/* into /bin automatically — no manual symlinks). `/bin/db-init` (deploy/docker/db-init.sh, baked via extraCommands) applies 00-ihp-schema/01-app-schema/02-roles/99-schema-migrations guarded on to_regclass('alerts'). roles.sql lives at deploy/docker/roles.sql (keep in sync with seed-halemans.sh + smoke-check.sh role privileges).
- Upgrades auto-migrate: `/bin/db-migrate` (deploy/docker/db-migrate.sh) applies pending /share/db-migrate/*.sql (baked from Application/Migration), records revisions in schema_migrations, pg_advisory_lock(hashtext('halemans-db-migrate')) + in-lock re-check make concurrent app/worker boots safe; compose app/worker/enqueue-pollers prefix `db-migrate && exec ...`. No-op on fresh DBs (db-init records all revisions) and on uninitialized DBs (defers to db-init). Gotchas: the dockerTools image has NO /tmp (pipe generated SQL to psql stdin, never mktemp); busybox psql \if needs a `\gset` boolean var.
- GenPassword = Application/Script/GenPassword.hs → `script-GenPassword` package (IHP flake module auto-packages Application/Script/*.hs, EXCLUDING Prelude.hs). Runs without a DB (hasql-pool acquires lazily; Config.hs provision hook skips when env unset). Script getArgs returns [Text] (CorePrelude), not [String].
- CI: .github/workflows/docker-images.yaml builds .#docker-image via nix (needs the devenv-root override dance: mkdir .devenv + printf $PWD > .devenv/root, --impure --override-input) and pushes ghcr.io/<repo>:{latest,sha,v*}.
- compose: deploy/docker/docker-compose.yaml, env via .env.example→.env (env_file passthrough for optional LLM_*/ZABBIX_TOKEN/GRAFANA_TOKEN; required POSTGRES_PASSWORD/IHP_SESSION_SECRET/HALEMANS_GENERIC_HOOK_TOKEN). Host port HALEMANS_PORT (8000 clashes with local Taiga). Tested live on this machine: docker daemon IS reachable interactively as pion (rootless restriction is nix-sandbox-only). Local image test loop: nix build .#docker-image (override dance) + docker load + scratch postgres:16-alpine container.

## Milestone 9 notes (resolved facets + facet dashboards, v1.14.0)
- Full notes in design_docs/milestone_9.md §11. Schema: alerts.facets jsonb + GIN (jsonb_path_ops), field_mappings (UNIQUE facet+rank), facet_backfill_jobs (cursor-chained, chunk 500). Migration 1788888987.
- Facet refs: `field:` = raw column, `label:` = alerts.labels, `attr:` = materialized alerts.facets (override chain). Card match ops =/!=/~/in; `~` → LIKE via globToLike (equivalent to globMatch). `!=` never matches absent values.
- facetValue lives in Application.Pipeline.Grouping (pure); Service.Facets re-exports it. DashboardCard v2 AST + legacy {env,filters} round-trip via cardLegacy flag + cardExtras (unknown keys preserved).
- Dashboard Show page WS scope is `dash:<uuid>` (ScopeUserDashboard): broadcaster re-fetches the alert, evaluates matchCardAlert per card, re-renders whole sections on ANY alert event kind. Legacy card DOM ids dashboard-card-<env> kept (Playwright depends), v2 = dashboard-card-<index>, groups ...-group-<value>.
- regroupAlert (Service/Groups) was built fresh — no replay machinery existed. Only runs when a facet-referencing rule exists; never ungroups.
- Dev-DB test tolerance: ensureMapping helper reuses seeded mapping rows; regroup test disables the seeded catch-all env+host rule temporarily.

## Milestone 8 notes (enrichment phase 0: Jira Assets + agent roles)
- Mock Assets: mock-assets :18085 (nix/mocks/mock_assets.py), ASSETS_TOKEN + HALEMANS_ASSETS_URL in env.sh (ensureTokens). Backdoors: POST /debug/reset, POST /debug/fail/500 {"times":N}. Icons + /login.jsp return 200 (browser <img> tags hit them — a 302 there redirect-loops). AQL evaluator: objectSchema=/attr like|=/AND only.
- Assets client (Application/Service/Assets.hs) uses wreq DIRECTLY with redirects=0 — NOT Application.Service.Http (Assets 302s unauth/unknown paths to a login page; following hides auth failures). BIGINT columns generate as Integer, not Int64 (assets_objects.object_id); column `label` generates as `label_` (codegen rename).
- assets_objects.attributes is a jsonb OBJECT (Aeson.object from flattened pairs) — Aeson.toJSON on [(k,v)] encodes an array of pairs and silently breaks the Attrs.objectAttributes parser.
- Cache/link: Application/Service/Assets/Cache.hs; negative cache = asset_alert_links row with NULL assets_object_id, matched_by "miss:<config_id>:<host>", 30min TTL, cross-alert. Refresh button (RefreshAssetsAction, view privilege) bypasses the TTL.
- WS kind "assets" is panel-only: Live.updatesFor ScopeAlert skips status badge + timeline for it (a second notify from EnrichAlert would double-prepend the timeline).
- llmPanelHtml shows the newest TERMINAL (done/failed) analysis; a pending newer row shows llm-pending-note — unless the pending row's JOB failed (jobErrors), which wins. Both Playwright checks depend on this exact precedence.
- Agent roles: llm_analyses.agent_role_id (nullable) + llm_agent_roles; resolveAgentRole = row id → is_default → legacy. tools jsonb [] means NO tools (role filters toolDefinitions by name); no role + toolsEnabled = full set. Seeds create default-enricher (full tool list) in BOTH seed-halemans and smoke-check.sh.
- alert_enrichment template is now v2 ({{assets_excerpt}} slot); seed deactivates only versions <2 so admin-created newer versions survive re-seeds. Playwright llm template checks assume v2/v3 numbering.
- mock_llm echoes the prompt's "## Linked assets" first line BEFORE the fenced json — parseCompletionOutput drops prose after the fence, so trailing echoes never reach result_md.
- m7 provision tests leave an ENABLED llm_configs row (m7-db.example) behind: any later spec that calls the LLM must `UPDATE llm_configs SET enabled = false` first (DB-first resolution beats LLM_ENDPOINT env).

## Milestone 8 addendum (icon cache, v1.15.x)
- Asset icons are served from the app: GET /assets/objects/{id}/icon (AssetsIconsController, view privilege) lazy-fills assets_icon_cache (config_id+url unique, bytea body) via Assets.fetchBinary; Fragments renders <img> at that route, never the Jira origin. absoluteIconUrl lives in Application/Service/Assets/Icons.hs.
- mock-assets serves distinct 16x16 colored PNGs per icon id (1x1 transparent made cards look icon-less).
- FIXED (1.16.1, cherry-picked from fix/assets-icon-406-accept-header) icons 404 against a real Jira server: the shared client opts pinned `Accept: application/json` on ALL requests; fetchBinary (icon/avatar images on the Jira origin) inherited it, and Jira's icon routes answer **406 Not Acceptable** to it → every icon fetch silently failed (Left → Nothing → controller 404) while the Assets JSON API worked fine. assets_icon_cache sat at 0 rows. Verified 406-vs-200 with the same icon URL + token via curl (Accept application/json → 406, Accept image/* → 200 image/png). The sandboxed smoke masked it: the mock Assets server does no content negotiation. Fix: `opts` takes the Accept header — JSON endpoints keep application/json, fetchBinary sends `image/*, */*`. The AssetsSpec warp test server now 406s on `Accept: application/json` at the icon route (emulating Jira) and a new spec asserts the fetchBinary Accept header.
- Unresolved smoke flake (observed 2026-09-09 on this machine, twice): check "llm: analysis panel fills via websocket without reload" fails with Playwright strict-mode — `llm-markdown` resolves to 2 elements (one nested in `llm-history`): a fresh probe alert already had 2 done analyses, i.e. EnrichAlert's `maybeRetriggerAnalysis` retrigger landed (LlmAnalysisJob#1 completed before the EnrichAlertJob run → different prompt hash → second analysis). Suspected pre-existing race, unrelated to 1.15.3 (sandbox mock ignores Accept); triage separately.
- FIXED (1.15.2) smoke flake: Playwright "source health: dashboard count never moved". Cause: leftover firing generic-hook alerts (grafana:pw-*) in dev get absence-resolved by PollGrafana mid-check, returning the count to its baseline text. Fix: the check closes all open alerts + reloads for a hermetic baseline before inserting the probe source.

## Commands
- Poll loops (PollZabbix/PollGrafana) STOP rescheduling when no enabled sources of their type exist; re-arm via `ensurePollerForSourceType` (Application/Service/PollerControl.hs) — called from SourcesController create/update/toggle + Provision.upsertSource. Sources inserted via raw SQL (seeds) need EnqueuePollers afterwards (seed-halemans.sh runs it last).
- App logging: HALEMANS_LOG_LEVEL=debug|info|warn|error (default info, validated at boot in Config.hs, read per call in Application/Service/Log.hs); HALEMANS_ACCESS_LOG=0 disables wai request logging (RequestLoggerMiddleware override — `option` is first-wins vs ihpDefaultConfig).
- Full stack: `nix develop .#default --impure -c devenv-flake-up -D` (detached). Attach: `process-compose -u /run/user/1000/devenv-*/pc.sock process list`.
- **Do NOT use the `devenv` CLI wrapper from a direnv-loaded shell** — it reuses the stale in-shell `devenv-flake-up` after nix/*.nix changes. Use the `nix develop` form above (or reload direnv).
- Smoke suite (needs running stack): `nix develop .#default --impure -c smoke-test`.
- Canonical check: `nix flake check --impure` (includes checks.smoke: full 3-source suite, sandboxed, no docker needed).
- Seed manually: `nix develop .#default --impure -c seed` (idempotent). `stack-status` for health report.
- Plain `nix eval`/`nix build` on this flake needs `--override-input devenv-root "file+file://$PWD/.devenv/root"` (create it: `mkdir -p .devenv && printf %s "$PWD" > .devenv/root`).

## Ports (this machine has squatters — check before changing)
- Halemans app **28080** (toolserver 28081 = PORT+1). NOT 8000 — that's the local Taiga (never touch, runs as user taiga). 8080/8081/8888/18080 also taken.
- grafana 3001 (admin/admin), alertmanager 9093, zabbix web/API 10080, zabbix server 10051, agent 10050.
- Tokens live in `.devenv/state/` (gitignored): halemans/{am-hook-token,generic-hook-token,env.sh,seed.done}, zabbix/token, grafana/token.

## Nix/stack pitfalls (hard-won)
- devenv 2.0's tasks wrapper never marks one-shot processes completed → `depends_on condition=process_completed_successfully` never fires. We gate web/worker on the marker file `.devenv/state/halemans/seed.done` instead.
- process-compose holds configs baked at server start: after changing nix/*.nix, `down` + full `devenv-flake-up` (process restart is NOT enough). Orphaned RunDevServer/ghc children can survive `down` and hold ports — `pkill -f RunDevServer/RunDevWorker` and check `ss -ltnp`.
- writeShellApplication runs shellcheck and fails on info-level findings (SC1091 source, SC2034 unused loop var) — add disable directives.
- Nix sandbox: loopback works (checks.smoke boots everything in-sandbox); docker does NOT (socket root:docker, nixbld excluded); `__noChroot` requires sandbox relaxed AND the worktree is unreadable for nixbld users anyway (/home/pion is 700). Podman doesn't help either — don't retry that.
- nix/scripts/*.sh are plain files (readFile) — no `''${}` nix escaping there; `''` leaks break shellcheck.

## Zabbix (native zabbix70, no docker)
- API is served by the PHP frontend (`php -S` on 10080), NOT by zabbix_server (10051 trapper).
- DB = `zabbix` database in the devenv postgres, imported by `halemans-zabbix-db-init`; DB user = OS user (no `postgres` role exists in devenv postgres) — rendered dynamically into configs.
- Zabbix 7.0 `token.create` returns only ids; secret comes from `token.generate`.
- `history.push` 200s with `data[].error` until the server config cache learns new items (CacheUpdateFrequency=5 in our template); fire-test-alert-zabbix retries until `data | all(has("itemid"))`.
- Fresh import creates default "Zabbix server" host with health/OS templates → real host alerts (disk space!) pollute ingestion; seed-zabbix disables it.
- Zabbix trigger events: problem event and OK event have different eventids → Halemans fingerprint is trigger-scoped: `zabbix:trigger:<id>`.
- First zabbix sync is bounded to `initialHistoryDays` back from now (source config key, default 1 day) via `initialCursor` in Application/Job/PollZabbix.hs; once `last_sync_cursor` exists the key is ignored. Sources UI has a numeric field (empty = default).
- Host-group filtering: `teams.host_groups` jsonb (names, Teams UI comma-separated); source config `hostGroupScope` = "all" (default, key omitted) | "teams" → PollZabbix resolves team group names against the `zabbix_host_groups` cache table (never the API — groups are near-static) and passes `groupids` to event.get; empty match = skip cycle (fetch nothing, not everything). Cache is populated via POST /sources/:id/sync-host-groups (button on Sources index, zabbix sources only, from `hostGroupsGetAll`) OR at boot via provisioning `hostGroupsFile` on a zabbix source item (local JSON: bare `[{"groupid","name"}]` array or full hostgroup.get dump with `result` wrapper; replaces the cache; for tokens without hostgroup.get permission). Both go through `replaceHostGroupCache` in Application/Service/HostGroups.hs. Team form offers the cached names as a filterable multi-select (`hostGroupPicker` in Web/View/Teams/New.hs, submitted via paramList @Text "hostGroups"); empty cache renders sync instructions instead. Members list shows only members; filter field reveals non-members to add, × button removes (role reset to ""); JS in teamPickersScript (inline, like Blackouts; DOMContentLoaded + turbolinks:load + immediate, dataset.init-guarded). Helpers in Application/Service/HostGroups.hs.

## Grafana
- Alert rule `dev-cpu-sim`: API-provisioned by seed-grafana (upsert; restores canonical non-firing threshold +1e9). fire-test-alert-grafana flips to -1e9/+1e9 (random_walk is unbounded → deterministic).
- Rule data chain: A=testdata random_walk → B=reduce last → C=`threshold` expression. classic_conditions CANNOT take expression inputs (only raw datasource queries) — use `threshold`.
- Rule-group interval PUT needs `{"title","interval"}` and interval must be a multiple of the scheduler tick (default 10s — evaluation_interval didn't lower it; we use 10s).
- Grafana 12: `[alerting] enabled` is rejected (legacy removed) — only `[unified_alerting]`.
- Contact point/policy file-provisioned; `$HALEMANS_GENERIC_HOOK_TOKEN` expanded by grafana from process env (provisioning env expansion works).
- `[unified_alerting]` ini, grafana.ini paths use `$__env{DEVENV_STATE}`.
- **Embedded alertmanager drops resolved alerts from `/api/alertmanager/grafana/api/v2/alerts` within seconds** (active=false filter returns nothing useful). PollGrafanaJob therefore reconciles resolves by ABSENCE (60s grace on last_seen_at) and has a 90s refire guard so listing lag can't refire an alert the webhook just resolved. Firing alerts carry endsAt ~2min in the FUTURE (moving resolve deadline) — never use `endsAt <= now` alone.
- Poller/webhook titles must match or smoke's title assertions break: severity/summary live in ANNOTATIONS on the AM listing, labels on the webhook path — amAlertToNormalized checks annotations first.

## IHP/Haskell specifics
- Schema.sql parser: NO inline `--` comments inside CREATE TABLE; NO column-level `REFERENCES` — FKs only as top-level `ALTER TABLE ... ADD CONSTRAINT ... FOREIGN KEY` (else the column generates as plain UUID, not `Id' "..."`).
- **Schema codegen ignores `ALTER TABLE ADD COLUMN`** (v1.6): new columns must go inline into `CREATE TABLE` in Schema.sql; ALTERs live only in the migration file. Regenerate: `rm -rf build/Generated && build-generated-code`.
- Column `type` → field `#type_`. Reserved words in HSX attribute expressions break HSX's parser — bind via `let` first.
- Regenerate model types: `make -f $IHP_LIB/lib/IHP/Makefile.dist build/Generated/Types.hs` (in nix develop shell).
- IHP.Prelude lacks: fetch/fetchOneOrNothing (IHP.Fetch), traverse/mapM for Either is mapM, `<&>`, posixSecondsToUTCTime (Data.Time.Clock.POSIX), aeson parseMaybe (Data.Aeson.Types), `.=` (import from Data.Aeson explicitly), `void` (Control.Monad), `read`, unsafePerformIO (System.IO.Unsafe).
- Ambiguous `record.id` with DuplicateRecordFields — use `get #id record`.
- typedSql v1.6: NO Maybe params (nullable-FK upserts → plain query-builder record API); multi-col results = labeled `SqlRow` records (`get #col_name`, FK cols decode as `Maybe (Id' "t")`); void-returning statements (pg_notify) unsupported — `SELECT 1 WHERE pg_notify(...) IS NULL` via sqlQueryTyped; compile-time introspection hits $DATABASE_URL, so checks.integration-tests is overridden in nix/checks.nix to pre-load the schema before runghc.
- typedSql `RETURNING id` decodes to `Id' "table"` directly (no UUID wrap).
- HSX: no inline `case` or `<>` fragments inside [hsx| |] — extract helper functions returning Html; attribute values need concrete Text (annotate `:: Text` on `cs`/`show`); fragment renderers shared between views and the WS broadcaster live in Web/View/Fragments.hs (render to Text via IHP.HSX.Markup.renderMarkupText, needs ?request/?context).
- Self-rescheduling jobs: override `queuePollInterval` (default 60s!) for sub-minute loops.
- Worker dev (RunDevWorker) recompiles via GHCi each boot (minutes) — smoke waits for a succeeded poller job before firing.
- typedSql needs ihp-typed-sql in haskellPackages; compile-time DB is auto (IHP_TYPED_SQL_AUTO_DB=1 in shell; prod builds auto-detect via buildWithPostgres).
- RunProdServer: no Config/ dir in cwd → session key autogenerated. prod bins: unoptimized-prod-server/bin/{RunProdServer,RunJobs}.
- HLS in this repo is often stale/wrong; trust `nix flake check --impure` + dev-server compile output, not LSP diagnostics.
- Auth: IHP LoginSupport; Config.hs needs explicit `module Config (config) where` or run-script's wrapper breaks on leaked field names; `CurrentUserRecord`/`HasNewSessionUrl` instances live in Application.Helper.Controller (visible from both Config and Web). Seeded users hashed via nix/scripts/hash-password.py (pwstore-fast pbkdf1 replica; salt fed to the hash is the base64 TEXT).
- WebSocket: IHP WSApp + `webSocketAppWithCustomPath @LiveController "ws"`; broadcaster = ihp-pglistener on `halemans_events` + per-connection scope registry (Application/Service/Live.hs). Scopes: dashboard/alerts/env:<name>/alert:<id>/group:<id>; group mutations publish kind="group" payloads with groupId (LiveEvent alertId/groupId both Maybe).
- IHP.Prelude's `head` returns Maybe (safe). `paramOrNothing`/`param` take ByteString field names — `cs` interpolated names. typedSql params into jsonb columns must be `Aeson.Value` (Text needs an explicit `::jsonb` cast AND still coerces as Value — pass Value). `fetch` lives in IHP.Fetch even for jobs.
- Generated record fields (e.g. AlertGroup.worstSeverity) silently clash with same-named local helpers in views — rename locals (see Web/View/Dashboard/Index.hs cardWorst).

## Alertmanager
- Must run with `--cluster.listen-address=""` (gossip binds 0.0.0.0:9094 otherwise → conflicts/sandbox failure).
- Config rendered at process start (sed @HALEMANS_AM_HOOK_TOKEN@) — templates in nix/lib.nix, shared with checks.smoke.

## Testing
- tests/smoke/run.sh: flags SMOKE_ZABBIX=0 for native-only subset. Fire scripts are retried/idempotent; suite matches alerts by fingerprint-prefix AND title. UI asserts log in as sre@dev via cookie jar (`login_as`); rbac scenario fires its own probe alert (all source scenarios end resolved).
- checks.smoke (nix/checks.nix + nix/scripts/smoke-check.sh): sandboxed, isolated $TMPDIR stack, prod binaries, full suite + Playwright (python3 playwright + playwright-driver.browsers; chromium needs FONTCONFIG_FILE=makeFontsConf[dejavu], --no-sandbox --disable-dev-shm-usage --no-zygote, writable HOME). Rebuilds when any tracked file changes (srcHash = self.outPath). **smoke-check.sh has its OWN inline seeding (does NOT call seed-halemans)** — new seeded rows/jobs must be added in BOTH places; worker needs `export GRAFANA_TOKEN` (only ZABBIX_TOKEN was exported pre-M2).
- checks.tests is overridden in nix/checks.nix too (same temp-postgres + schema pattern as integration-tests) because unit specs transitively import typedSql modules via connectors → Ingest.
- checks.tests (hspec unit, Test/Main.hs) and checks.integration-tests (Test/Integration.hs, overridden in nix/checks.nix) are auto-wired by the IHP flake module. Integration.hs applies the schema itself when missing (to_regclass guard) so `runghc Test/Integration.hs` also works against a dev DB.
- New files must be `git add -N`'d or the flake source won't see them.
- postgres under devenv dies on full disk (PANIC checkpoint) and crash-loops even after space frees: kill leftover postmaster, rm postmaster.pid + core.*, `process-compose process start postgres`.
- DB is named `halemans` (was IHP default `app`): the IHP flake module hardcodes `app` in env.DATABASE_URL/PGDATABASE/initialDatabases — flake.nix mkForce-overrides all three (replicating the IHPSchema+Schema+Fixtures schema bundle). ihp.appName only renames derivations.

## Milestone 9 addendum (attr CSV mapping + demo matrix, v1.16.0)
- attr-kind field mappings take the FIRST element of comma-separated Assets attribute values, stripped (`firstCsvElement`, Application/Service/Facets.hs); verbatim whitelisted-attribute copies keep the full string.
- field_mappings.key is UNPREFIXED, interpreted by kind (field = closed enum env/host/service/check/severity/status, label = alerts.labels key, attr = Assets attribute name). The `field:`/`label:`/`attr:` prefixed refs exist only in dashboard card JSON.
- Facets materialize onto alerts.facets only — the alert card always shows raw columns, so a successful recompute never changes the card's env. Recompute backfills non-closed alerts via facet_backfill_jobs (worker required); attr facets need linked assets (EnrichAlertJob ran).
- mock-assets matrix hosts for field-mapping demos: etcd-eu-01 (ETCD/EU/"PROD,TEST"), etcd-us-01 (ETCD/US/TEST), pg-eu-01 (POSTGRES/EU/PROD), pg-us-01 (POSTGRES/US/"TEST, PROD"); linking matches `Name like "{host}"`. Seeds (BOTH seed-halemans.sh + smoke-check.sh) add rank-50 attr mappings Environments/Service/Location + 'Service matrix' dashboard (8 cards, one per Service×Location×Environments combo).
- mock-assets reads ASSETS_TOKEN from its OWN process env per request — starting the working-tree mock without sourcing env.sh first gives 401 on everything.
- seed-halemans re-run can die on llm_agent_roles_default_idx (is_default unique) if a default role exists under a different name; seed crash-loop removes seed.done and the worker then never starts — touch .devenv/state/halemans/seed.done manually when the DB is already seeded.
