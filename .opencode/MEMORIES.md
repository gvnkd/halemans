# Halemans project memories

## Versioning
- ANY code change bumps the version per semver (patch: fixes/internal, minor: features/compatible, major: breaking). Version lives in Halemans.cabal (`version:` field) AND in Application/Version.hs (`appVersion` — runtime copy, cabal not readable in nix build; Test/VersionSpec fails the suite when they drift); releases are git tags `vX.Y.Z` pushed to both remotes (origin=gitea, github). When several version bumps accumulate in one uncommitted session, commit them together but tag ONLY the latest version.

## IHP sources
- Local IHP checkout: `/home/pion/work/dev/ihp` — read it directly for framework internals (IHP.ModelSupport, IHP.Job.*, IHP.HSX, LoginSupport, etc). Do NOT grep /nix/store for IHP sources.

## What this is
IHP (Haskell) app aggregating alerts from Zabbix/Grafana/Alertmanager/generic webhooks, with enrichment (Confluence CMDB, Jira, Jira Assets, LLM), facets and custom dashboards. Design: design_docs/01_highlevel.md; per-milestone implementation notes in design_docs/milestone_*.md.

## Branding
- Master artwork in images/ (lockups, glyphs, app icons, favicon PNGs). static/ carries the web assets: favicon.ico (multi-size 16/32/48/64), halemans-favicon-{16,32}.png, halemans-app-icon-180.png (apple-touch-icon); linked from Web/View/Layout.hs metaTags via assetPath. Regenerate the ico: `magick images/halemans-favicon-{16,32,48,64}.png static/favicon.ico` (imagemagick is in the dev shell).

## Integrations (mocks + real servers)
- Mocks (python stdlib, nix/mocks/): confluence :18082, jira :18083, llm :18084 (no auth), assets :18085. Tokens CONFLUENCE_TOKEN/JIRA_TOKEN/ASSETS_TOKEN + HALEMANS_*_URL in env.sh (ensureTokens).
- Backdoors: mock jira POST /debug/issue/{key}/status; mock llm POST /debug/fail/{429,500,malformed} {"times":N}, POST /debug/reset; mock assets POST /debug/reset, POST /debug/fail/500 {"times":N}. Mock assets icons + /login.jsp return 200 (browser <img> hits them).
- mock_llm STRICTLY validates chat-completion requests (OpenAI contract): explicit nulls, unknown fields, bad tool shapes → 400. Validation runs after /debug/fail. App payload builder is `chatCompletionPayload` (omits nulls); new request fields need the mock's KNOWN_TOP_LEVEL whitelist extended. Deterministic completions: "disk" in last user msg → disk analysis. Echoes the prompt's "## Linked assets" first line before the fenced json — parseCompletionOutput drops prose after the fence.
- mock-assets AQL evaluator: objectSchema=/attr like|=/AND only. Reads ASSETS_TOKEN from its OWN process env. Demo matrix hosts: etcd-eu-01, etcd-us-01, pg-eu-01, pg-us-01 (Service/Location/Environments combos; linking matches `Name like "{host}"`).
- Real Atlassian servers vs mocks: Jira Server/DC has only REST v2 (`/rest/api/3` 302s to login.jsp) → `HALEMANS_JIRA_API_VERSION` (default "3") feeds `JiraConfig.apiVersion` via `Jira.apiUrl`. Trailing slashes in HALEMANS_*_URL must be stripped (`Text.dropWhileEnd (== '/')` in `Jira.apiUrl`/`Cmdb.apiUrl`/`Llm.apiUrl`) or path joins yield `//rest/...` → 404.
- Real Jira answers 406 to icon/avatar fetches carrying `Accept: application/json`: Assets `fetchBinary` sends `Accept: image/*, */*`, JSON endpoints keep application/json. Dev-stack process-compose bakes mocks from the store — to run the working-tree mock: `process-compose process stop mock-llm` + `python3 nix/mocks/mock_llm.py 18084`.

## HTTP client (wreq)
- wreq THROWS on connection failure (dead port) — connectors' `Either Text` only covers HTTP/parse errors. Pollers wrap calls in `try SomeException` before SourceHealth.recordFailure.
- wreq/http-client only auto-follow 301/302 for GET/HEAD and STRIP the Authorization header on cross-host redirects → all connectors (except Assets) route through Application/Service/Http (getFollowing/postFollowing/deleteFollowing): manual 30x following, max 5 hops, full opts re-sent per hop, relative Location via network-uri. Final non-2xx throws `HttpStatusError url code` UNLESS the caller set a custom checkResponse (LLM reads 429/500 bodies).
- Assets client (Application/Service/Assets.hs) uses wreq DIRECTLY with redirects=0 — Assets 302s unauth/unknown paths to a login page; following hides auth failures.
- wreq: disable status-check exceptions with `checkResponse .~ Just (\_ _ -> pure ())`, read code via `statusCode (resp ^. Wreq.responseStatus)` (http-types statusCode is a function, not a lens). No connect-timeout knob in this http-client version; only managerResponseTimeout.

## Jobs
- Poll loops (PollZabbix/PollGrafana) STOP rescheduling when no enabled sources of their type exist; re-arm via `ensurePollerForSourceType` (Application/Service/PollerControl.hs) — called from SourcesController create/update/toggle + Provision.upsertSource. Sources inserted via raw SQL (seeds) need EnqueuePollers afterwards.
- EnrichAlertJob/WriteBackJob are one-shot event jobs; JiraSyncJob self-reschedules (5min) — in tests INSERT a fresh jira_sync_jobs row to trigger promptly. SourceHealthJob self-reschedules 30s. EnqueuePollers seeds JiraSyncJob.
- Self-rescheduling jobs: override `queuePollInterval` (default 60s) for sub-minute loops.
- Retry/backoff overrides: HALEMANS_WRITEBACK_BACKOFF_SECONDS="0,0,0" + HALEMANS_WRITEBACK_MAX_ATTEMPTS; HALEMANS_LLM_BACKOFF_SECONDS (used in checks).
- LlmAnalysisJob retries count `llm_analysis_jobs` ROWS per analysis (fresh requeued rows reset attempts_count).

## Source health & write-back
- Fingerprint `halemans:source-health:<source_id>`; warning → high at 5 failures (severity_upgraded event); recordSuccess posts Resolved + resets backoff. Backoff: `interval × 2^failures` cap 30min, deterministic ±10% jitter from fingerprint hash. Pollers skip sources with next_poll_at > now.
- Webhook silence: SourceHealthJob checks sources with config.expectedIntervalSeconds; baseline = max(raw_events.received_at) or sources.created_at.
- Pure alertmanager sources have NO reverse silence reconcile (no poller); only grafana (PollGrafana) and zabbix (PollZabbix) mirror source acks.
- Generic-hook alerts get fingerprint prefix `grafana:` (quirk, not `generic:`) → PollGrafana's absence-reconcile can resolve them mid-scenario: smoke/Playwright must NOT assert dedupe copies for them.
- Connection tests (`connectionOk`) return `Either Text ()`; controllers log the reason via Log.logWarn (Request has NO `logger` field — shadow `?context = ?context.frameworkConfig` first).

## LLM
- Never name a column `error`: generated `LlmAnalysis.error` would clash with Prelude.error in every module importing Generated.Types (llm_analyses uses `error_message`).
- LLM provider CRUD on /admin/llm: llm_configs UI, enable flips others off transactionally (llm_configs_enabled_idx unique-WHERE). DB-first resolution (`llmConfigFromDb`/`currentLlmConfig`) in Application/Service/Llm/DbConfig.hs — separate module because the service record `LlmProviderConfig` shares field names with the generated one; callers using `.endpoint` etc must import `LlmProviderConfig (..)`.
- Structured output = fenced ```json, parsed best-effort in Application/Service/Llm/Output.hs; card renders markdown as escaped <pre> (no markdown lib in deps).
- Prompt-hash dedupe runs in the JOB (not at ingest): hash needs the rendered prompt. Only hits for same-alert re-analysis — AlertEvent timestamps make cross-alert prompts unique.
- Enrichment re-analysis marker: llm_analyses.error_message = 'enrichment_retrigger', caps at one per alert.
- Agent roles: llm_analyses.agent_role_id (nullable) + llm_agent_roles; resolveAgentRole = row id → is_default → legacy. tools jsonb [] means NO tools (role filters toolDefinitions by name); no role + toolsEnabled = full set. Seeds create default-enricher in BOTH seed-halemans and smoke-check.sh (guard checks `NOT EXISTS (is_default)`, not just name).
- alert_enrichment template is v2+ ({{assets_excerpt}} slot); seed deactivates only versions <2 so admin-created newer versions survive re-seeds.
- llmPanelHtml shows the newest TERMINAL (done/failed) analysis; a pending newer row shows llm-pending-note — unless the pending row's JOB failed (jobErrors), which wins. Playwright checks depend on this precedence.
- Record field selectors must be IMPORTED to get GHC's HasField magic (`import M (ParsedOutput (..))`), or cross-module `parsed.markdown` fails with Could-not-deduce-HasField.

## Assets (Jira Assets / CMDB)
- assets_objects.attributes is a jsonb OBJECT (Aeson.object from flattened pairs) — Aeson.toJSON on [(k,v)] encodes an array of pairs and silently breaks the Attrs.objectAttributes parser. BIGINT columns generate as Integer (assets_objects.object_id); column `label` generates as `label_`.
- Cache/link: Application/Service/Assets/Cache.hs; negative cache = asset_alert_links row with NULL assets_object_id, matched_by "miss:<config_id>:<host>", 30min TTL, cross-alert. Refresh button (RefreshAssetsAction) bypasses the TTL.
- Asset icons served from the app: GET /assets/objects/{id}/icon (AssetsIconsController, view privilege) lazy-fills assets_icon_cache (config_id+url unique, bytea) via Assets.fetchBinary; Fragments renders <img> at that route, never the Jira origin. absoluteIconUrl in Application/Service/Assets/Icons.hs.
- WS kind "assets" is panel-only: Live.updatesFor ScopeAlert skips status badge + timeline for it (a second notify from EnrichAlert would double-prepend the timeline).

## Facets & dashboards
- Facet refs: `field:` = raw column, `label:` = alerts.labels, `attr:` = materialized alerts.facets. Card match ops =/!=/~/in; `~` → LIKE via globToLike; `!=` never matches absent values. The prefixed refs exist only in dashboard card JSON — field_mappings.key is UNPREFIXED, interpreted by kind (field = closed enum env/host/service/check/severity/status, label = labels key, attr = Assets attribute name).
- attr-kind field mappings take the FIRST element of comma-separated Assets attribute values, stripped (`firstCsvElement`, Application/Service/Facets.hs); verbatim whitelisted copies keep the full string.
- Schema: alerts.facets jsonb + GIN (jsonb_path_ops), field_mappings (UNIQUE facet+rank), facet_backfill_jobs (cursor-chained, chunk 500). Facets materialize onto alerts.facets only — the alert card always shows raw columns. Backfill needs the worker; attr facets need linked assets (EnrichAlertJob ran).
- facetValue lives in Application.Pipeline.Grouping (pure); Service.Facets re-exports. DashboardCard v2 AST + legacy {env,filters} round-trip via cardLegacy flag + cardExtras (unknown keys preserved).
- Card templates: `"forEach": "<facetRef>"` expands per distinct value (title `{value}` placeholder); `"hideWhen"` hides when count <= N; `"summary": true` renders the rollup (runCardSummary); `"sortBy": [...]` (builtins key:severity|count|title, `-` prefix flips, facet refs use the pinned value); `"alertSortBy": [...]` orders the card's ALERTS (columns = AlertList validSortColumns, natural dir severity=critical-first/last_seen_at=newest-first, `-` flips; sortCardAlerts in DashboardCards, stable over lastSeenAt-desc base, limit applied after sort); `"size": {"width","height"}`. Card detail page shares the /alerts sortable table widget (alertsTableHtml/AlertsTable in Fragments); its ?sort=&dir= header links are transient — NOT persisted to users.settings (renderCardDetail in Web/Controller/Dashboards). Expansion+hide in expandDashboardCards, hidden→summary→grouped→flat dispatch in fetchCardData (Web/View/Dashboards/Show) — called by BOTH the controller and the Live broadcaster; templates never persist expanded. CardData in Show.hs is a view type — the service layer must not depend on it.
- Rollup widget: RollupCard + rollupCardHtml in Web/View/Fragments.hs renders BOTH overview env cards and custom summary cards. GOTCHA: a record field of type Html breaks GHC's HasField selector magic even in the defining module — pattern-match with RecordWildCards.
- Dashboard Show page WS scope is `dash:<uuid>` (ScopeUserDashboard). It must expand the FULL card config then filter expanded cards by match — expanding a pre-filtered template list shifts ecIndex and re-renders wrong domIds. No alert.status pre-filter either. Vanished forEach values need explicit mode:"remove" fragments keyed by expandedDomId.
- Detail page GET /dashboards/:id/cards/:index[?value=] (ShowDashboardCardAction) re-expands and re-matches by index+value — no persisted card addressing.
- Stale limitation: a forEach card whose last alert closes is only removed on page reload (closed-alert events skip re-render by design).
- Alert card context panels (cmdb/jira/writeback chip) live-update via WS kinds "enriched"/"writeback" (contextPanelUpdates in Live.hs). WS supports multi-scope: data-live-scope="env:a,env:b".

## Views
- Shared widgets in Web/View/Fragments.hs: pageHeaderHtml/sectionHeaderHtml, inlinePostFormHtml, editDeleteActionsHtml, enabledBadgeHtml/stateBadgeHtml, statusBadgeHtml/severityBadgeHtml, panelHtml, detailsJsonHtml, externalLinkFooterHtml. Form/table widths: .maxw-400/500/600/700 in app.css.
- ALL display timestamps go through utcTimeHtml/maybeUtcTimeHtml/utcTimeOrHtml (Application/Helper/View.hs — <time datetime> UTC fallback, app.js rewrites to browser-local). Raw show/tshow of UTCTime in views is a bug. Blackouts form inputs keep isoUtc text.
- HSX Maybe attribute values omit the attribute (ApplyAttribute Maybe instance) — used for optional style/testid.
- Dashboard grid: #dashboard-cards flex-wrap with .dashboard-card-summary flex 1 1 340px; Layout body is container-fluid px-4.
- users.settings.theme (validated against Application.Helper.Theme themes) drives `<html data-theme>`; app.js halemansApplyTheme persists via POST /profile/theme.

## Public API & metrics
- Read-only JSON API (`/api/v1/alerts`, `/api/v1/alerts/:id`, `/api/v1/environments`), `/metrics` Prometheus exporter, `api_tokens` table, profile UI token mgmt + admin revoke-any, per-token in-memory rate limit (Application/Service/Api/*).
- Token hash = sha256 hex via cryptonite `convertToBase Base16`; prefix = first 8 chars of plaintext. resolveToken touches last_used_at at most once/min.
- Rate limits: 120/min API, 6/min /metrics; env overrides HALEMANS_API_RATE_LIMIT / HALEMANS_METRICS_RATE_LIMIT (read per request).
- Smoke token: checks.smoke exports HALEMANS_API_TOKEN="halemans-smoke-api-token-v1" (fixed test secret, sre@dev, both scopes); dev seed generates a random one into `.devenv/state/halemans/api-token` (name demo-cli). run.sh falls back to the state file.
- Audit export: GET /admin/audit/export (admin), CSV/JSONL, scope ≤1 of environment/alert + time range (default 7d); audit_exports row inserted before streaming, row_count updated after.

## Provisioning
- Application/Service/Provision.hs: `applyProvisionConfig :: (?modelContext) => FilePath -> IO ()`, strict aeson parsers reject unknown keys; per-category advisory-locked transaction; strict delete FK violations caught per row, rethrown as `ProvisionError` naming entity + blocking constraint.
- Categories include `fieldMappings` (upsert on UNIQUE(facet,rank); kind validated, field-kind keys checked against parseAlertField) and `dashboards` (upsert by userEmail+name, config via decodeDashboardConfig, isDefault flips the user's other dashboards; strict deletes are GLOBAL — keep-lists must replay live rows incl. config). Examples in deploy/docker/provision.example.json.
- sources.name is unique.
- Config.hs provisioning hook: ConfigBuilder is evaluated before `ihpDefaultConfig`, so `DatabaseUrl` isn't in the tmap — call `defaultDatabaseUrl` + `withModelContext <url> noopLogger` inside `configIO`. Covers web + worker + dev server + run-script (all eval Config.hs). ghci `-e` exits 0 even when the expression throws — always capture stderr.
- Smoke provision scenario (run.sh, last) is gated on `SMOKE_APP_MANAGED=1` — only smoke-check.sh sets it (exports SMOKE_APP_PID/SMOKE_APP_LOG; run.sh `restart_app` kill/relaunches `$RUN_PROD_SERVER` with HALEMANS_PROVISION_CONFIG). Dev-stack smoke skips it.
- halemans-gen-password (nix/lib.nix genPassword, script nix/scripts/gen-password.sh) prints `password:`/`passwordHash:` lines + a users.items JSON fragment; pwgen + genPassword in devenv packages and checks.smoke nativeBuildInputs.

## Docker deployment (deploy/docker/)
- End-user flow needs NO nix: image carries RunProdServer/RunJobs/EnqueuePollers/GenPassword/psql/busybox-sh + schema bundle at /share/db-init (dockerTools links contents' bin/* into /bin automatically). `/bin/db-init` (deploy/docker/db-init.sh, baked via extraCommands) applies 00-ihp-schema/01-app-schema/02-roles/99-schema-migrations guarded on to_regclass('alerts'). roles.sql at deploy/docker/roles.sql (keep in sync with seed-halemans.sh + smoke-check.sh role privileges).
- Upgrades auto-migrate: `/bin/db-migrate` (deploy/docker/db-migrate.sh) applies pending /share/db-migrate/*.sql (baked from Application/Migration), records revisions in schema_migrations, pg_advisory_lock(hashtext('halemans-db-migrate')) + in-lock re-check make concurrent app/worker boots safe; compose app/worker/enqueue-pollers prefix `db-migrate && exec ...`. No-op on fresh DBs and on uninitialized DBs (defers to db-init). Gotchas: the dockerTools image has NO /tmp (pipe generated SQL to psql stdin, never mktemp); busybox psql \if needs a `\gset` boolean var.
- GenPassword = Application/Script/GenPassword.hs → `script-GenPassword` package (IHP flake module auto-packages Application/Script/*.hs, EXCLUDING Prelude.hs). Runs without a DB. Script getArgs returns [Text] (CorePrelude), not [String].
- CI: .github/workflows/docker-images.yaml builds .#docker-image via nix (needs the devenv-root override dance: mkdir .devenv + printf $PWD > .devenv/root, --impure --override-input) and pushes ghcr.io/gvnkd/halemans:{latest,sha,v*}.
- compose: deploy/docker/docker-compose.yaml, env via .env.example→.env. Host port HALEMANS_PORT (8000 clashes with local Taiga). Docker daemon IS reachable interactively as pion (rootless restriction is nix-sandbox-only). Local image test loop: nix build .#docker-image (override dance) + docker load + scratch postgres:16-alpine container.

## Logging
- HALEMANS_LOG_LEVEL=debug|info|warn|error (default info, validated at boot in Config.hs, read per call in Application/Service/Log.hs); HALEMANS_ACCESS_LOG=0 disables wai request logging (RequestLoggerMiddleware override — `option` is first-wins vs ihpDefaultConfig).

## Commands
- Full stack: `nix develop .#default --impure -c devenv-flake-up -D` (detached). Attach: `process-compose -u /run/user/1000/devenv-*/pc.sock process list`.
- **Do NOT use the `devenv` CLI wrapper from a direnv-loaded shell** — it reuses the stale in-shell `devenv-flake-up` after nix/*.nix changes. Use the `nix develop` form above (or reload direnv).
- Smoke suite (needs running stack): `nix develop .#default --impure -c smoke-test`.
- Canonical check: `nix flake check --impure` (includes checks.smoke: full suite, sandboxed, no docker needed).
- Seed manually: `nix develop .#default --impure -c seed` (idempotent). `stack-status` for health report.
- Plain `nix eval`/`nix build` on this flake needs `--override-input devenv-root "file+file://$PWD/.devenv/root"` (create it: `mkdir -p .devenv && printf %s "$PWD" > .devenv/root`).
- Perf harness: nix/scripts/perf-harness.sh (manual gate, not a flake check). psql generate_series seeding (ORM seeding too slow).
- Run test subsets: `ghci -v0 Test/Main.hs -e 'main'` / `ghci -v0 Test/Integration.hs -e 'System.Environment.withArgs ["--match","..."] main'` with DATABASE_URL exported.

## Ports (this machine has squatters — check before changing)
- Halemans app **28080** (toolserver 28081 = PORT+1). NOT 8000 — that's the local Taiga (never touch, runs as user taiga). 8080/8081/8888/18080 also taken.
- grafana 3001 (admin/admin), alertmanager 9093, zabbix web/API 10080, zabbix server 10051, agent 10050. Mocks 18082-18085.
- Tokens live in `.devenv/state/` (gitignored): halemans/{am-hook-token,generic-hook-token,env.sh,seed.done,api-token}, zabbix/token, grafana/token.
- Dev stack live DB socket is `/run/user/1000/devenv-*/postgres` — the stale direnv `/tmp/devenv-*` path fails; the running app's DATABASE_URL is in /proc/<RunDevServer>/environ.

## Nix/stack pitfalls (hard-won)
- devenv 2.0's tasks wrapper never marks one-shot processes completed → `depends_on condition=process_completed_successfully` never fires. Web/worker gate on the marker file `.devenv/state/halemans/seed.done` instead.
- process-compose holds configs baked at server start: after changing nix/*.nix, `down` + full `devenv-flake-up` (process restart is NOT enough). Orphaned RunDevServer/ghc children can survive `down` and hold ports — `pkill -f RunDevServer/RunDevWorker` and check `ss -ltnp`.
- writeShellApplication runs shellcheck and fails on info-level findings (SC1091 source, SC2034 unused loop var) — add disable directives.
- Nix sandbox: loopback works (checks.smoke boots everything in-sandbox); docker does NOT (socket root:docker, nixbld excluded); `__noChroot` doesn't help (worktree unreadable for nixbld, /home/pion is 700). Podman doesn't help either — don't retry.
- nix/scripts/*.sh are plain files (readFile) — no `''${}` nix escaping there; `''` leaks break shellcheck.
- ghcWithPackages exposes ONLY packages listed in flake.nix haskellPackages (transitive deps get stub confs). Test-only deps (warp for Test/HttpSpec) must be listed too. The devenv shell's ghc env does NOT rebuild on haskellPackages edits without a full stack restart; `nix build .#checks.x86_64-linux.tests` picks them up immediately.
- cache.digitallyinduced.com 500s on a narinfo block substitution entirely; the path is often already local → re-run `nix build ... --offline`.
- DB is named `halemans` (was IHP default `app`): the IHP flake module hardcodes `app` in env.DATABASE_URL/PGDATABASE/initialDatabases — flake.nix mkForce-overrides all three. ihp.appName only renames derivations.
- postgres under devenv dies on full disk (PANIC checkpoint) and crash-loops even after space frees: kill leftover postmaster, rm postmaster.pid + core.*, `process-compose process start postgres`.
- Worker dev (RunDevWorker) recompiles via GHCi each boot (minutes) — smoke waits for a succeeded poller job before firing.

## Zabbix (native zabbix70, no docker)
- API is served by the PHP frontend (`php -S` on 10080), NOT by zabbix_server (10051 trapper).
- DB = `zabbix` database in the devenv postgres, imported by `halemans-zabbix-db-init`; DB user = OS user (no `postgres` role in devenv postgres) — rendered dynamically into configs.
- Zabbix 7.0 `token.create` returns only ids; secret comes from `token.generate`.
- `history.push` 200s with `data[].error` until the server config cache learns new items (CacheUpdateFrequency=5 in our template); fire-test-alert-zabbix retries until `data | all(has("itemid"))`.
- Fresh import creates default "Zabbix server" host with health/OS templates → real host alerts pollute ingestion; seed-zabbix disables it.
- Zabbix trigger events: problem event and OK event have different eventids → Halemans fingerprint is trigger-scoped: `zabbix:trigger:<id>`.
- First zabbix sync is bounded to `initialHistoryDays` back from now (source config key, default 1 day) via `initialCursor` in Application/Job/PollZabbix.hs; once `last_sync_cursor` exists the key is ignored.
- Host-group filtering: `teams.host_groups` jsonb (names); source config `hostGroupScope` = "all" (default) | "teams" → PollZabbix resolves team group names against the `zabbix_host_groups` cache table (never the API) and passes `groupids` to event.get; empty match = skip cycle. Cache populated via POST /sources/:id/sync-host-groups (button on Sources index, zabbix only) OR at boot via provisioning `hostGroupsFile` (bare array or full hostgroup.get dump). Both go through `replaceHostGroupCache` in Application/Service/HostGroups.hs. Team form offers cached names as filterable multi-select (`hostGroupPicker` in Web/View/Teams/New.hs, paramList @Text "hostGroups"; JS in teamPickersScript).
- Resolved-state reconcile (PollZabbix.reconcileProblemStates): one batched `problem.get` (recent=false, `object=0` + `objectids`=trigger ids — problem.get has NO `triggerids` param, -32602) per due cycle resolves local firing/ack alerts whose trigger has no open problem — keyed on the TRIGGER id from the fingerprint, NOT alerts.external_id (refires don't rotate external_id, so the stored event id can point at an already-resolved older problem). Refires make a trigger's LATEST problem row the state source (`latestProblemByTrigger`). Config keys (defaults): reconcileResolved(true), reconcileGraceSeconds(60, guards ingest/reconcile race via last_seen_at), reconcileIntervalSeconds(0=every cycle, tracked in sources.last_reconcile_at), absentResolveMinAgeSeconds(86400, no-problem-rows = purged/deleted trigger → resolve only if alert older; guards API permission gaps), eventPageLimit(1000). event.get is PAGED until a short page (post-outage backlog > 1 page used to skip OK events past the page boundary — the stuck-firing root cause). resolved_at is back-dated to r_clock via a guarded UPDATE after ingest.
- SourcesController.sourceConfig MERGES form-managed keys onto the existing config jsonb — keys the form doesn't know (reconcile*, expectedIntervalSeconds, hostGroupsFile, provisioned extras) survive UI edits. Add new form-managed keys to managedKeys there.

## Grafana
- Alert rule `dev-cpu-sim`: API-provisioned by seed-grafana (upsert; restores canonical non-firing threshold +1e9). fire-test-alert-grafana flips to -1e9/+1e9 (random_walk is unbounded → deterministic).
- Rule data chain: A=testdata random_walk → B=reduce last → C=`threshold` expression. classic_conditions CANNOT take expression inputs — use `threshold`.
- Rule-group interval PUT needs `{"title","interval"}` and interval must be a multiple of the scheduler tick (10s — evaluation_interval doesn't lower it).
- Grafana 12: `[alerting] enabled` is rejected (legacy removed) — only `[unified_alerting]`. grafana.ini paths use `$__env{DEVENV_STATE}`.
- Contact point/policy file-provisioned; `$HALEMANS_GENERIC_HOOK_TOKEN` expanded by grafana from process env.
- **Embedded alertmanager drops resolved alerts from `/api/alertmanager/grafana/api/v2/alerts` within seconds**. PollGrafanaJob reconciles resolves by ABSENCE (60s grace on last_seen_at) with a 90s refire guard. Firing alerts carry endsAt ~2min in the FUTURE — never use `endsAt <= now` alone.
- Poller/webhook titles must match or smoke's title assertions break: severity/summary live in ANNOTATIONS on the AM listing, labels on the webhook path — amAlertToNormalized checks annotations first.

## Alertmanager
- Must run with `--cluster.listen-address=""` (gossip binds 0.0.0.0:9094 otherwise → conflicts/sandbox failure).
- Config rendered at process start (sed @HALEMANS_AM_HOOK_TOKEN@) — templates in nix/lib.nix, shared with checks.smoke.

## IHP/Haskell specifics
- Schema.sql parser: NO inline `--` comments inside CREATE TABLE; NO column-level `REFERENCES` — FKs only as top-level `ALTER TABLE ... ADD CONSTRAINT ... FOREIGN KEY` (else the column generates as plain UUID, not `Id' "..."`).
- **Schema codegen ignores `ALTER TABLE ADD COLUMN`**: new columns must go inline into `CREATE TABLE` in Schema.sql; ALTERs live only in the migration file. Regenerate: `rm -rf build/Generated && build-generated-code` (or `make -f $IHP_LIB/lib/IHP/Makefile.dist build/Generated/Types.hs`).
- Column `type` → field `#type_`. Reserved words in HSX attribute expressions break HSX's parser — bind via `let` first. Ambiguous `record.id` with DuplicateRecordFields — use `get #id record`.
- IHP.Prelude lacks: fetch/fetchOneOrNothing (IHP.Fetch, even for jobs), traverse/mapM for Either is mapM, `<&>`, posixSecondsToUTCTime (Data.Time.Clock.POSIX), aeson parseMaybe (Data.Aeson.Types), `.=` (import from Data.Aeson), `void` (Control.Monad), `read`, unsafePerformIO (System.IO.Unsafe). IHP.Prelude's `head` returns Maybe (safe).
- typedSql: `${utctime}` params work; `::timestamptz` cast on a Text param makes it infer UTCTime. `${uuid}` from Data.UUID. Multi-col results = labeled SqlRow (`get #col_name`, FK cols decode as `Maybe (Id' "t")`), single-col = bare value. BIGINT columns need Int64 params; `LIMIT (${n} + 1)` infers Int. UNION ALL string-literal columns and `status::text` decode as **Maybe** — `fromMaybe` them; `coalesce(...)` counts as NOT NULL. sqlExecTyped returns IO Int64, void it. NO Maybe params (nullable-FK upserts → query-builder record API). Void pg functions: `SELECT 1 WHERE pg_notify(...)/pg_advisory_xact_lock(...) IS NULL` via sqlQueryTyped. `RETURNING id` decodes to `Id' "table"` directly. Params into jsonb columns must be `Aeson.Value`. `${idParam}` of type `Id' "t"` needs the `Id` constructor in scope (`import IHP.ModelSupport (Id' (..))`). `${...}` can't lex `get #field rec` — bind to a local first.
- typedSql needs ihp-typed-sql in haskellPackages; compile-time introspection hits $DATABASE_URL (IHP_TYPED_SQL_AUTO_DB=1 in shell; prod builds auto-detect via buildWithPostgres); checks integration/tests are overridden in nix/checks.nix to pre-load the schema. Stale introspection postmasters linger in /tmp/ihp-typed-sql--* — kill + rm when schema errors look wrong.
- HSX: no inline `case` or `<>` fragments inside [hsx| |] — extract helper functions returning Html; attribute values need concrete Text (annotate `:: Text` on `cs`/`show`); fragment renderers shared between views and the WS broadcaster live in Web/View/Fragments.hs (render to Text via IHP.HSX.Markup.renderMarkupText, needs ?request/?context).
- Generated record fields (e.g. AlertGroup.worstSeverity) silently clash with same-named local helpers in views — rename locals.
- warp's module is `Network.Wai.Handler.Warp` (there is no `Network.Warp`).
- HLS in this repo is often stale/wrong; trust `nix flake check --impure` + dev-server compile output, not LSP diagnostics.
- Auth: IHP LoginSupport; Config.hs needs explicit `module Config (config) where` or run-script's wrapper breaks on leaked field names; `CurrentUserRecord`/`HasNewSessionUrl` instances live in Application.Helper.Controller. Seeded users hashed via nix/scripts/hash-password.py (pwstore-fast pbkdf1 replica; salt fed to the hash is the base64 TEXT).
- WebSocket: IHP WSApp + `webSocketAppWithCustomPath @LiveController "ws"`; broadcaster = ihp-pglistener on `halemans_events` + per-connection scope registry (Application/Service/Live.hs). Scopes: dashboard/alerts/env:<name>/alert:<id>/group:<id>/dash:<uuid>. Session cookie name is SESSION (wai-session); WS clients must send {"type":"env","name":...} subscribe frames (masked) before receiving broadcasts. The socket SURVIVES turbolinks navigation: halemans-live.js re-subscribes on `turbolinks:load` by sending a {"type":"reset"} frame (server: isResetFrame clears the connection's scopeRef) followed by the current page's scope frames — without this, pages reached via in-place navigation keep the FIRST page's scope and get no live updates until a full reload.
- `paramOrNothing`/`param` take ByteString field names — `cs` interpolated names.
- RunProdServer: no Config/ dir in cwd → session key autogenerated. prod bins: unoptimized-prod-server/bin/{RunProdServer,RunJobs}.
- IHP dev server serves its LAST compile's error page forever; `touch` a source file to force recompile.

## Testing
- tests/smoke/run.sh: flags SMOKE_ZABBIX=0 for native-only subset. Fire scripts are retried/idempotent; suite matches alerts by fingerprint-prefix AND title. UI asserts log in as sre@dev via cookie jar (`login_as`).
- checks.smoke (nix/checks.nix + nix/scripts/smoke-check.sh): sandboxed, isolated $TMPDIR stack, prod binaries, full suite + Playwright (python3 playwright + playwright-driver.browsers; chromium needs FONTCONFIG_FILE=makeFontsConf[dejavu], --no-sandbox --disable-dev-shm-usage --no-zygote, writable HOME). Rebuilds when any tracked file changes (srcHash = self.outPath). **smoke-check.sh has its OWN inline seeding (does NOT call seed-halemans)** — new seeded rows/jobs must be added in BOTH places; worker needs `export GRAFANA_TOKEN`.
- checks.tests is overridden in nix/checks.nix too (same temp-postgres + schema pattern) because unit specs transitively import typedSql modules via connectors → Ingest. Integration.hs applies the schema itself when missing (to_regclass guard) so `runghc Test/Integration.hs` works against a dev DB.
- Integration tests against the DEV db race the dev worker — use future created_at (2999) and order-tolerant assertions; sandboxed check has no worker so it's deterministic there. GOTCHA: 2999-dated llm_analyses leftovers permanently sort above fresh rows (panel orders by createdAt DESC) — the LLM panel on such alerts never shows new analyses until the rows are deleted. Integration tests needing tokens on dev DB: export ASSETS_TOKEN/JIRA_TOKEN/ZABBIX_TOKEN from .devenv/state/halemans/env.sh into the ghci env or enrichment soft-fails silently.
- Dev-DB smoke runs can fail pre-existing scenarios from polluted dev state — only the sandboxed `nix flake check --impure` is authoritative. Dev-DB test tolerance: ensureMapping helper reuses seeded mapping rows.
- New files must be `git add -N`'d or the flake source won't see them.
- psql -c "INSERT ... RETURNING id" ALSO prints the command tag ("INSERT 0 1") on stdout — grep-extract the uuid, never capture raw. Smoke cleans up probe sources with `UPDATE enabled=false` (alerts FK blocks DELETE).
- Turbolinks (turbolinksMorphdom) replaces body WITHOUT pushState after form POSTs → Playwright `wait_for_url` never fires; wait on element testids instead. Real `page.goto` navigations DO fire URL changes.
- Playwright: `context.new_page()` shares the context cookie jar — logging in a throwaway user overwrites the main page's session; re-login main page afterwards.
- Dev DB rebuild: terminate backends, DROP+CREATE DATABASE halemans, apply ihp-schema + Schema.sql + Fixtures, then `INSERT INTO schema_migrations` all revision numbers (`revision BIGINT NOT NULL UNIQUE`), rm seed.done, `process-compose process restart seed` (one process at a time; multi-arg form prints usage). If seed.done is wrongly removed while the DB is already seeded, touch .devenv/state/halemans/seed.done manually.
