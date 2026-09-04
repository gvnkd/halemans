# Halemans project memories

## What this is
IHP (Haskell) app aggregating alerts from Zabbix/Grafana/Alertmanager. Design: design_docs/01_highlevel.md; milestone 0 (dev/test env) done — see design_docs/milestone_0.md §10 for implementation notes.

## Commands
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

## Grafana
- Alert rule `dev-cpu-sim`: API-provisioned by seed-grafana (upsert; restores canonical non-firing threshold +1e9). fire-test-alert-grafana flips to -1e9/+1e9 (random_walk is unbounded → deterministic).
- Rule data chain: A=testdata random_walk → B=reduce last → C=`threshold` expression. classic_conditions CANNOT take expression inputs (only raw datasource queries) — use `threshold`.
- Rule-group interval PUT needs `{"title","interval"}` and interval must be a multiple of the scheduler tick (default 10s — evaluation_interval didn't lower it; we use 10s).
- Grafana 12: `[alerting] enabled` is rejected (legacy removed) — only `[unified_alerting]`.
- Contact point/policy file-provisioned; `$HALEMANS_GENERIC_HOOK_TOKEN` expanded by grafana from process env (provisioning env expansion works).
- `[unified_alerting]` ini, grafana.ini paths use `$__env{DEVENV_STATE}`.

## IHP/Haskell specifics
- Schema.sql parser: NO inline `--` comments inside CREATE TABLE; NO column-level `REFERENCES` — FKs only as top-level `ALTER TABLE ... ADD CONSTRAINT ... FOREIGN KEY` (else the column generates as plain UUID, not `Id' "..."`).
- Column `type` → field `#type_`. Reserved words in HSX attribute expressions break HSX's parser — bind via `let` first.
- Regenerate model types: `make -f $IHP_LIB/lib/IHP/Makefile.dist build/Generated/Types.hs` (in nix develop shell).
- IHP.Prelude lacks: fetch/fetchOneOrNothing (IHP.Fetch), traverse/mapM for Either is mapM, `<&>`, posixSecondsToUTCTime (Data.Time.Clock.POSIX), aeson parseMaybe (Data.Aeson.Types), `.=` (import from Data.Aeson explicitly).
- Ambiguous `record.id` with DuplicateRecordFields — use `get #id record`.
- Self-rescheduling jobs: override `queuePollInterval` (default 60s!) for sub-minute loops.
- Worker dev (RunDevWorker) recompiles via GHCi each boot (minutes) — smoke waits for a succeeded poller job before firing.
- typedSql needs ihp-typed-sql in haskellPackages; compile-time DB is auto (IHP_TYPED_SQL_AUTO_DB=1 in shell; prod builds auto-detect via buildWithPostgres).
- RunProdServer: no Config/ dir in cwd → session key autogenerated. prod bins: unoptimized-prod-server/bin/{RunProdServer,RunJobs}.
- HLS in this repo is often stale/wrong; trust `nix flake check --impure` + dev-server compile output, not LSP diagnostics.

## Alertmanager
- Must run with `--cluster.listen-address=""` (gossip binds 0.0.0.0:9094 otherwise → conflicts/sandbox failure).
- Config rendered at process start (sed @HALEMANS_AM_HOOK_TOKEN@) — templates in nix/lib.nix, shared with checks.smoke.

## Testing
- tests/smoke/run.sh: flags SMOKE_ZABBIX=0 for native-only subset. Fire scripts are retried/idempotent; suite matches alerts by fingerprint-prefix AND title.
- checks.smoke (nix/checks.nix + nix/scripts/smoke-check.sh): sandboxed, isolated $TMPDIR stack, prod binaries, full suite. Rebuilds when any tracked file changes (srcHash = self.outPath).
