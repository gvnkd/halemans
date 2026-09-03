# Milestone 0 — Dev/Test Environment Foundation

Goal: a single `devenv up` brings up the full local stack (PostgreSQL, Zabbix, Grafana, Alertmanager, Halemans app + worker), all sources are pre-provisioned with credentials and alert-producing fixtures, and an automated smoke suite proves alerts flow from each source into the Halemans backend and are visible in the web UI.

This milestone builds no product features. It exists so every later phase (1–5) has a runnable, testable foundation from day one, per the testing strategy in `01_highlevel.md` §16.

## 1. Deliverables

| # | Deliverable | Acceptance |
|---|---|---|
| D1 | `devenv up` starts the full stack with one command | All processes healthy via process-compose |
| D2 | Zabbix server + agent, dev API token, seeded host + trigger | `zabbix problem.get` returns data with dev token |
| D3 | Grafana with dev service-account token, seeded alert rule | Alerting API reachable, rule fires on demand |
| D4 | Alertmanager wired to accept test alerts | `POST /api/v2/alerts` accepted, webhook configured |
| D5 | Halemans app + worker run under devenv | Web UI reachable, `nix flake check --impure` green |
| D6 | Smoke/E2E test: fire alert in each source → assert arrival in Halemans DB + web UI | Suite runs in `nix flake check --impure` |
| D7 | All custom nix code in `./nix/`, imported by root `flake.nix` | Root flake stays thin |

## 2. Layout

```
flake.nix                  # thin: imports ./nix/devenv.nix into devenv.shells.default
nix/
  devenv.nix               # devenv shell module: imports the service modules below
  postgres.nix             # PostgreSQL for Halemans (+ zabbix db if native zabbix)
  zabbix.nix               # zabbix process definitions + provisioning scripts
  grafana.nix              # grafana process + provisioning (datasources, alert rules, SA token)
  alertmanager.nix         # alertmanager process + config (webhook receiver → halemans)
  scripts.nix              # devenv scripts: seed-*, fire-test-alert-*, smoke-test
tests/
  smoke/                   # integration/E2E smoke suite (see §6)
```

Root `flake.nix` change is minimal:

```nix
devenv.shells.default = {
    imports = [ ./nix/devenv.nix ];
};
```

## 3. Source services

Grafana and Alertmanager are single static Go binaries in nixpkgs — run them as native devenv `processes` with generated config files. Zabbix is the heavyweight: two options, decided below.

### 3.1 Zabbix

- **Approach: OCI containers via docker-compose (docker runtime), wrapped in a devenv process.** `zabbix-server-pgsql` + `zabbix-web-nginx-pgsql` + `zabbix-agent` official images, dedicated postgres service inside the compose project (isolated from the Halemans DB). Zabbix-server has no clean native nixpkgs service path outside NixOS modules; containerizing keeps the dev shell reproducible and matches upstream docs.
- **Provisioning (idempotent script `seed-zabbix`)**: waits for API health, logs in with default `Admin/zabbix`, then via JSON-RPC:
  - creates a dev API token (fixed name `halemans-dev`), writes it to `.devenv/state/zabbix/token` (gitignored) and exports `ZABBIX_TOKEN` via devenv env;
  - registers host `dev-host-01` (zabbix-agent, env label `dev`);
  - creates a controllable trigger pair: one item the seed script can flip problem/OK on demand (`fire-test-alert-zabbix`) so alert generation is deterministic, not timer-based.
- Ports: API/web `10051/8080→localhost:10080` (avoid clash with IHP :8000).

### 3.2 Grafana

- Native `pkgs.grafana` binary as a devenv process; data dir under `.devenv/state/grafana`.
- File-based provisioning dropped in by the nix module:
  - admin user fixed (`admin/admin`, dev-only);
  - **dev service account + token** created by an idempotent `seed-grafana` script against the API on first boot, token written to `.devenv/state/grafana/token` → `GRAFANA_TOKEN`;
  - a testdata datasource + one alert rule (`dev-cpu-sim`) evaluating the testdata random-walk with a threshold the seed script can force into firing/OK deterministically;
  - contact point + notification policy pointing at Halemans webhook `POST /hooks/generic/:token` (direct; no Alertmanager forwarding in this milestone).
- Port `3001` (3000 is common dev clutter).

### 3.3 Alertmanager

- Native `pkgs.prometheus-alertmanager` as a devenv process; config rendered by the nix module with a `halemans` webhook receiver → `POST /hooks/alertmanager/:token` on the app.
- No auth token needed (Alertmanager has none); Halemans-side per-source `WebhookToken` is the credential, seeded via fixtures.
- `fire-test-alert-alertmanager` script posts directly to `http://localhost:9093/api/v2/alerts` with a fixed labelset (`env=dev`, `host=dev-host-01`) and matching `EndsAt` control for resolve.
- Port `9093`.

## 4. Wiring into Halemans

- `Application/Fixtures.sql` seeds the three `Source` rows (zabbix/grafana/alertmanager, all `env=dev`) plus `WebhookToken`s with values passed via env (`${HALEMANS_AM_HOOK_TOKEN}`), so smoke tests and alertmanager/grafana configs share one token source generated at `devenv` enterShell time and stored in `.devenv/state/`.
- Source credentials resolve via env vars per `01_highlevel.md` §14 — no plaintext secrets in DB or repo. Only dev tokens ever exist; they are local-only and gitignored.

## 5. Process topology (devenv)

```
process-compose (devenv up)
├── postgres          (Halemans DB)
├── zabbix-compose    (zabbix-server + zabbix-web + zabbix-agent + zabbix-db)
├── grafana
├── alertmanager
├── seed              (oneshot: seeds sources, tokens, fixtures; depends_on healthy)
├── app               (IHP web)
└── worker            (WorkerMain jobs)
```

Health gates: `seed` runs only after zabbix API, grafana API, alertmanager, and postgres all answer; `app`/`worker` start after `seed` completes so webhook tokens exist before receivers point at them.

## 6. Smoke / E2E suite

Location `tests/smoke/`, wired into `nix flake check --impure` as a dedicated check that boots the devenv stack in an isolated state dir.

Scenarios (one per source, identical shape):

1. **Zabbix**: `fire-test-alert-zabbix` flips the dev trigger → within poll interval + slack, assert via SQL (`sqlQueryTyped`) that an `Alert` with matching fingerprint exists, then HTTP-GET the alert card and assert the title renders. Flip back → assert `resolved`.
2. **Alertmanager**: post firing alert to :9093 → alertmanager webhooks into Halemans → assert alert visible in UI; post resolved → assert `resolved`.
3. **Grafana**: force `dev-cpu-sim` into firing → assert arrival via generic-hook/poller path → assert visible in UI.

Plus a stack-health test asserting all interconnections (app→DB, worker→DB, app reachable, each source API reachable with its dev token).

These smoke tests double as the harness for Phase 1+: golden-payload unit tests and Playwright suites (§16 of highlevel) plug into the same booted stack.

## 7. Commands (devenv scripts)

| Command | Purpose |
|---|---|
| `devenv up` | full stack |
| `seed` | re-run provisioning (idempotent) |
| `fire-test-alert-{zabbix,alertmanager,grafana} [resolve]` | deterministic alert generation |
| `smoke-test` | run §6 against the running stack |
| `stack-status` | per-service health + token presence report |

## 8. Acceptance checklist (end of Milestone 0)

- [x] Fresh clone → `devenv up` → all processes healthy without manual steps
- [x] `zabbix`, `grafana`, `alertmanager` each hold a working dev credential (token file / webhook token) consumed by Halemans config
- [x] App + worker start; seeded dev sources visible at `/sources` (login/roles land in phase 1)
- [x] `fire-test-alert-*` for all three sources → alert arrives in DB and renders in web UI; resolve propagates
- [x] `smoke-test` green locally and inside `nix flake check --impure` (zabbix scenario excluded from the sandboxed check, see §10)
- [x] No nix code outside `./nix/` except the thin import in root `flake.nix` (plus the `checks` import line)
- [x] No secrets committed; dev tokens live only under `.devenv/state/` (gitignored)

## 9. Decisions

- **Zabbix container runtime**: docker. The devenv module requires a docker daemon on the dev machine; no podman fallback.
- **Grafana unified alerting**: direct webhook to Halemans (`POST /hooks/generic/:token` contact point). The Alertmanager-forwarding path is deferred until the Phase 2 Alertmanager connector lands.

## 10. Implementation notes (as built)

- **App port is 28080, not 8000**: port 8000 is occupied on the dev machine by the local Taiga instance. IHP derives the toolserver port as PORT+1 (28081). Webhook targets in alertmanager/grafana configs point at 28080.
- **Webhook tokens**: psql does not expand env vars, and `Application/Fixtures.sql` is baked at nix eval time — so Fixtures.sql carries only the static `sources` rows, while `webhook_tokens` are upserted by `seed-halemans` from runtime-generated files under `.devenv/state/halemans/` (same intent as §4, different mechanism).
- **Zabbix API token**: Zabbix 7.0 `token.create` returns only ids; the secret is obtained via a follow-up `token.generate` call (see `nix/scripts/seed-zabbix.sh`).
- **Zabbix test trigger control**: `fire-test-alert-zabbix` pushes 1/0 into a trapper item via `history.push`; the poller watches trigger events (`event.get`, source=0) with a cursor on `sources.last_sync_cursor`.
- **Grafana test rule**: `dev-cpu-sim` is API-provisioned by `seed-grafana` (upsert, canonical form restored on each seed); `fire-test-alert-grafana` flips the threshold between -1e9/+1e9 over the testdata `random_walk`. Rule uses reduce + `threshold` expressions (classic_conditions cannot consume expression inputs).
- **Grafana timing**: scheduler tick stays at the default 10s; rule-group interval is 10s, notification policy group_wait 5s / group_interval 10s.
- **Oneshot `seed` gating**: devenv 2.0's task wrapper never reports a finished one-shot process as completed, so `process_completed_successfully` never fires. App/worker instead wait for the marker file `.devenv/state/halemans/seed.done` written by `seed` (doc §5 intent preserved).
- **`checks.smoke` (nix flake check)**: runs fully sandboxed and boots an isolated native stack (postgres + grafana + alertmanager + prod app + jobs worker) in `$TMPDIR`, then runs the smoke suite with `SMOKE_ZABBIX=0`. The zabbix scenario needs docker, which is unreachable from the nix sandbox (daemon socket is group-restricted), so the full three-source suite runs against the dev stack via `smoke-test`.
- **First boot latency**: the worker's first GHCi compile takes minutes; the smoke suite waits for the poller loop to be warm before firing the zabbix test alert.
- **devenv CLI wrapper**: when the current shell is a direnv-loaded devenv shell, `devenv up` reuses the shell's (possibly stale) `devenv-flake-up`. After changing `nix/*.nix`, restart the stack with `nix develop .#default --impure -c devenv-flake-up -D` or reload direnv.

