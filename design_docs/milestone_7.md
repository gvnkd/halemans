# Milestone 7 — Provision configs (declarative bootstrap)

Goal: at app start, if a provision config is provided, create/update database
entities from it — users (with pre-hashed passwords), sources, teams, and LLM
config — so a fresh deployment boots into a working state without clicking
through the UI. Ships with a password-generation helper (`pwgen` + the existing
pwstore-fast hash replica) to produce the `password_hash` values the config
consumes.

Builds on the milestone-6 stack. No new external services, no new mocks.
Phase 1 scope (this milestone): users, sources, teams, llm config. Later
phases can extend the same mechanism to notification rules, grouping rules,
retention, dashboards.

## 1. Deliverables

| # | Deliverable | Acceptance |
|---|---|---|
| D1 | Provision config format + loader (`Application/Service/Provision.hs`) with per-category `strict` flag (default `false`) | Missing/empty config path = no-op; malformed file = startup abort with a clear error naming the offending section |
| D2 | Startup hook: provisioning runs in both web and worker processes before serving/working | `RunProdServer` and `RunJobs` both apply the config idempotently; second boot changes nothing (no duplicate rows) |
| D3 | Users provisioning (email, display_name, password_hash, roles, settings) | Users log in with the provisioned password; roles auto-created when unknown; re-run updates password_hash and role assignments |
| D4 | Password tool `halemans-gen-password` (pwgen plaintext + hash + config snippet) | Output hash verifies against login; pwgen available in the devenv shell |
| D5 | Sources provisioning (type, name, base_url, env, poll interval, enabled, config jsonb, webhook tokens by env reference) | Provisioned zabbix/grafana/alertmanager/webhook sources poll/accept hooks exactly like fixture-seeded ones |
| D6 | Teams provisioning (name, description, host_groups, defaults, members by email + role, default dashboard) | Members resolve against provisioned-or-existing users; unknown member email = config error at startup |
| D7 | LLM config provisioning: new `llm_configs` table + DB-first resolution with env fallback; prompt templates provisionable | Active DB row drives `Application.Service.Llm`; no row → today's env behaviour unchanged |
| D8 | Tests (unit + integration) and smoke extension; `nix flake check --impure` green | Check green |

## 2. Provision config file

- One JSON file (aeson is already a dependency; no YAML lib in the tree and
  JSON keeps secrets-handling trivial). Path from env var
  `HALEMANS_PROVISION_CONFIG`; unset or empty → provisioning is skipped
  entirely (dev stack unaffected).
- Top-level sections, all optional. Every section is an object with a
  `strict` flag (default `false`) and an `items` list:

```json
{
  "users": {
    "strict": false,
    "items": [
      {"email": "ops@example.com", "displayName": "Ops",
       "passwordHash": "sha256|17|...|...",
       "roles": ["admin"], "settings": {"theme": "dark"}}
    ]
  },
  "sources": {
    "strict": true,
    "items": [
      {"type": "zabbix", "name": "zabbix-prod", "baseUrl": "https://zabbix.example",
       "env": "prod", "pollIntervalSeconds": 30, "enabled": true,
       "config": {"tokenEnv": "ZABBIX_TOKEN", "hostGroupScope": "teams",
                  "writeBack": true, "cmdbSpace": "OPS", "jiraProject": "OPS"},
       "webhookTokens": [{"tokenEnv": "HALEMANS_AM_HOOK_TOKEN"}]}
    ]
  },
  "teams": {
    "strict": false,
    "items": [
      {"name": "sre", "description": "Site reliability engineering",
       "hostGroups": ["Linux servers"], "defaults": {},
       "members": [{"email": "ops@example.com", "role": "lead"}],
       "defaultDashboardConfig": [{"env": "prod", "filters": {"status": [], "severity": []}}]}
    ]
  },
  "llm": {
    "strict": false,
    "items": [{
      "providerName": "default", "endpoint": "http://127.0.0.1:18084",
      "model": "qwen", "apiKeyEnv": "LLM_API_KEY", "toolsEnabled": false,
      "promptTemplates": [{"name": "alert_enrichment", "version": 1,
                           "body": "...", "active": true, "notes": "provisioned"}]
    }]
  }
}
```

- **`strict` semantics (per category, independent of each other):**
  - `strict: false` (default) — additive/upsert only: config entries are
    created or updated; DB rows absent from the config are left untouched.
  - `strict: true` — desired-state reconciliation: after upserting the
    config entries, every DB row of that category whose natural key is NOT
    in the config is deleted (users by email, sources by name, teams by
    name, llm_configs by provider_name, prompt templates by (name, version)
    within each provisioned template name).
  - A strict section with an empty `items` list deletes EVERYTHING in that
    category. That is the requested semantic, so the loader requires
    `"items"` to be present (even if empty) when `strict: true` — an omitted
    `items` is a config error, never interpreted as "delete all".
  - Deletion runs inside the same transaction as the upserts for that
    category; a FK-blocked delete (e.g. a source referenced by `alerts`, a
    user referenced by `alerts.acknowledged_by`) aborts startup naming the
    row and the blocking table. Operators disable instead
    (`"enabled": false` for sources) or clean up referencing data first —
    no silent fallback to soft-delete.

- Secrets never appear in the file in plaintext: source credentials and
  webhook tokens are `*Env` references resolved at runtime (same posture as
  §14 of `01_highlevel.md` and the existing `tokenEnv` convention). Passwords
  are the one deliberate exception-as-data: only the pbkdf1 hash is stored,
  produced by the D4 tool.
- Parsing is strict: unknown top-level keys and unknown per-entity fields are
  rejected (aeson `rejectUnknownFields`-style manual parsers — the Generated
  types don't derive aeson, so explicit parsers in
  `Application/Service/Provision.hs`); a typo'd field must fail loudly at
  startup, not be silently dropped.

## 3. Startup integration

- New module `Application/Service/Provision.hs` exporting
  `applyProvisionConfig :: (?context :: ...) => IO ()`-shaped entry point
  (exact implicit-param plumbing pinned during implementation against
  `IHP.ModelSupport`).
- Invoked from `Config.hs` (the `ConfigBuilder` runs in IO and is evaluated by
  both `RunProdServer` and `RunJobs`, so one hook covers web + worker; the dev
  server also picks it up when the env var is set). Keep the explicit
  `module Config (config) where` export list per project convention.
- Ordering: runs after framework config init (DB pool available), before the
  server accepts requests / the worker starts polling.
- Failure semantics: invalid JSON, schema violations, unresolved references
  (team member email that exists nowhere, missing `tokenEnv` variable) → log
  the specific error and abort startup. A half-provisioned boot is worse than
  no boot.
- Idempotency: every write is an upsert keyed on natural keys —
  `users.email`, `sources.name` (+ type), `teams.name`,
  `llm_configs.provider_name`, `llm_prompt_templates.(name, version)`.
  Re-runs update mutable fields (password_hash, enabled, config, membership
  roles) and insert missing rows only.
- **Per-category `strict` flag** (§2) controls deletion: `false` (default) =
  additive/upsert only, an entity removed from the file stays in the DB;
  `true` = full reconciliation, rows absent from the config are deleted.
  Strict deletes run per category in one transaction with the upserts, in
  dependency order (user_roles before users, webhook_tokens before sources,
  team_members before teams); a FK-blocked delete aborts startup naming the
  row — no silent soft-delete fallback.
- Concurrency: web and worker may provision simultaneously on a cold boot.
  All upserts use `ON CONFLICT` (typedSql where possible, record API where
  Maybe FK params force it — see MEMORIES typedSql notes) so racing applies
  converge. Strict categories additionally take a `pg_advisory_xact_lock`
  keyed on the category name before reconciling, so two racing strict
  applies can't interleave delete/upsert into a transient violation.

## 4. Users provisioning

- Fields: `email` (key), `displayName`, `passwordHash` (required, pwstore-fast
  format `sha256|17|b64salt|b64hash`), `roles` (list of names; unknown role
  names are auto-created with empty privileges and a startup warning — a typo
  in privileges is worse than a typo'd role name, but both are visible),
  `settings` (jsonb, merged not replaced; theme values validated against
  `Application.Helper.Theme` like the UI does).
- Upsert: `ON CONFLICT (email) DO UPDATE` password_hash/display_name; roles
  reconciled additively (`user_roles` insert-if-missing; provision does not
  strip manually-granted roles).
- Locked users (`locked_at` set) stay locked — provisioning never unlocks.
- `strict: true`: users whose email is not in `items` are deleted
  (user_roles first, then the user row). A user referenced by alert history
  (`acknowledged_by`/`closed_by`, comments, feedback) is FK-blocked → startup
  aborts naming the user; lock them in the UI or drop them from strict scope
  instead.

### D4 — password tool

- New nix script `halemans-gen-password [email]` in `nix/scripts/`
  (`gen-password.sh`), wired in `nix/scripts.nix` next to
  `halemans-hash-password`:
  1. `pwgen -s 24 1` for the plaintext (add `pwgen` to devenv `packages`).
  2. `halemans-hash-password <pw>` (existing pbkdf1 replica) for the hash.
  3. Prints plaintext once, the hash, and a ready-to-paste
     `{"email": ..., "passwordHash": ..., "roles": [...]}` item fragment for
     the `users.items` list.
- Plaintext goes to stdout only (never a file, never the provision file);
  the operator copies it into a password manager.

## 5. Sources provisioning

- Fields: `type` (`zabbix|grafana|alertmanager|webhook`), `name` (key),
  `baseUrl`, `env`, `pollIntervalSeconds`, `enabled`, `config` (jsonb, stored
  verbatim — same free-form keys the fixtures and Sources UI already use:
  `tokenEnv`, `hostGroupScope`, `initialHistoryDays`, `writeBack`,
  `cmdbSpace`, `jiraProject`, `expectedIntervalSeconds`).
- Upsert keyed on `name`; existing `sources` table has no unique index on
  name → phase 1 enforces uniqueness in Haskell (select-then-insert/update
  inside the upsert; cold-boot race between web/worker is handled by
  serializable-retry or a real unique index — decide in implementation, a
  partial unique index `ON sources(name)` is acceptable and cheap).
- Webhook tokens: `webhookTokens` entries carry `tokenEnv` references; the
  referenced variable must be set at provision time (else config error).
  Insert-if-missing on `webhook_tokens.token` (unique already).
- Zabbix host groups: `hostGroupsFile` (zabbix sources only) points at a
  local JSON file — a bare `[{"groupid": ..., "name": ...}]` array or a full
  `hostgroup.get` response dump (`result` wrapper accepted verbatim). On
  apply the source's `zabbix_host_groups` cache is REPLACED with the file
  contents (same semantics as the manual sync button). This is the import
  path for tokens without `hostgroup.get` permission; unreadable or
  malformed files abort startup.
- `strict: true`: sources whose name is not in `items` are deleted
  (webhook_tokens and zabbix_host_groups first). Sources referenced by
  alerts/raw_events are FK-blocked → startup abort; keep them with
  `"enabled": false` in `items` instead (this is exactly the M5 smoke
  cleanup pattern — alerts FK blocks DELETE).
- Provisioning does not enqueue pollers — the existing `EnqueuePollers`
  script stays the mechanism; prod deployments run it once as today
  (documented). The host-group cache is synced by the manual button or
  imported from a local file via `hostGroupsFile` (see above).

## 6. Teams provisioning

- Fields: `name` (key), `description`, `hostGroups` (jsonb array of names —
  strings as today, validated only for shape; group existence against a zabbix
  cache is a runtime concern of PollZabbix, not of provisioning),
  `defaults` (jsonb), `defaultDashboardConfig` (jsonb, same shape the Teams
  UI writes), `members` (`email` + `role`).
- Omitted `description`/`hostGroups`/`defaults`/`defaultDashboardConfig` keys
  leave the DB value untouched on update, so re-provisioning (e.g. container
  restarts) does not clobber UI edits; an explicit key — even `[]` or `""` —
  overwrites. Creation falls back to `""`/`[]`/`{}` when the keys are absent.
- Member emails must resolve — against users provisioned earlier in the same
  file (users are applied first; section order is fixed, not file order) or
  already in the DB. Unresolvable email = startup abort.
- Membership reconciliation is additive with role update on conflict
  (`ON CONFLICT (team_id, user_id) DO UPDATE team_role`); provision never
  removes members.
- `strict: true`: teams whose name is not in `items` are deleted
  (team_members first). Teams referenced by on_call_schedules or
  notification_rules are FK-blocked → startup abort naming the team. Member
  lists WITHIN a kept team are reconciled too: members absent from the
  config entry are removed from that team (this is the one place strict
  reaches below the top-level entity — a team whose membership can't be
  pruned by the config that owns it isn't really reconciled).
- On-call schedules, escalation policies, notification/grouping rules stay
  UI/API-managed (phase 2 candidates).

## 7. LLM config provisioning

- New table `llm_configs` (Schema.sql inline + migration per project
  convention — revision from `date +%s`):
  `provider_name` (unique key), `endpoint`, `model`, `api_key_env` (text
  nullable — env var NAME, not the key), `tools_enabled` bool,
  `enabled` bool, `created_at`, `updated_at`. At most one `enabled` row
  (partial unique index, same pattern as
  `llm_prompt_templates_active_idx`).
- `Application.Service.Llm` gains `llmConfigFromDb` and the resolution order
  becomes: enabled `llm_configs` row → else `llmConfigFromEnv` (unchanged).
  `apiKeyEnv` is resolved with `lookupEnv` at read time. Callers
  (`LlmAnalysisJob`, admin connection test) go through a single
  `currentLlmConfig` helper so the fallback logic lives in one place.
- Prompt templates under the `llm` section are upserted on
  `(name, version)`; activating a provisioned template deactivates the
  previous active one for that name (single-statement update, mirroring the
  admin UI behaviour).
- `strict: true`: `llm_configs` rows whose provider_name is not in `items`
  are deleted (nothing references `llm_configs` — always safe). Prompt
  templates are reconciled per provisioned template NAME: versions of a
  provisioned name absent from the config are deleted unless referenced by
  `llm_analyses.prompt_template_id` (FK-blocked → startup abort); templates
  with names not mentioned in the config at all are left alone (they belong
  to the UI-managed namespace, same posture as non-strict).
- Env fallback keeps the milestone-4 dev/test story (mock-llm env vars,
  `HALEMANS_LLM_BACKOFF_SECONDS`) untouched — checks and smoke that don't
  provision an llm section behave exactly as before.

## 8. Dev fixtures & smoke

- Dev stack: unchanged by default (no `HALEMANS_PROVISION_CONFIG` in
  `env.sh`) — fixtures + `seed-halemans` remain the dev seeding path.
- Smoke: `tests/smoke/run.sh` gains one scenario that writes a small
  provision config into the sandbox (extra user + team + disabled source +
  llm section, all `strict: false`), restarts the app process against it,
  and asserts: login with the generated password works, the
  team/membership/source rows exist, and the llm config row is the one the
  app reports (admin LLM page or direct DB assert). A second pass re-applies
  with `strict: true` on the teams category after removing the extra team
  from the config and asserts the row is gone. `halemans-gen-password`
  itself is exercised here (generated hash → successful login round-trip).
- Keep any new inline seeding in `smoke-check.sh` in sync per the standing
  convention (MEMORIES: both places).

## 9. Testing

- Unit (checks.tests): config JSON parsing — valid fixtures round-trip,
  unknown fields rejected, each required-field-missing error names the path;
  `strict` defaults to `false` and parses both ways; `strict: true` with
  omitted `items` is a config error; env-reference validation; upsert-key
  derivation; llm resolution order (DB row wins, env fallback).
- Integration (checks.integration-tests, temp postgres): apply a full
  provision config twice → identical DB state (idempotency); re-apply with
  changed password_hash/enabled/role → updated in place; unknown team member
  email → error; `llm_configs` row drives `currentLlmConfig`; prompt
  template activation swaps `active`. Strict mode: seed an extra
  team/user/llm_config absent from the config, apply with `strict: true` →
  extra rows deleted, config rows intact; strict delete of an
  alert-referenced user/source → provisioning error, startup aborts.
- Smoke: §8 scenario.
- Canonical gate: `nix flake check --impure`.

## 10. Acceptance checklist (end of Milestone 7)

- [x] Boot with `HALEMANS_PROVISION_CONFIG` unset → current behaviour, byte-identical seeds
- [x] Boot with a full config → users log in, sources poll, teams show members, LLM uses the DB row
- [x] Reboot with the same config → zero row churn (no duplicates, no updated_at noise beyond expected)
- [x] Reboot with edited config, all categories `strict: false` → changed fields updated, removed entities still present (additive-only)
- [x] Reboot with a category `strict: true` and an entity dropped from the config → that row is deleted; FK-referenced rows → startup aborts naming the blocker
- [x] `strict: true` with `"items": []` empties the category; omitted `items` under `strict: true` → config error, no deletions
- [x] `halemans-gen-password` output → paste hash into config → login succeeds
- [x] Malformed JSON / unknown field / missing tokenEnv / unknown member email → startup aborts naming the cause
- [x] No `llm_configs` row → env-based LLM config works exactly as in M4–M6
- [x] `nix flake check --impure` green

## 12. Implementation notes

- Hook: `Config.hs` `configIO provisionAtBoot` resolves `defaultDatabaseUrl`
  itself (the builder runs before `ihpDefaultConfig`) and opens a short-lived
  pool via `withModelContext … noopLogger`. One hook covers web, worker, dev
  server and `run-script`.
- Module: `Application/Service/Provision.hs` — `parseProvisionConfig`
  (strict aeson parsers, unknown keys rejected everywhere) +
  `applyProvisionConfig :: FilePath -> IO ()` under `?modelContext`. Each
  category applies in one transaction behind
  `pg_advisory_xact_lock(hashtextextended(category, 0))`, so racing web/worker
  boots converge. Strict deletes run per row; FK violations are rethrown as
  `ProvisionError` naming the entity and the blocking constraint/table.
- The generated `llm_configs` record took the name `LlmConfig`, so the M4
  service record was renamed `LlmProviderConfig`. DB-first resolution lives
  in `Application/Service/Llm/DbConfig.hs` (separate module: both records
  share field names); `currentLlmConfig` = enabled DB row → else env.
  `LlmAnalysisJob` and the admin connection test go through it; the admin LLM
  page displays the effective config.
- Schema: `llm_configs` table + `llm_configs_enabled_idx` partial unique (one
  enabled row) + `sources_name_idx` unique (upsert key). Inline in Schema.sql
  and migration `1788686328-llm-configs.sql`.
- Users upsert merges settings (`users.settings || EXCLUDED.settings`);
  unknown role names are auto-created with empty privileges plus a startup
  warning. Locked users are never touched (no `locked_at` in the upsert).
- Team upsert uses the record API (nullable `default_dashboard_config`);
  omitted `defaultDashboardConfig` leaves the existing value alone on update.
  Strict teams prunes members of kept teams, the one sub-entity strict reach.
- LLM `enabled: true` upsert first deactivates other rows (single enabled
  invariant); prompt templates insert inactive then activate via the same
  two-statement flip as the admin UI.
- `halemans-gen-password` = `nix/scripts/gen-password.sh` wired as
  `halemansLib.genPassword`; in devenv packages (with `pwgen`) and
  checks.smoke inputs.
- Smoke: run.sh's last scenario is gated on `SMOKE_APP_MANAGED=1` (set only
  by smoke-check.sh, which exports `SMOKE_APP_PID`/`SMOKE_APP_LOG`); it
  restart_apps twice — full non-strict config (login round-trip with the
  gen-password hash, rows asserted, active llm_configs row asserted), then
  strict teams keeping `sre` with its seeded membership and dropping the
  extra team.
- Tests: `Test/ProvisionSpec.hs` (13 parser specs) and `m7Spec` in
  Test/Integration.hs (12 specs: idempotency, in-place updates, settings
  merge, abort paths, DB-first LLM resolution, template activation swap,
  strict delete/prune for teams/llm/users/sources, transactional rollback on
  FK-blocked strict delete). `nix flake check --impure` green.

## 11. Decisions

- **JSON over YAML**: aeson is already in the tree; strict manual parsers give
  loud unknown-field errors that YAML libs make awkward. Operators generate
  the file with `jq`/nix anyway.
- **Provision at process start (Config.hs hook), not a separate script**: the
  requirement is "at app start"; one hook in the shared config builder covers
  web + worker + dev server with no new process to orchestrate, and idempotent
  upserts make the double-run (web and worker boot together) harmless.
- **Per-category `strict` flag, default `false`**: non-strict provisioning is
  a bootstrap/declarative-overlay (adds and updates, never deletes) — safe to
  point at a database that also has UI-managed rows. `strict: true` opts a
  category into full desired-state reconciliation (config = source of truth,
  absent rows deleted), which is what unattended/config-managed deployments
  want. Per-category (not one global flag) because the blast radius differs
  wildly: deleting a stale llm_config is routine, deleting a user referenced
  by two years of alert history is an incident.
- **FK-blocked strict deletes abort startup instead of soft-deleting**:
  silently converting "delete this source" into "disable it" would make the
  DB diverge from the config while claiming reconciliation — the operator
  must choose explicitly between `"enabled": false` in the config and
  cleaning up referencing rows.
- **`strict: true` requires an explicit `items` key**: "delete everything in
  this category" must be written as an empty list on purpose, never produced
  by a templating accident that dropped a key.
- **LLM config moves to DB-with-env-fallback instead of staying env-only**:
  "create database entities according to the configs" requires a table, and a
  DB row makes the provisioned config observable/testable; the env fallback
  preserves every existing dev/test/smoke path, so nothing in M4–M6 regresses.
- **Passwords as pre-hashed values, generated offline**: the provision file
  must not carry plaintext (it will live in config management); reusing the
  pwstore-fast replica (`hash-password.py`) keeps one hash format across seed,
  UI, and provisioning. `pwgen` supplies the plaintext — standard, audited,
  and already packaged in nixpkgs.
- **Strict parsing (reject unknown fields)**: a provisioning file that
  silently ignores a typo'd key is a production incident at 3am; aborting at
  startup is the only acceptable failure mode.
