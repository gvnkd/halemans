# Halemans — Code Review & Improvement Analysis

**Repository:** [github.com/gvnkd/halemans](https://github.com/gvnkd/halemans) (master @ `6e78c70`, analyzed 2026-09-12)
**Stack:** Haskell + IHP v1.6, PostgreSQL, Nix flakes, server-rendered HSX + WebSocket live updates
**Scope:** ~25,000 lines of Haskell in 208 modules, 58-table schema, 18 migrations, Docker + NixOS deployment

---

## Executive Summary

Halemans is an unusually disciplined project for a small (single-author) codebase: it ships a real RBAC layer, a strictly-typed SQL policy, cursor-based idempotent connectors, an append-only audit log, multi-layer tests (unit → integration → smoke → Playwright in a Nix sandbox), and a thoughtful token-based theming system. The overall verdict is that it **follows industry best practices in architecture and operational hygiene far better than most IHP projects**, but it has clear, actionable weaknesses in four areas:

1. **CI gating** — the canonical `nix flake check` suite is *manual-dispatch only* on GitHub; nothing runs automatically on push/PR. The strongest test suite in the world provides no protection if it isn't run.
2. **DRY violations in the view layer** — New/Edit view pairs duplicate each other (up to 78% similarity, including copy-pasted inline `<script>` blocks), and small helpers like `stringList` are copy-pasted across five modules.
3. **Encapsulation** — 112 of 122 application modules have fully open export lists (`module X where`), and `Web/View/Fragments.hs` has grown into an 874-line grab-bag.
4. **Branding depth & typography** — the project has excellent logo *artwork*, but the running app shows only a plain-text wordmark, there is no PWA manifest, no custom font stack or typographic scale, no CSP, and accessibility basics (landmarks, skip links) are missing.

Everything below is organized as: what the project does well → concrete findings with file references → prioritized recommendations.

---

## 1. Repository Layout & Coding Style

### 1.1 What follows best practice

The layout is a textbook IHP structure, extended sensibly rather than fought against. `Web/{Controller,View,Routes,Types,FrontController}.hs` follows the framework convention exactly; `Application/` is subdivided by *role* (`Connector`, `Pipeline`, `Service`, `Job`, `Helper`, `Script`, `Migration`), which is the recommended IHP layering ([IHP Guide — Architecture](https://ihp.digitallyinduced.com/Guide/architecture.html)). Non-framework concerns live in clearly-named top-level directories: `deploy/docker/` for end-user deployment, `nix/` for custom Nix code (dev shell, mocks, checks, seed scripts), `design_docs/` for the design history (initial sketch, high-level design, eleven milestone notes), `images/` for master artwork, `tests/` for E2E harnesses. Nothing important is ambiguous about where it lives.

Developer-experience files are all present and mutually consistent: `flake.nix` + `flake.lock` pin the entire toolchain via the IHP flake module; `hie.yaml` and `.ghci` make HLS/GHCi work out of the box; `.envrc` integrates direnv; a committed `.stylish-haskell.yaml` enforces import/pragma alignment (80 columns, vertical pragmas); `AGENTS.md` documents project-specific rules (typed-SQL-only policy, migration revision discipline using raw `date +%s`, canonical test command); `.gitignore` correctly excludes secrets (`.env`, `provision.json`, `Config/client_session_key.aes`, `.devenv*`). The `AGENTS.md` migration rule — *never hand-pick revision numbers because IHP silently skips duplicate revisions* — is exactly the kind of hard-won operational knowledge that belongs in writing, and it is written down.

Coding style is consistent throughout: `NoImplicitPrelude` with `IHP.Prelude`, uniform 4-space indentation, consistent naming (`camelCase` functions, `PascalCase` types), extensive and genuinely explanatory comments that reference the motivating design document section (e.g. *"Milestone 7 (D2, design_docs/milestone_7.md §3)"*). Every SQL statement in application code goes through `sqlQueryTyped`/`sqlExecTyped` (`ihp-typed-sql`) — a grep for raw `sqlQuery` returns zero hits in `Application/` and `Web/`, which is a stronger type-safety posture than the IHP default and is explicitly enforced by the project's own policy.

### 1.2 Findings and gaps

| # | Finding | Evidence | Severity |
|---|---|---|---|
| L1 | **No whole-module formatter.** `stylish-haskell` only aligns imports/pragmas; actual code layout is hand-maintained. No `fourmolu`/`ormolu` config, and `hlint` is installed in the dev shell but has **no config file and no CI step** — it is never run automatically. | `.stylish-haskell.yaml` (IHP default, 90% comments); `flake.nix` `devHaskellPackages` lists `hlint` with no `.hlint.yaml` anywhere | Medium |
| L2 | **Open export lists everywhere.** Only 10 of 122 app modules declare explicit export lists (`Application/Helper/Theme.hs`, `Application/Service/Http.hs`, `Application/Connector/Zabbix.hs` are the good examples). The other 112 use `module X where`, re-exporting every internal helper and widening the compile-time coupling surface. | `grep -l "^module.*where$"` → 112 files vs. 10 with explicit lists | Medium |
| L3 | **Missing `LICENSE` file.** `Halemans.cabal` declares `license: AllRightsReserved` and `license-file: LICENSE`, but no `LICENSE` file exists in the repo. Any tooling or packager that follows the cabal reference hits a dead end, and the legal posture is ambiguous for a public GitHub repo. | `Halemans.cabal` vs. `ls LICENSE` → not found | Low–Medium |
| L4 | **Stale stock `Makefile`.** The IHP-generated Makefile is unmodified: it pins `jquery-3.6.0.slim.min.js` and unversioned `bootstrap.min.css`, while `Web/View/Layout.hs` actually loads `jquery-4.0.0` and `bootstrap-5.3.8` paths. If `static.makeBundling` is ever enabled, the production asset bundle will silently ship the wrong library versions. | `Makefile` `JS_FILES`/`CSS_FILES` vs. `Layout.hs` `stylesheets`/`scripts` | Medium |
| L5 | **AI-agent internals committed.** `.opencode/MEMORIES.md` (205 lines) and `.claude/launch.json` are checked in. The memories file leaks local machine paths (`/home/pion/work/dev/ihp`), mock backdoor endpoints (`/debug/fail/500`), and operational internals. Useful for the author's agent sessions; noise — and mild information exposure — for everyone else. | repo root | Low |
| L6 | **Boilerplate leftovers.** `static/ihp-welcome-icon.svg` (IHP welcome-page asset, referenced nowhere), commented-out `synopsis`/`description` and `maintainer: developers@example.com` in the cabal file, `Setup.hs` stub. | `static/`, `Halemans.cabal` | Low |
| L7 | **README portability nit.** The quick-start documents `process-compose -u /run/user/1000/devenv-*/pc.sock`, hard-coding UID 1000. | `README.md` | Low |

### 1.3 Recommendations

- **Adopt `fourmolu`** (the current standard, successor to ormolu, with configurable indentation) with a committed `fourmolu.yaml`, run it once over the tree, and add a `--mode check` step to CI. Retire or keep stylish-haskell only for import alignment. Add a `.hlint.yaml` and a `hlint` step to `nix/checks.nix` — both tools are already in the dev shell, so the marginal cost is near zero ([fourmolu](https://github.com/fourmolu/fourmolu), [hlint](https://github.com/ndmitchell/hlint)).
- **Move to explicit export lists incrementally**, starting with the `Application/Service/*` modules that controllers import. This is the single highest-leverage change for compile-time hygiene and for making each module's public contract reviewable; tools like `ghc -ddump-minimal-imports` and HLS code actions can generate the lists mechanically.
- Either commit an actual `LICENSE` file matching `AllRightsReserved` (a short proprietary notice) or relicense deliberately — the current state is the worst of both.
- Refresh the `Makefile` asset lists to match the versioned vendor paths the layout really uses, or delete the bundling variables entirely and rely on the `assetPath`-based per-file loading that `Layout.hs` already does.
- Move `.opencode/MEMORIES.md` and `.claude/` into `.gitignore` (or extract the non-sensitive, durable knowledge — versioning rules, mock backdoors, the IHP-local-source hint — into `design_docs/` or `AGENTS.md`, which is their proper home).

---

## 2. Code Sharing and Duplication (DRY)

### 2.1 What is shared well

The project demonstrates a real instinct for shared infrastructure rather than ad-hoc copying. **`Application/Service/Http.hs`** is the standout: a single, well-documented redirect-following HTTP wrapper (`getFollowing`/`postFollowing`/`deleteFollowing`, 5-hop cap, full option re-issue per hop) that all connectors route through, because the author discovered that wreq strips `Authorization` on cross-host redirects — the kind of subtle bug that a shared module fixes exactly once. Similarly, **`Application/Helper/Ingest.hs`** centralizes the normalized-event model (`NormalizedEvent`, `SourceStatus`) that every connector (Zabbix JSON-RPC, Grafana unified alerting, Alertmanager webhooks, generic webhooks) produces, so the pipeline (dedupe → blackout → grouping → state machine → notify) is written once against one abstraction. **`Web/View/Fragments.hs`** provides shared HSX widgets (the unified alerts table, LLM analysis blocks, badges) reused across the HTML views *and* the WebSocket live-update path — so the live UI and the initial render can never drift apart visually. Severity normalization is shared (`Grafana` imports `normalizeSeverity` from `Alertmanager` rather than re-implementing it). Config forms for the two structurally-identical integration types (Jira/Confluence) share `Web/View/Integrations/Form.hs`, and the Dashboards/AssetsAdmin New/Edit pairs correctly delegate to shared `Form.hs` modules.

### 2.2 Where DRY breaks down

| Pattern | Instances | Evidence |
|---|---|---|
| `stringList :: Value -> [Text]` — identical one-liner | **5 verbatim copies** | `Application/Service/Jira/DbConfig.hs:50`, `Application/Service/Cmdb/DbConfig.hs:45`, `Application/Service/Llm/AutoAnalyze.hs:58`, `Web/Controller/Integrations.hs:231`, `Web/View/Integrations/Form.hs:70` |
| Test-time helper `atTime` | 4 verbatim copies | `Test/CmdbSpec.hs`, `Test/WriteBackSpec.hs`, `Test/LlmSpec.hs`, `Test/ReconcileSpec.hs` |
| Blackouts New vs. Edit view | **78% similar**, including a duplicated inline `<script>` block (the scope-type/scope-id filter, ~20 lines of JS, with subtly divergent behavior — Edit omits the "select first enabled option" step) | `Web/View/Blackouts/New.hs` vs. `Edit.hs`; `scopeValue` duplicated between both |
| Sources New vs. Edit view | 58% similar, no shared Form module | `Web/View/Sources/` |
| DB-config resolution pattern (fetch enabled rows → resolve token from env → apply per-source scope override) | Near-identical structure in `Jira/DbConfig.hs` and `Cmdb/DbConfig.hs`, including identical error-aggregation logic (`length errs == length configs → Left first error`) | both modules |
| `configInt`/`configBool` | Two functions, identical bodies, differing only in the (unchecked) type annotation | `Application/Job/PollZabbix.hs:338-342` |
| Jira/Confluence icon/avatar fetch + 406 workaround knowledge | Documented in `.opencode/MEMORIES.md`, enforced only by convention | `Application/Service/Assets.hs` uses wreq directly, deliberately not `Service.Http` |

### 2.3 Recommendations

- **Create `Application/Helper/Json.hs`** (or extend `Application.Helper.View`) with `stringList`, and delete all five copies. For tests, add a `Test/Helpers.hs` module with `atTime` and the UTC literal helpers — spec modules already share an import style, so this is a ten-minute change.
- **Extract a shared `Web/View/Blackouts/Form.hs`** parameterized on `Maybe Blackout`, exactly the pattern the project itself already proves works with `Integrations/Form.hs` and `Dashboards/Form.hs`. This eliminates the duplicated inline JavaScript (which is also a CSP liability — see §6) and the drift risk that already materialized: the Edit copy of the script lost the "auto-select first valid option" behavior, a real behavioral divergence introduced by copy-paste.
- **Generalize the `DbConfig` pattern.** `Jira.DbConfig` and `Cmdb.DbConfig` differ only in row type, field names, and the downstream service record. A small typeclass (`class IntegrationConfig row cfg | row -> cfg where scopeOf :: row -> Value; build :: row -> Text -> [Text] -> cfg`) or even just a shared `configsFromDb :: (row -> Maybe cfg) -> IO [cfg]` skeleton would collapse ~80 lines of parallel code and, more importantly, make the *"no enabled rows = silent skip, total failure = Left"* semantics impossible to implement inconsistently.
- Keep `Assets.hs`'s deliberate bypass of `Service.Http`, but promote the rationale from the agent-memories file into a code comment at the import site, where the next maintainer will actually see it.

---

## 3. Project Branding

### 3.1 What exists

The brand assets themselves are genuinely professional and, notably, treated as *engineering artifacts*: a memorable backronymed name (**H**ome **ALE**rt **MAN**agement **S**ystem), a complete master-artwork set in `images/` (light/dark lockups, color/dark-bg/mono glyph variants at 1024px, app icons at 180–512px, favicons at 16–64px), and a README header that switches lockups via `prefers-color-scheme` `<picture>` sources. The web app wires the favicon suite and apple-touch-icon through `assetPath` (cache-busted in production), sets `og:title`, and shows a version badge (`v2.3.0`) next to the wordmark in the navbar. Versioning is disciplined: semver in two places (`Halemans.cabal` + `Application/Version.hs`) with `Test/VersionSpec.hs` failing the suite when they drift, and git tags driving the Docker image tags (`latest`, `sha`, `v*`). The six theme packs are themselves part of the brand identity — Catppuccin Latte/Frappé/Macchiato and Dracula are recognizable, deliberate palette choices rather than defaults.

### 3.2 Gaps

| Finding | Evidence |
|---|---|
| The **running app never shows the logo**. The navbar brand is the plain text "Halemans"; the glyph artwork (which exists in four variants) appears nowhere in the UI — not the navbar, not the login page, not the favicon-sized contexts. | `Web/View/Layout.hs` `navigation` |
| **No PWA manifest.** The project ships a push service worker (`static/push-sw.js`) and app icons at exactly the sizes a manifest wants (192/512), but no `manifest.webmanifest` and no `<meta name="theme-color">`. | `static/`, `Layout.hs metaTags` |
| Minimal social metadata: `og:title` only — no `og:description`, `og:image`, or Twitter card. | `Layout.hs metaTags` |
| Boilerplate undermines the brand surface: `ihp-welcome-icon.svg` still in `static/`; README feature prose is dense and excellent for engineers but there is no screenshot of the actual UI anywhere in the README or design docs. | repo |
| The version badge under the wordmark uses raw version text (`v2.3.0`) but the cabal `synopsis`/`description` fields are commented out — package metadata is incomplete. | `Halemans.cabal` |

### 3.3 Recommendations

- Put the **glyph** in the navbar (`<img>` + wordmark, ~24px) and on the login page — the single highest-visibility branding change, using artwork that already exists.
- Add `static/manifest.webmanifest` (name, short_name, the 192/512 icons that already exist, `theme_color`/`background_color` from the dark pack tokens) and link it plus `<meta name="theme-color">` from `metaTags`. This also fixes mobile "add to home screen" presentation, which the push-notification feature implies is a real use case.
- Complete `og:*` metadata, delete `ihp-welcome-icon.svg`, fill in cabal `synopsis`/`description`, and add one or two UI screenshots to the README — for an alert-management UI, a screenshot of the dark-themed dashboard communicates the brand faster than the feature list.

---

## 4. WebUI Theming & Typography

### 4.1 Theming — mostly exemplary

The theming architecture is the best part of the frontend. **Every used color is a CSS custom property** defined per theme pack (six packs: `latte`, `frappe`, `macchiato`, `dracula`, `light`, `dark`), with a semantic token model (`--bg/--surface/--border/--text/--text-muted/--accent` plus a full `--severity-*` and `--status-*` set) switched via `data-theme` on `<html>` and persisted per-user in `users.settings.theme`. A **bridge layer** maps the tokens onto Bootstrap's own variables (`--bs-body-bg`, `--bs-link-color`, `--bs-table-*`, navbar variables), so stock components follow the active pack instead of fighting it — including the subtle `bsTheme` mapping that sets Bootstrap 5.3's `data-bs-theme` color mode so placeholders and muted text don't render light-on-dark. Third-party widgets are handled: flatpickr's light-only skin is fully re-themed, and the server-generated SVG report charts (`diagrams-svg`) carry class names that CSS re-maps onto the tokens, so charts follow the theme like everything else. Theme validation is centralized and unit-tested (`Application/Helper/Theme.hs`, `Test/ThemeSpec.hs`). Per-component CSS (`.maxw-400..700` utility classes) has already replaced most per-view inline widths, with a comment documenting that intent.

### 4.2 Typography and UI issues

| Finding | Detail | Evidence |
|---|---|---|
| **No font stack at all** | Zero `font-family`/`@font-face` declarations anywhere; the UI inherits Bootstrap's default system stack. Readable, but unbranded and inconsistent across platforms (Segoe UI on Windows, San Francisco on macOS, Roboto on Android); monospace contexts (JSON viewer, LLM markdown, dashboard config) use Bootstrap's `font-monospace` with no curated mono face. | `grep font-family static/` → 0 hits |
| **No typographic scale** | Headings are bare `<h1>` per page with Bootstrap defaults; ad-hoc sizes scattered as utilities (`font-size: 0.65rem` badge, `0.75rem` hourly buckets, `0.8rem` uppercase labels with `letter-spacing`). The uppercase/letter-spaced `dt` pattern is a good micro-style, but it is the *only* intentional typographic device. | `static/app.css` |
| **Fixed-geometry cards** | `.env-card { height: 168px; overflow: hidden }` and `flex: 0 0 360px` tiles: content that grows (long env names, more hourly buckets) is silently clipped rather than reflowed; 200%+ browser zoom truncates card content. The comment says "zoom changes the column count" — true for columns, but the fixed height still clips. | `static/app.css` |
| **Residual inline styles & scripts** | Three views still carry `style="..."` attributes (`Dashboards/Form.hs`, `Dashboards/Index.hs`, `Teams/New.hs`) and three views embed raw `<script>` blocks (`Teams/New.hs`, both Blackouts views) — hostile to any future CSP and inconsistent with the project's own token discipline. | §2.2 table |
| **Accessibility gaps** | No `<main>` landmark (body is `<nav>` + bare `<div class="container-fluid">`), no skip-to-content link, `.alert-row.suppressed { opacity: 0.5 }` almost certainly fails WCAG contrast on muted text, and tables rely on default markup without `scope` attributes. The 403 page from `requirePrivilege` is an unstyled raw HTML string with no layout, no navbar, and no theme. | `Layout.hs`, `Application/Helper/Controller.hs` |
| **`!important` in the token layer** | `.text-muted { color: var(--text-muted) !important; }` works but signals a specificity fight with Bootstrap that the bridge layer otherwise avoids cleanly. | `static/app.css` |

### 4.3 Recommendations

- **Define a brand font stack in `:root` tokens** (`--font-sans`, `--font-mono` — e.g. Inter/IBM Plex Sans + JetBrains Mono, self-hosted woff2 in `static/` to avoid a CDN dependency and GDPR-relevant third-party requests), plus a two- or three-step typographic scale (`--text-sm`, `--text-base`, `--text-lg`) and migrate the scattered `font-size` literals onto it. This is a one-file change in `app.css` with immediate visual payoff.
- Replace the fixed `168px` card height with `min-height` + `grid-auto-rows` or content-sized cards, keeping the 360px column basis; add a Playwright zoom-level assertion (the suite already re-renders on window resize, so the harness exists).
- Move the three residual inline scripts into `static/app.js` keyed on `data-testid` hooks (already present), enabling a future **Content-Security-Policy** header; move the three inline styles onto `.maxw-*`-style utility classes.
- Accessibility pass: wrap page content in `<main id="content">`, add a skip link, give the 403 response the real layout (render it through `defaultLayout` instead of a raw string), and check `--text-muted`-on-`--surface` and `opacity: 0.5` rows against WCAG AA (4.5:1) per theme pack — the token architecture makes this a six-line-per-pack audit rather than a redesign.

---

## 5. Test Suite Coverage

### 5.1 What exists — genuinely strong

The testing pyramid is broader than most production Haskell services. **Unit layer:** 29 hspec spec modules (~2,700 LOC) covering the pipeline state machine, grouping, blackouts, escalation, push, connectors (Alertmanager parsing, Zabbix polling), CMDB, Jira, write-back, reconciliation, dashboards, themes, LLM, source health, audit export, the public API, rate limiting, provisioning, facets, filter prefs, markdown rendering, timeline, flapping, reports — including the delightful `VersionSpec` that guards the cabal/version-module drift. **Integration layer:** `Test/Integration.hs` boots a real PostgreSQL and runs **101 `it` blocks** against the live schema: the full alert lifecycle (fire → ack → resolve → stall → auto-close), dedupe races, escalation trackers, enrichment with soft-fail semantics, write-back retry/terminal-failure, LLM job stale-recovery, provisioning idempotency. **System layer:** `nix/checks.nix` builds an *isolated full stack inside the Nix sandbox* — Postgres, the prod binaries, a jobs worker, and real nixpkgs builds of Zabbix, Grafana and Alertmanager — then runs a shell smoke suite and a 1,224-line **Playwright** harness (44 named checks) against it. Python **mocks** (`nix/mocks/`) for Confluence/Jira/LLM/Assets come with failure backdoors (`/debug/fail/429`) so retry and backoff paths are tested deterministically. `AGENTS.md` establishes `nix flake check --impure` as the single canonical gate. This is, frankly, better coverage infrastructure than many commercial products.

### 5.2 But: it is not "fully covered", and the gate is off

| Finding | Evidence | Severity |
|---|---|---|
| **CI never runs the suite automatically.** `.github/workflows/nix-flake-check.yml` is `workflow_dispatch:` **only** — explicitly *"Disabled on push/PR"*. The docker workflow runs on tags and builds but does not test. Every merge to master is unverified unless someone remembers to click "Run workflow". | `.github/workflows/nix-flake-check.yml` header comment | **High** |
| **The integration suite is one 2,564-line file.** `Test/Integration.hs` imports 40+ application modules and mixes pipeline, LLM, API, provisioning, and dashboard tests in a single `main`. Compile times, failure localization, and merge conflicts all scale with file size; the unit specs already prove the project knows the one-module-per-concern pattern. | `Test/Integration.hs` | Medium |
| **Coverage gaps at the module level.** No spec touches `Application/Service/Live.hs` (the WebSocket registry/broadcaster with `unsafePerformIO` global state — arguably the riskiest module in the tree), `PollerControl` (the single-loop invariant that prevents double-ingest is documented as load-bearing but only indirectly exercised), `Application/Service/Notify.hs` beyond `currentOnCall` stubs, and most `Web/View/*` render paths (mitigated by Playwright, but view logic like `computeEnvCards` is only hit via the DB-backed integration file). View/controller authorization matrices (privilege × endpoint) have no systematic test — 50 mutating actions are protected by convention, verified by eyeball. | grep across `Test/` | Medium |
| **No coverage measurement.** No `hpc` instrumentation or report anywhere, so "fully covered" is unmeasurable and the gaps above are invisible in the tooling. | repo | Low |
| **No property-based testing.** Pure, total functions with rich edge cases (`decodeCursor`, grouping-rule matching, facet extraction, severity normalization, `parseCompletionOutput`) are unit-tested with hand-picked examples only; QuickCheck-style properties (round-trip cursor encode/decode, dedupe idempotency) would fit naturally ([QuickCheck](https://hackage.haskell.org/package/QuickCheck)). | `Test/*Spec.hs` | Low |

**Verdict: not "fully covered" — but the skeleton of full coverage exists.** The honest assessment is *broad coverage of domain logic, thin coverage of the web layer and the riskiest concurrency code, and no CI enforcement of any of it*.

### 5.3 Recommendations

1. **Turn the gate on** — change `nix-flake-check.yml` to run on `push`/`pull_request` to master (the workflow already uses `nothing-but-nix` and cachix; runtime cost is the only trade-off, and a `paths-ignore` for `design_docs/**`/`**.md` keeps doc-only commits cheap). This single YAML edit is the highest-value change in this entire report.
2. **Split `Test/Integration.hs`** into `Test/Integration/{Pipeline,Enrichment,Llm,Api,Provisioning,Dashboards}Spec.hs` along the existing `describe` boundaries, with the schema-bootstrap logic in a shared `Test/Integration/Setup.hs`.
3. Add targeted tests for the documented load-bearing invariants: the poller single-loop guard (two sibling jobs, assert one refuses), the Live broadcaster registry, and a privilege-matrix test that walks every mutating route and asserts 403 without the matching privilege — the RBAC data model makes this a table-driven test of a few dozen lines.
4. Enable `hpc` in the tests derivation and archive the report as a CI artifact; add QuickCheck round-trip properties for cursor encoding and fingerprint dedupe.

---

## 6. Common Pitfalls & Mistakes Found

These are the concrete bugs, smells, and near-misses a maintainer should fix or at least know about. Several are already *documented* in `AGENTS.md`/`.opencode/MEMORIES.md` — which is good — but a pitfall documented in an agent-memories file is a pitfall waiting for a human to rediscover it.

| # | Pitfall | Location | Why it matters |
|---|---|---|---|
| P1 | **Test suite not wired into CI** (details §5.2) | `.github/workflows/` | Regression protection is purely manual |
| P2 | **Duplicated inline JS with drifted behavior** — Blackouts New vs. Edit scope-filter scripts differ (Edit lost the auto-select-first-option step) | `Web/View/Blackouts/{New,Edit}.hs` | Already-diverged copy-paste; breaks under any CSP |
| P3 | **`error` in production paths** — `upsertEnvironment` calls `error` if `INSERT ... RETURNING` yields no row (effectively unreachable, but a crash instead of an exception type a caller can handle); `headEx [] = error` in `Fragments.hs` guards LLM rendering by convention | `Application/Helper/Ingest.hs:239`, `Web/View/Fragments.hs:556` | Uncatchable-by-type failures in a codebase that is otherwise disciplined about `Either Text` |
| P4 | **`unsafePerformIO` global mutable state** (HTTP manager, WS registry, rate-limit buckets). All correctly carry `{-# NOINLINE #-}` — *good* — but the in-memory rate limiter and live registry silently reset on restart and never scale beyond one process; the single-node design makes this acceptable, yet it is undocumented in `design_docs/` and invisible to operators | `Application/Service/{Push,Api/RateLimit,Live}.hs` | Operational surprise; also the first thing to break if the app ever goes multi-replica |
| P5 | **IHP's safe `head` is load-bearing.** Dozens of `head` calls rely on `IHP.Prelude`'s nonstandard `head :: [a] -> Maybe a`. Fine inside the project, but a trap for any contributor pattern-matching on stock-Prelude semantics, and it makes some call sites (`fromMaybe "" (head projects)`) silently produce empty-string configs on malformed data | ~20 sites, e.g. `Jira/DbConfig.hs:45` | Latent misconfiguration: an empty project list becomes `project = ""` and a JQL clause against an empty project |
| P6 | **Migration ledger vs. folded schema risk, mitigated.** `Schema.sql` folds migrations and `flake.nix` synthesizes a `schema_migrations` ledger so fresh deploys don't re-run them; upgrades apply pending migrations via `db-migrate.sh`. The AGENTS.md duplicate-revision warning proves this was hit in practice. Non-idempotent migrations (plain `ALTER TABLE ... ADD COLUMN`, no `IF NOT EXISTS`) mean a ledger drift is fatal, not retriable. | `flake.nix` dbInit, `Application/Migration/*` | High blast radius when it breaks; currently correct |
| P7 | **Version defined in two places** — mitigated by `VersionSpec`, but the mitigation itself reads `Halemans.cabal` from CWD, so it only works when tests run from the repo root (true in the Nix check, fragile in ad-hoc GHCi from another directory) | `Application/Version.hs`, `Test/VersionSpec.hs` | Minor fragility in an otherwise neat guard |
| P8 | **Stale Makefile asset pins** (jquery 3.6.0 vs. the 4.0.0 actually served) — §1.2 L4 | `Makefile` | Wrong libraries if bundling is ever enabled |
| P9 | **403 responses bypass the layout** — unstyled, unthemed, no navigation, raw string concatenation | `Application/Helper/Controller.hs` | UX + the only place user-controlled-ish text is interpolated into raw HTML (mitigated: `privilege` is a hardcoded string at every call site, so no XSS today — but the pattern invites misuse) |
| P10 | **README/ops hardcoding** — UID 1000 in the process-compose socket path; `POSTGRES_PASSWORD=`/`IHP_SESSION_SECRET=` empty defaults in `.env.example` (good: forces explicit config; risk: compose fails with a bare error rather than a helpful message) | `README.md`, `deploy/docker/.env.example` | Onboarding friction |
| P11 | **No security headers at all** — no CSP, `X-Content-Type-Options`, `Referrer-Policy`, or `frame-ancestors` middleware; combined with inline `<script>` blocks (P2), adding CSP later requires the refactoring from §4.3 | (absent) | Defense-in-depth gap; IHP's built-in CSRF protection covers forms, but headers are free |
| P12 | **Committed agent memories expose mock backdoors and local paths** — §1.2 L5 | `.opencode/MEMORIES.md` | Information hygiene |

A note on what was *checked and found healthy*, because pitfall reviews usually omit it: parameterized-everything SQL with a typed-SQL-only policy (zero string interpolation in queries), consistent `requirePrivilege` guards across all 50 mutating actions (the three controllers without them — Dashboards, Profile, Sessions — correctly guard per-user resources by ownership instead), secrets strictly by env-var indirection (`tokenEnv` stores the *name* of the env var, never the secret, in the DB), negative caching for CMDB misses, backoff + terminal-failure semantics on write-back retries, and the poller single-loop invariant designed against double-ingest. The mistakes that remain are mostly *consistency* mistakes, not competence mistakes.

---

## 7. Prioritized Improvement Roadmap

| Priority | Action | Effort | Impact |
|---|---|---|---|
| 1 | Enable `nix-flake-check.yml` on push/PR (remove `workflow_dispatch`-only gating, add `paths-ignore` for docs) | 15 min | The entire test suite starts actually protecting the repo |
| 2 | Extract `Blackouts/Form.hs` + move inline scripts to `app.js`; delete the drifted duplicate JS | Half a day | Kills the worst duplication; unblocks CSP |
| 3 | Create `Application/Helper/Json.hs` (`stringList`) and `Test/Helpers.hs` (`atTime`); delete 9 copies | 1 hour | DRY hygiene, sets the pattern |
| 4 | Add fourmolu + hlint configs and a check step in `nix/checks.nix` | Half a day | Permanent style enforcement, zero ongoing cost |
| 5 | Brand surfacing: glyph in navbar + login page, `manifest.webmanifest`, `theme-color`, `og:*` completion | Half a day | The app starts looking like its own artwork |
| 6 | Typography tokens (`--font-*`, size scale), fix fixed-height cards, `<main>`/skip-link/layout-rendered 403 | 1–2 days | Real UI polish + accessibility floor |
| 7 | Split `Test/Integration.hs`; add privilege-matrix and poller-invariant tests; hpc coverage artifact | 2–3 days | Makes "fully covered" measurable and approachable |
| 8 | Explicit export lists, starting with `Application/Service/*`; split `Web/View/Fragments.hs` (874 LOC) by widget domain | Ongoing, mechanical | Encapsulation, faster recompiles, reviewable APIs |
| 9 | Housekeeping: `LICENSE` file, Makefile asset pins, remove `ihp-welcome-icon.svg`, uncommit `.opencode/`+`.claude/`, security-headers middleware | Half a day | Legal clarity + hygiene + defense-in-depth |
| 10 | Optional: QuickCheck round-trips (cursor, fingerprint dedupe); generalize the Jira/Cmdb `DbConfig` pattern into a shared skeleton | 1–2 days | Long-tail quality |

---

*Methodology: full source audit of the repository at commit `6e78c70` (2026-09-12) — static reading of all configuration, build, and test infrastructure; targeted greps for duplication, partial functions, authorization coverage, and security surfaces; quantitative similarity analysis of New/Edit view pairs; GitHub API metadata. No code was executed; test-count and coverage statements derive from static inspection of the suite, not from a local `nix flake check` run.*
