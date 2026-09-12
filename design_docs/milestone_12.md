# Milestone 12 — code review follow-ups: licensing, CI gating, hygiene

Applies the actionable findings from `halemans-code-review.md` (audit of
master @ `6e78c70`, 2026-09-12). No new product features; this milestone is
legal clarity, CI enforcement, DRY/encapsulation cleanup, and brand/UI
polish. Ordered so the cheap high-impact items land first.

## 1. License: MIT

`Halemans.cabal` declares `license: AllRightsReserved` + `license-file:
LICENSE` but no `LICENSE` file exists. Decision: relicense to **MIT**.

Dependency audit (all direct deps are permissive — nothing copyleft that
would propagate to Halemans' own code):

| Component | License |
| --- | --- |
| ihp, ihp-typed-sql, ihp-pglistener, ihp-hspec | MIT (verified in `/home/pion/work/dev/ihp` cabal files) |
| base, text, aeson, aeson-pretty, vector, wreq, fast-logger, cryptonite, memory, http-types, network-uri, network, base64-bytestring, cmark, diagrams-core/lib/svg, svg-builder, process | BSD-2/BSD-3 |
| lens | BSD-2 |
| wai, warp, http-client, http-client-tls, hspec | MIT |
| Vendored JS (`static/vendor/`): bootstrap 5.3.8, jquery 4.0.0, flatpickr, popper, morphdom, turbolinks, timeago | MIT |
| GHC (toolchain) | BSD-3 |

No GPL/LGPL/AGPL in the link closure, so MIT for our own source is
permissible. Note: the Docker image bundles third-party binaries under
their own licenses (busybox is GPL-2.0, PostgreSQL under the PostgreSQL
license) — this is mere aggregation and does not affect the license of
Halemans' code; record it in the README/license section so downstream
redistributors of the *image* know their obligations.

Work:

- Add `LICENSE` with the standard MIT text (copyright holder: the author).
- `Halemans.cabal`: `license: MIT`, fill in `synopsis`, `description`,
  `copyright`, real `maintainer` (review findings L6, §3.2).
- README: short License section noting MIT for the app source and the
  third-party licenses of the Docker image contents.

## 2. CI gate + git workflow (review priority 1 — highest value)

`.github/workflows/nix-flake-check.yml` is `workflow_dispatch:` only — the
canonical suite never runs automatically.

New branching model (mandatory once CI is on):

- All new changes land on the **`dev` branch**, never tagged there.
- Releases: merge `dev` → `master`; tag `vX.Y.Z` on master **only after
  the full suite is green on the merge**. No direct feature pushes to
  master, no tags on unverified code.
- Tags keep driving the Docker image workflow as today.

Precondition — **flake-free suite**: the gate is only as good as its
signal. Before enabling push/PR triggers, audit the suite for flapping
checks (smoke timing waits, Playwright races, integration tests against
shared state) and fix them — a flaky check is a bug, never something to
retry or skip. Concrete steps:

- Re-run `nix flake check --impure` repeatedly (target: 5 consecutive
  green runs) and investigate every non-deterministic failure.
- Fix known race sources: smoke's poller/wait loops, Playwright
  `wait_for_url` vs. testid waits (Turbolinks), dev-DB vs. sandbox
  differences.

Then:

- Trigger on `push` and `pull_request` to `master` **and** `dev`, with
  `paths-ignore` for `design_docs/**` and `**.md` so doc-only commits
  stay cheap.
- Keep `workflow_dispatch` for manual runs.

## 3. DRY cleanup (review §2, priorities 2–3)

- New `Application/Helper/Json.hs` exporting `stringList :: Value ->
  [Text]`; delete the 5 verbatim copies (`Jira/DbConfig.hs`,
  `Cmdb/DbConfig.hs`, `Llm/AutoAnalyze.hs`,
  `Web/Controller/Integrations.hs`, `Web/View/Integrations/Form.hs`).
- New `Test/Helpers.hs` with `atTime` (+ UTC literal helpers); delete the
  4 copies in the spec modules.
- Extract `Web/View/Blackouts/Form.hs` parameterized on `Maybe Blackout`,
  following the existing `Integrations/Form.hs` / `Dashboards/Form.hs`
  pattern. This kills the 78%-similar New/Edit pair AND the already
  drifted inline `<script>` (Edit lost the auto-select-first-option
  behavior — restore it in the shared form).
- Extract `Web/View/Sources/Form.hs` the same way (58% similar pair).
- Move the three residual inline `<script>` blocks (`Teams/New.hs`, both
  Blackouts views) into `static/app.js` keyed on existing `data-testid`
  hooks; move the three residual inline `style="…"` attributes onto
  `.maxw-*`-style utility classes. Unblocks a future CSP (§8).
- Promote the "Assets bypasses Service.Http because Assets 302s unauth
  paths to a login page" rationale from agent memories into a code
  comment at the import site in `Application/Service/Assets.hs`.
- Fix `configInt`/`configBool` identical bodies in
  `Application/Job/PollZabbix.hs` (one generic `configVal` or honest
  types).

## 4. Style tooling (review §1.3, priority 4)

- Add `fourmolu.yaml` (4-space indent to match current style), run
  fourmolu once over the tree as a single dedicated commit, retire
  stylish-haskell or keep it for import alignment only.
- Add `.hlint.yaml` (tune out noise on first run; IHP idioms like
  `ImplicitParams` triggers may need ignores).
- New derivation in `nix/checks.nix` running `fourmolu --mode check` +
  `hlint` so both are enforced by `nix flake check --impure`.

## 5. Branding surface (review §3, priority 5)

- Navbar: glyph `<img>` (~24px) + wordmark, using existing
  `images/` artwork exported into `static/`; same glyph on the login page.
- `static/manifest.webmanifest` (name, short_name, existing 192/512
  icons — generate 192/512 PNGs from the app-icon masters if missing,
  `theme_color`/`background_color` from the dark pack tokens) linked from
  `Layout.hs metaTags` + `<meta name="theme-color">`.
- Complete `og:*`: `og:description`, `og:image`, Twitter card.
- Delete `static/ihp-welcome-icon.svg` and `Setup.hs` boilerplate if
  unused.

## 6. Typography & accessibility (review §4, priority 6)

- `:root` tokens `--font-sans` / `--font-mono` with self-hosted woff2 in
  `static/vendor/fonts/` (no CDN); plus a small scale `--text-sm/base/lg`;
  migrate scattered `font-size` literals in `app.css` onto the scale.
- `.env-card`: replace fixed `height: 168px; overflow: hidden` with
  `min-height` + content-sized rows (keep the 360px column basis).
- Layout: wrap page content in `<main id="content">`, add a skip-to-content
  link.
- Render the `requirePrivilege` 403 through `defaultLayout` (styled,
  themed, with navbar) instead of raw string HTML (review P9).
- Contrast audit per theme pack: `--text-muted` on `--surface` and
  `.alert-row.suppressed { opacity: 0.5 }` against WCAG AA 4.5:1; adjust
  tokens, not markup.
- Remove the `.text-muted !important` specificity hack if the bridge
  layer makes it redundant.

## 7. Tests (review §5, priority 7)

- Split `Test/Integration.hs` (2,564 LOC) into
  `Test/Integration/{Pipeline,Enrichment,Llm,Api,Provisioning,Dashboards}Spec.hs`
  along existing `describe` boundaries; shared schema-bootstrap in
  `Test/Integration/Setup.hs`. Keep `Test/Integration.hs` as a thin
  runner so existing invocation docs stay valid.
- Privilege-matrix test: table-driven walk of every mutating route
  asserting 403 without the matching privilege (RBAC model makes this
  small).
- Poller single-loop invariant test: two sibling poll jobs, assert the
  younger refuses / reschedule no-ops.
- Live broadcaster registry unit test (`Application/Service/Live.hs`).
- Enable `hpc` in the tests derivation; archive the report in CI.
- Playwright zoom assertion for the card-height fix in §6 (harness
  already re-renders on resize).

## 8. Security headers & housekeeping (review §6, priority 9)

- Security-headers middleware (wai): `Content-Security-Policy` (feasible
  only after §3's inline-script extraction), `X-Content-Type-Options:
  nosniff`, `Referrer-Policy`, `frame-ancestors 'self'`.
- `Makefile`: sync `JS_FILES`/`CSS_FILES` with the versioned vendor paths
  `Layout.hs` actually serves (jquery 4.0.0, bootstrap 5.3.8), or delete
  the bundling variables (finding L4/P8).
- Uncommit `.opencode/` and `.claude/` (add to `.gitignore`, `git rm -r
  --cached`); migrate the durable, non-sensitive knowledge into
  `AGENTS.md` / `design_docs/` first (finding L5/P12).
- Replace `error` in `upsertEnvironment`
  (`Application/Helper/Ingest.hs:239`) with a proper exception type;
  document or remove `headEx` in `Fragments.hs` (finding P3).
- Fix README UID-1000 hardcoding in the process-compose socket path
  (finding L7/P10).

## 9. Explicit export lists (review §1.2 L2, priority 8 — incremental)

Start with `Application/Service/*` (the modules controllers import),
generated mechanically via `ghc -ddump-minimal-imports` / HLS code
actions. Split `Web/View/Fragments.hs` (874 LOC) by widget domain
(alerts table, badges, panels, dashboard widgets). Ongoing; not a gate
for closing this milestone — track as follow-up if it outgrows the
milestone.

## 10. Explicitly out of scope (review §10)

- QuickCheck round-trip properties (cursor encode/decode, fingerprint
  dedupe) — candidate for a later quality milestone.
- Generalizing the Jira/Cmdb `DbConfig` pattern into a shared
  typeclass/skeleton — revisit if a third integration type appears.
- Multi-replica concerns around `unsafePerformIO` state (rate limiter,
  WS registry) — document single-node assumption in
  `design_docs/01_highlevel.md` instead (finding P4).

## 11. Verification & versioning

- Canonical gate: `nix flake check --impure` after each phase; the new
  fourmolu/hlint check derivation (§4) must pass from its introduction
  onward.
- Versioning per project rule: each landing phase bumps the version in
  `Halemans.cabal` + `Application/Version.hs` together (patch for
  fixes/internal hygiene, minor for the user-visible branding/UI
  changes); tag only the final version.
- License change lands as its own commit before any other milestone work
  (clean legal boundary in history).
