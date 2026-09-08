# Milestone 9 — Resolved facets, field overrides, facet dashboards

Goal: make enrichment output (Jira Assets attributes today, other plugin
sources later) a first-class, queryable dimension of an alert. Introduce
**resolved facets** — logical alert properties (`env`, `service`, `team`,
  `location`, any Assets attribute) computed from an ordered override chain —
materialized on the alert row, and an extended **dashboard JSON schema**
that filters and groups cards by any facet. (External tooling — a KCL-based
renderer — will generate this JSON later; Halemans itself keeps plain JSON
as the only dashboard definition format.)

Motivating cases:

- The `env` a Zabbix source reports is the *Zabbix service's* environment;
  the monitored hosts span test/stage/prod. The real environment comes from
  Assets (`Environments:PROD`). Facet override: `env := attr:Environments,
  fallback field:env`.
- A DBA dashboard filters `Service = PostgreSQL` and renders one card per
  `DB Cluster` value (`ibstaffcopdb01`, ...), both Assets attributes.

Builds on milestone 8: `assets_objects.attributes` jsonb is the attribute
source, `asset_alert_links` the alert↔object linkage, `EnrichAlertJob` the
computation point. Dashboards keep the M3 storage (`dashboards.config`
jsonb, raw-textarea editing).

Deferred (NOT in this milestone): dedup by facet (fingerprint identity
change, needs its own design), per-source mapping scopes, CMDB/Jira as
  facet sources (Assets only), a form-based dashboard editor (JSON stays the
  authoring surface).

## 1. Deliverables

| # | Deliverable | Acceptance |
|---|---|---|
| D1 | Schema delta: `alerts.facets` jsonb + GIN index, `field_mappings` table | Migrations apply clean; Types regenerated |
| D2 | Facet resolution service (`Application/Service/Facets.hs`) + admin CRUD for mappings | Override `env ← attr:Environments` takes effect on enriched alerts; fallback to alert field when attribute absent |
| D3 | Facet materialization at ingest (field/label mappings) and in `EnrichAlertJob` (attr mappings); recompute on manual assets refresh | New alert carries facets pre-enrichment; post-enrichment update overwrites with resolved values |
| D4 | Card JSON schema v2 (`Application/Helper/DashboardConfig.hs`): facet `match` clauses + `groupBy`, backward-compatible with M3 `{env, filters}` cards | Old dashboards parse unchanged; invalid configs rejected with precise errors |
| D5 | Grouped dashboard rendering: `groupBy` card renders one section per distinct facet value | `Service=PostgreSQL` + `group by attr:"DB Cluster"` shows a card per cluster |
| D6 | WS live updates for facet cards | Firing alert matching a card's filter live-updates the dashboard |
| D7 | Grouping rules accept `{facet:name}` in `group_key_template` and facet globs in `match`; regroup replay after enrichment | Rule keyed on `{facet:DB Cluster}` groups alerts once enrichment lands |
| D8 | mock-assets dataset + seeds extended (Service/DB Cluster/Environments/Team/Location attributes); unit + integration + smoke green | `nix flake check --impure` green |

## 2. Schema delta

- `alerts` gains `facets JSONB NOT NULL DEFAULT '{}'` — flattened
  facet-name → value map (e.g. `{"env":"PROD","service":"ETCD","team":"IT:RnD:DBA","location":"LV","DB Cluster":"ibstaffcopdb01"}`).
  Keys are the facet names from `field_mappings` plus every whitelisted
  Assets attribute (see §3). GIN index `alerts_facets_idx` (jsonb_path_ops
  covers `facets @> {...}` equality probes; GROUP BY scans stay seq at our
  scale — revisit with an expression index if a hot facet emerges).
- `field_mappings` — facet TEXT, rank INT (resolution order, lower wins),
  kind TEXT (`field` | `label` | `attr`), key TEXT (alert field name /
  `alerts.labels` key / Assets attribute name), enabled bool,
  created/updated. Unique (facet, rank). Global for the whole instance —
  per-source scoping is deferred (see §8).

Seed mappings (passthrough, rank 100): `env ← field:env`,
`service ← field:service`, `host ← field:host`.

## 3. Facet resolution (`Application/Service/Facets.hs`)

Pure core + thin DB shell, same shape as `Application/Pipeline/Grouping.hs`
(pure module never touches the DB, stays unit-testable).

- `resolveFacets :: [FieldMapping] -> [(AssetsObject)] -> Alert -> [(Text, Text)]`
  — for each facet, walk its mappings by ascending rank; first kind that
  yields a non-empty value wins:
  - `field:<name>` → alert column (closed enum, same as `AlertField`).
  - `label:<name>` → `alerts.labels`.
  - `attr:<name>` → `attributes->>'<name>'` of the first linked object
    (link `created_at` order) that has the attribute. Multi-object
    ambiguity is first-wins, documented; multi-valued attributes are
    already `", "`-joined display strings by `flattenAttributes`.
- Additionally every attribute named in any enabled config's
  `attribute_names` is copied verbatim as a facet (key = attribute name) so
  the card schema can group/filter by `DB Cluster` etc. without a mapping
  row.
  Mapping-derived facets win on key collision.
- Materialization points:
  - **Ingest** (`Application/Helper/Ingest.hs`): resolve with an empty
    object list — field/label mappings already work, attr facets land as
    absent. Dashboards are correct (if possibly coarse) immediately.
  - **EnrichAlertJob** (after the assets step) and **RefreshAssetsAction**:
    re-resolve with linked objects, `set #facets`, publish the existing
    `enriched`/`assets` WS kinds — no new kind needed.
  - Mapping edits do NOT retro-update existing alerts in v1; an admin
    "recompute facets" button enqueues a bounded backfill job (chunked
    UPDATE over non-closed alerts).

## 4. Dashboard card JSON schema (v2)

JSON stays the only dashboard definition format (`dashboards.config`,
unchanged table, raw-textarea editing as today). No homemade DSL: an
external KCL renderer will generate this JSON, so the schema must be
stable, self-describing and forward-tolerant — unknown keys are preserved
on round-trip, decoders ignore what they don't understand (same tolerance
as `matchExprFromJSON`).

Card schema v2:

```json
{ "title": "PostgreSQL clusters",
  "match": [ {"facet": "attr:Service", "op": "=", "value": "PostgreSQL"},
             {"facet": "field:severity", "op": "in", "values": ["critical","high"]} ],
  "groupBy": "attr:DB Cluster",
  "limit": 50 }
```

- `match` — array of clauses, conjunction (consistent with grouping
  rules, milestone_2.md §13). Clause ops: `=`, `!=`, `~` (glob,
  `Grouping.globMatch`), `in` (array `values`). Facet references:
  `field:<name>` (alert column, closed enum), `label:<name>`
  (`alerts.labels`), `attr:<name>` (resolved Assets attribute).
- `groupBy` — at most one facet reference; absent = flat card (today's
  rendering).
- `limit` — per-group cap when grouped, global cap otherwise; default 100.
- Non-closed status filter stays implicit.

`DashboardCard` (Application/Helper/DashboardConfig.hs:18) is replaced by
this AST. Backward compatibility: legacy `{"env": ..., "filters": {...}}`
cards decode into the same AST (`env` → `field:env = X`, status/severity →
`in` clauses); encoding keeps the legacy shape for cards that use nothing
new so old rows round-trip byte-identically. Validation errors name the
card index and the offending key.

## 5. Query + rendering

- `cardQuery` (Web/Controller/Dashboards.hs:152) is replaced by a
  `runCardQuery :: CardAst -> IO [Alert]` compiled to a single-table
  query over `alerts` — every facet reference becomes `facets->>'name'`
  (the materialized map already encodes the override chain, so queries
  never join `asset_alert_links`). `field:` facets of fields that exist as
  columns may compile to the column directly; correctness identical, it's
  an index-use detail.
- `groupBy`: `SELECT facets->>'DB Cluster' AS g, ... GROUP BY g` for the
  section headers (count, worst severity via `severityRank`), then the
  alert list per group (`LIMIT` per group, not global). Alerts with the
  group facet absent land in a `-` section (same convention as
  `renderTemplate`'s missing placeholder).
- Rendering: `Web/View/Dashboards/Show.hs` renders one section per group
  value, sections ordered by worst severity then count desc. Flat cards
  keep today's rendering.
- Teams' `default_dashboard_config` accepts the same AST (template copy
  path unchanged).

## 6. Live updates

Dashboard pages currently subscribe one `env:<name>` scope per card
(Live.hs:76-79). Facet cards can't be keyed by env. Approach:

- `Live` event payload for alert updates gains the alert's `facets` map
  (read at broadcast time; the row is already fetched for other kinds).
- New scope `ScopeCard` is NOT added; instead `ScopeDashboard` connections
  gain server-side card evaluation: for each alert event, the broadcaster
  evaluates each card's `match` predicate (pure function over the alert
  row, same code path as D5's compiler but in Haskell) against the
  viewing user's dashboard cards; matching cards re-render their fragment
  (group section targeted by `data-card-id` + group value in the DOM id,
  morphdom handles the diff).
- Card membership is recomputed per event, so an alert whose facets change
  at enrichment time naturally moves between group sections on the next
  event (the `enriched` kind itself triggers re-evaluation).

## 7. Grouping rules over facets (D7)

- `Application/Pipeline/Grouping.hs`: `MatchExpr` gains
  `meFacetGlobs :: [(Text, Text)]` (jsonb key `facets`, same glob engine);
  `renderTemplate` placeholders gain `{facet:name}`. Both read
  `alerts.facets` — no new resolution at group time.
- Timing: `assignGroup` runs at ingest when attr facets are still absent.
  Rules that reference facets declare it implicitly (parser detects
  `facet:` references); `EnrichAlertJob`, after materializing facets,
  calls a new `regroupAlert` for alerts matched by such rules — reuses the
  existing `grouped_by_version` replay machinery (milestone_2.md §13) so
  group moves emit the same events/WS payloads as rule edits.
- Dedup stays fingerprint-based; facet-aware dedup is explicitly deferred
  (changes alert identity semantics).

## 8. Decisions

- **Materialized facets, not read-time joins.** Read-time resolution would
  need a join per facet reference and can't index per-alert values;
  materialization makes dashboards, grouping, the alert list and the
  public API one-table consumers, at the cost of a staleness window
  between ingest and enrichment (closed by ingest-time field/label
  resolution + the `enriched` WS re-render).
- **Global mappings v1.** Per-source override scopes (e.g. "only for
  zabbix sources") are a `field_mappings.source_id` nullable column away,
  deferred until a second conflicting source exists.
- **JSON-only dashboard definitions.** No homemade DSL — the card schema is
  designed for machine generation (a KCL renderer is planned externally);
  the existing textarea UI and provisioning path stay. Schema stability is
  therefore a compatibility contract: additions only, tolerant decoders.
- **First-wins for multi-object attributes.** An alert linked to several
  assets objects takes the facet value from the earliest-linked object
  carrying the attribute. Documented; a per-facet aggregation strategy
  (join/unique-list) can be added to `field_mappings` later.
- **Conjunction-only filters**, consistent with grouping rules
  (milestone_2.md §13). Disjunction is expressible as multiple cards.

## 9. Testing

- Unit: facet resolution precedence/fallback; card schema decode/encode
  round-trip, legacy-config decoding, rejection of malformed clauses;
  glob reuse.
- Integration: ingest → enrich with mock-assets object carrying
  `Environments/Service/DB Cluster` → `alerts.facets` populated; override
  mapping beats the zabbix-source env; grouped card query returns correct
  sections; regroup after enrichment moves an alert between groups.
- Smoke/Playwright: mock-assets dataset gains the Sergey-case attributes
  for `dev-host-01`; a seeded v2-JSON dashboard renders a per-cluster card;
  firing a matching alert live-updates the section. Reminder: new seeded
  rows go in BOTH seed-halemans and smoke-check.sh inline seeding.

## 10. Acceptance checklist

- [x] `alerts.facets` populated at ingest and post-enrichment; override
      `env ← attr:Environments` visibly beats the source env on the alert
      card and in queries
- [x] v2-JSON dashboard: `attr:Service = PostgreSQL` + `groupBy:
      attr:DB Cluster` renders one card per cluster value
- [x] Legacy M3 dashboards render unchanged
- [x] Facet card live-updates via WS on new matching alert
- [x] Grouping rule with `{facet:DB Cluster}` groups enriched alerts
- [x] `nix flake check --impure` green

## 11. Implementation notes (v1.14.0)

- **No regroup machinery existed** despite §7's assumption —
  `Application.Service.Groups.regroupAlert` was built from scratch:
  no-op unless an enabled rule references facets (facet globs in match or
  `{facet:}` in the template, detected by `Grouping.ruleReferencesFacets`);
  re-evaluates all enabled rules first-match-wins, moves the alert
  (rollups recomputed on old + new group), never ungroups (consistent with
  "rule edits don't ungroup"). Conservative on purpose.
- **Live event payload did NOT gain a facets map** (§6): the
  `ScopeUserDashboard` broadcaster branch re-fetches the alert row anyway
  (facets included), so extending the pg_notify payload was dead weight.
- Card facet refs compile as: `field:` → the raw column, `label:` →
  `labels->>key`, `attr:` → `facets->>name` (the resolved override chain).
  So `field:env` deliberately bypasses an `env ← attr:...` override;
  `attr:env` reads the resolved facet. `~` compiles to LIKE via
  `globToLike` (`*`↔`%`, `?`↔`_`, LIKE specials escaped) — semantically
  identical to `Grouping.globMatch` used by the Haskell-side WS matcher.
- `!=` requires a non-null value on both paths (SQL: `IS NOT NULL AND <>`,
  Haskell: `Nothing` never matches) — absent facets don't satisfy `!=`.
- Grouped cards group in Haskell after a full match-fetch (no GROUP BY in
  SQL): facet keys are dynamic so typedSql can't express it; per-group
  `limit` applies post-grouping, sections sort by worst severity then count
  desc, absent groupBy facet lands in `-`. Seq-scan cost accepted per §2.
- `field_mappings` unique key is (facet, rank); admin CRUD at
  /admin/field-mappings (privilege manage_rules) + "Recompute facets"
  button enqueuing `FacetBackfillJob` (chunk 500, cursor-chained over
  non-closed alerts, queuePollInterval 5s).
- Dashboard card DOM ids: legacy cards keep `dashboard-card-<env>` (M3
  Playwright depends on it), v2 cards use `dashboard-card-<index>`; group
  sections `dashboard-card-<i>-group-<value>` (spaces → `_`). The Show page
  subscribes a single `dash:<uuid>` WS scope; the broadcaster re-renders
  whole card sections (`replace`) for cards whose match the event's alert
  passes, on any alert event kind (so `enriched` re-evaluates membership).
- Legacy card JSON round-trips byte-identically only when decoded from the
  legacy shape AND still structurally legacy (`cardLegacy` flag +
  `legacyCardValue` guard); unknown keys survive via `cardExtras`.
- Seeds (BOTH seed-halemans.sh and smoke-check.sh): passthrough mappings
  env/service/host ← field: rank 100, assets_configs.attribute_names
  widened with Service/DB Cluster/Environments/Team/Location (+ UPDATE for
  pre-existing dev rows), 'DBA clusters' v2 dashboard for sre@dev — the
  dashboards INSERT must run AFTER user creation in both files.
- mock-assets dev-host-01/dev-db-01 gained Service/DB Cluster/
  Environments/Team/Location attributes; integration `ensureAssetsConfig`
  sets the widened attribute_names explicitly and refreshes stale rows.
- Integration m9Spec is dev-DB tolerant: `ensureMapping` reuses seeded
  mapping rows (unique (facet,rank) collisions), regroup test sidelines
  the seeded catch-all `env+host` rule for its duration.
- Smoke flake observed once: "source health: dashboard count never moved"
  (pre-existing timing sensitivity, unrelated; green on re-run).
