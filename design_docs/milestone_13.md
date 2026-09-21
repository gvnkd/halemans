# Milestone 13 — zabbix metrics chart + local metric cache

Extends the alert-detail metric chart (today grafana-only, see
`Application/Service/MetricChart.hs`) to zabbix sources and adds a small
local cache so repeated views don't re-hit upstream. Decisions from the
design discussion:

- Token with `history.get` only is sufficient — `trends.get` is a
  downsampled convenience, nothing in the pipeline needs it. Wide ranges
  are handled by client-side bucketing, not by trends.
- Item resolution needs no mapping table: `trigger.get` with
  `selectItems` returns the items referenced by the trigger expression,
  server-resolved (templates, per-host). The event payload itself carries
  only `triggerid` (we keep it in the fingerprint).
- Plain Postgres table, no timescale/TSDB extension — this is an
  on-demand cache (viewed charts only), not a metrics store. No
  prefetching.

## 1. Zabbix connector: metric fetch (`Application/Connector/Zabbix.hs`)

New functions, reusing the existing rpc/`postFollowing` pattern:

- `triggerItemsGet :: baseUrl -> token -> [triggerId] -> IO (Either Text [ZabbixTriggerItem])`
  — `trigger.get` with `triggerids`, `output: [triggerid]`,
  `selectItems: ["itemid", "key_", "name", "value_type", "units"]`.
  `ZabbixTriggerItem { triggerItemTriggerId, triggerItemId, triggerItemKey, triggerItemName, triggerItemValueType, triggerItemUnits }`.
  Token needs no extra scope: trigger read permission implies item read on
  the same hosts.
- `historyGet :: baseUrl -> token -> itemId -> historyType -> from -> to -> IO (Either Text [MetricSeries])`
  — `history.get` with `itemids`, `history` (0=float, 3=unsigned; skip
  items of other types before fetching), `time_from`/`time_till`,
  `sortfield: clock`, `sortorder: ASC`. Paginate with `limit` +
  last-`clock` continuation (same loop shape as `eventGet`,
  Zabbix.hs:63-79), because the server caps results per call. Series name =
  item `name` (+ `key_` on collision).
- Unit test trap from MEMORIES applies: new module exports must be added to
  `halemans.cabal` via `docker/prepare-cabal-build.sh` and `git add`ed
  before `nix flake check` (flake builds from the git index).

Fingerprint: `alert.fingerprint` is `zabbix:trigger:<id>`
(`Application/Job/PollZabbix.hs:359`), so the trigger id is
`stripPrefix "zabbix:trigger:"` — same helper the reconcile path uses.

## 2. Metric cache

Migration `Application/Migration/<date +%s>-metric_cache.sql` (revision =
raw `date +%s`, never rounded — see project rule in AGENTS.md):

```sql
CREATE TABLE metric_cache (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    source_id UUID NOT NULL REFERENCES sources(id) ON DELETE CASCADE,
    series_key TEXT NOT NULL,        -- zabbix: "z:<itemid>"; grafana: "g:<dsUid>:<sha1(expr)>"
    bucket_start TIMESTAMPTZ NOT NULL, -- hourly-aligned window start
    points JSONB NOT NULL,           -- [[unixSeconds, value], ...] ascending
    fetched_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE UNIQUE INDEX metric_cache_key ON metric_cache (source_id, series_key, bucket_start);
CREATE INDEX metric_cache_stale ON metric_cache (fetched_at);
```

- Hourly buckets, aligned with `date_trunc('hour', ...)`. Closed buckets
  are immutable → served forever. The bucket containing `now` is volatile:
  treated as a miss when older than `max(item interval, 60s)` — simplest
  correct rule is a per-fetch `freshenMs` (default 60s), no interval
  detection.
- `Application/Service/MetricCache.hs`:
  `readCached :: sourceId -> seriesKey -> from -> to -> freshenMs -> IO [(UTCTime, Double)]` (coverage report: contiguous cached prefix/suffix + gaps),
  `storeBuckets :: sourceId -> seriesKey -> points -> IO ()` (split into hourly buckets, `ON CONFLICT (source_id, series_key, bucket_start) DO UPDATE SET points, fetched_at`).
- Retention: opportunistic delete on store —
  `DELETE FROM metric_cache WHERE fetched_at < now() - interval '<N> days'`
  for the same source_id, N = `sources.config.metrics.cacheRetentionDays`
  default **7**. No background job.
- `series_key` for grafana needs the expr, i.e. one `ruleQueryGet` round
  trip before cache lookup; that call stays uncached (cheap, per rule).

## 3. Fetch orchestration (`Application/Service/MetricChart.hs`)

`fetchAlertMetricSeriesUnchecked` becomes a 3-source-type dispatch:

```
"zabbix"  -> triggerItemsGet -> numeric items -> per item:
               cached coverage? -> fetch only missing sub-ranges via historyGet
             -> storeBuckets -> merge -> downsample -> [MetricSeries]
"grafana" -> ruleQueryGet -> dsQueryRange -> storeBuckets (per returned series)
             -> merge with cache -> downsample
_         -> Left "Metrics are only available for Grafana- and Zabbix-sourced alerts"
```

- `downsample :: Int -> [(UTCTime, Double)] -> [(UTCTime, Double)]` —
  fixed-count time buckets, mean per bucket. Lives in MetricChart (pure,
  unit-tested). `mwMaxPoints` stays the knob.
- Error states rendered by the existing widget: missing token env (reuse
  `tokenEnv` config), trigger with no numeric items ("no numeric metric
  for this trigger"), upstream error text passthrough.
- Weekly dynamics (the motivating case) falls out for free: window comes
  from `metricWindowFor`; per-source overrides `leadMinutes` /
  `trailMinutes` / `maxPoints` already exist — add `cacheRetentionDays`
  and `freshenSeconds` to the same config block.

## 4. Controller/view wiring

No changes needed to `Web/Controller/Alerts.hs:176-179` (it already calls
`fetchAlertMetricSeries` + `seriesChartSvg` and renders `Left` as the
message) — verify that's true for zabbix alerts end-to-end and that the
`data-testid="metric-chart-svg"` path works in the zabbix integration
fixture. The grafana-only Left message changes (§3).

## 5. Config surface

`Web/View/Sources/Form.hs` metric-chart section: currently grafana is
implicit. No new form fields required for the milestone (defaults suffice);
document `sources.config.metrics` keys in the milestone closeout:
`leadMinutes`, `trailMinutes`, `maxPoints`, `cacheRetentionDays`,
`freshenSeconds`.

## 6. Mocks + tests

- Extend the zabbix mock (`nix/mocks/`) with `trigger.get`
  (`selectItems` echo) and `history.get` (deterministic series for a
  couple of itemids, honours `time_from`/`time_till`/`limit`). Check the
  existing grafana mock covers `ds/query` already (it backs
  `Test/Integration/MetricChartSpec.hs`).
- Unit (`Test/MetricChartSpec.hs`): history response decoding (value_type
  filter, clock parsing), `downsample` (bucket count, mean, single-point
  and empty input), bucket split/merge round-trip incl. partial first/last
  hour, coverage-gap computation.
- Integration: zabbix-sourced alert renders
  `data-testid="metric-chart-svg"`; second render within freshen window
  does NOT re-call the mock (assert mock request count); text-valued item
  renders the "no numeric metric" message; missing token env renders the
  config hint.
- Full gate: `nix flake check --impure` (kill the devenv stack first —
  port lottery, MEMORIES 2026-09-20). Pre-existing flakes not to chase:
  smoke `job-metrics-table` timeout (deterministic on HEAD), smoke
  load-flakiness variants.

## 7. Out of scope

- `trends.get` support (if an admin later grants it, it slots in as a
  range-split strategy — not now).
- Prefetching / background metric collection (different product).
- TimescaleDB or any TSDB extension (revisit only if the cache table
  measurably hurts).
- Zabbix calculated-item special handling beyond value_type filtering.
- Per-item series selection when a trigger references many items: render
  all (palette already cycles), selection UI only if users complain.

## 8. Verification & versioning

- `cabal build` for fast iteration; HLS diagnostics are unreliable for new
  modules (MEMORIES) — trust the build.
- Canonical gate `nix flake check --impure`; run fourmolu exactly as the
  style check does (`--no-cabal -o -XOverloadedRecordDot -o
  -XOverloadedLabels`, twice, via `bash -c`), `git add -A` before the
  check.
- Version bump in BOTH `halemans.cabal` and `Application/Version.hs`
  (minor — user-visible feature), tag at closeout per release flow.

## 9. Implementation notes (post-landing)

- The zabbix mock did not exist — created `nix/mocks/mock_zabbix.py`
  (port 18087) instead of extending; wired into `nix/mocks.nix`,
  `nix/checks.nix` (integration + smoke) and `tests/smoke/run.sh`.
- `Web/Controller/Alerts.hs` `metricsAvailable` gate extended to
  `elem ["grafana","zabbix"]` (was grafana-only; without it zabbix alerts
  never show the chart button).
- Smoke seeds the zabbix-mock source with `enabled=false`: an enabled
  zabbix source + seeded firing alert row in the smoke DB reliably breaks
  the "jira: create ticket" playwright check (mechanism unknown,
  reproduced 3/3 — recorded in `.opencode/MEMORIES.md`; root-cause before
  any future smoke change that needs an enabled zabbix source).
- Nasty aeson trap hit during development: unannotated `parseMaybe`
  result unified with a downstream tuple type and silently failed to
  parse — annotated `[Aeson.Value]`; recorded in MEMORIES.
- The diagrams-based `seriesChartSvg` needed three fixes to become the
  project's standard chart widget (`Application/Service/Chart.hs`, the
  reusable building block for future charts): pin BOTH output dimensions
  (`mkSizeSpec2D`) — width-only lets the envelope aspect rescale the whole
  plot; position the invisible backdrop at the frame center so the envelope
  IS the frame (1:1 coordinates); and respect `D.position` origin
  conventions (a stroked `fromVertices` path has origin (0,0) with absolute
  vertices — placing it at a non-zero point double-offsets it; that single
  bug inflated the envelope to 1740px and rescaled everything 0.52x). The
  widget also pads degenerate single-sample domains and draws dots for
  sparse series (<= 25 points). `seriesChartSvg` is now a thin adapter over
  `Chart.lineChartSvg`.
- The /reports volume chart migrated onto the same widget:
  `Chart.stackedBarChartSvg` + `Chart.StackedBar`/`BarSegment`;
  `Reports.volumeChartSvg` is a small severity-mapping adapter. The
  horizontal report charts stay plain HTML bar rows (not SVG widgets).
  Two more diagrams traps surfaced during the migration and are recorded
  in the module header + MEMORIES: svg-space stacking math hangs bars
  below the baseline in y-up diagrams space (accumulate segment bottoms
  with `scanl (+)`), and `extentX`/`extentY` are the fast envelope
  debugger.
- `historyGet` covers the token-permission question from the design
  discussion: only `trigger.get` + `history.get` are called, both covered
  by the same host read permission; `trends.get` is never needed.
