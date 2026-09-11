# Milestone 11 — flapping alerts detector

Admin tool that analyzes alert timelines and reports flapping alerts: how
often they flap, typical flap periods, and which fingerprints are the worst
offenders. Milestone 11 is the on-demand report; a scheduled job with
persisted reports is a follow-up milestone (§6).

## 1. Flap definition

A **flap** is a fire → resolve → refire cycle. A fingerprint is **flapping**
when its timeline in the analysis window contains an episode of at least
`minFlaps` (default 3) such cycles where each resolve → refire gap is at
most `maxGapSeconds` (default 1800s — a refire long after resolution is a
new incident, not a flap).

Flap-relevant transitions only:

- `firing → resolved` (source resolve) — starts counting a cycle
- `resolved|stalled → firing` (refire) — closes the cycle; stalled→refire
  revives count, they are the classic noisy-source case
- closed alert row → NEW alert row with the same fingerprint — counts as a
  refire (row succession). Dedupe keeps one active row per fingerprint, so
  post-close refires would otherwise be invisible.

Plain `firing → firing` "repeated" events (occurrences bumps) are NOT
flaps. Manual `ack`/`unack`/`closed` transitions do not count as flap
edges, but a manual close followed by a new row still counts via row
succession.

## 2. Timeline extraction (SQL)

One typedSql query (sqlQueryTyped, per project rule) pulls, per
fingerprint, the ordered union of:

1. `alert_events` rows of kinds `resolved`, `repeated`, `stalled`,
   `closed` joined to their `alerts` row — restricted to events inside the
   analysis window, plus the alert's `started_at` as the initial firing
   marker;
2. `alerts` row succession: all rows sharing a fingerprint ordered by
   `first_seen_at` — the boundary (row N closed, row N+1 created) is a
   refire edge.

Only fingerprints with at least `minFlaps` candidate edges are folded in
Haskell; the SQL pre-filters via `GROUP BY fingerprint HAVING count(*) >=
minFlaps` so large histories stay cheap. Analysis window bounds the query
(default: last 24h, form offers 6h/24h/7d/30d).

## 3. Analysis (pure)

`Application/Service/Flapping.hs` — pure fold over the per-fingerprint
timeline, no DB, unit-tested like `Application.Pipeline.StateMachine`:

```haskell
data FlapParams = FlapParams
    { windowFrom    :: UTCTime
    , windowTo      :: UTCTime
    , minFlaps      :: Int      -- default 3
    , maxGapSeconds :: Int      -- default 1800
    }

data FlapEdge = FlapEdge
    { at   :: UTCTime
    , kind :: EdgeKind          -- Fired | Resolved | Revived (row succession)
    }

data FlapReport = FlapReport
    { fingerprint      :: Text
    , latestAlertId    :: Id' "alerts"
    , title            :: Text
    , severity         :: Text
    , effectiveEnv     :: Maybe Text   -- coalesce(nullif(facets->>'env',''), env)
    , host             :: Maybe Text
    , sourceName       :: Maybe Text
    , flapCount        :: Int
    , flapRatePerHour  :: Double
    , medianGapSeconds :: Double       -- dominant flapping period
    , p90GapSeconds    :: Double
    , minGapSeconds    :: Int
    , maxGapSecondsObs :: Int
    , mttrSeconds      :: Double       -- mean firing→resolved duration
    , activeFrom       :: UTCTime
    , activeTo         :: UTCTime
    , lastFlapAt       :: UTCTime
    }
```

`detectFlapping :: FlapParams -> [(Fingerprint, [FlapEdge])] -> [FlapReport]`
folds each timeline into episodes (gap > `maxGapSeconds` breaks an
episode), keeps episodes meeting `minFlaps`, computes the stats, and sorts
by `flapCount` desc. `FlapReport` is shaped so a later `flap_reports`
table stores it 1:1 — the job milestone adds persistence only, no rework.

## 4. Admin UI

`/admin/flapping` (admin privilege), following the existing admin page
conventions (`Web/Controller/…`, Fragments widgets, `pageHeaderHtml`):

- Params form: window (select 6h/24h/7d/30d), `minFlaps`, `maxGapSeconds`
  (number inputs, defaults per §1).
- Results table: fingerprint, title (link to latest alert), severity/env/
  host badges (existing Fragments helpers), source, plus the flap metrics
  below (`utcTimeHtml` for timestamps — all display timestamps go through
  the Helper.View time helpers). Each metric column header carries a
  `title` tooltip with this description:

  | Column | Meaning |
  | --- | --- |
  | Flaps | Fire/resolve/refire cycles in qualifying episodes (resolve → refire gap ≤ max gap) |
  | Rate/h | Flaps per hour over the analysis window |
  | Median gap | Median time between a resolve and the following refire — the usual flapping period |
  | P90 gap | 90th percentile of resolve → refire gaps — worst-case flapping period, outliers excluded |
  | MTTR | Mean time from firing to resolved (mean time to resolve) across flapping episodes |
  | Last flap | Most recent refire of a flapping episode |
- Empty state text when nothing flaps.

Report table mirrors the /alerts sortable table style; sorting is
server-side by flap count only in M11 (no user sortable columns).

## 5. Tests

- `Test/…` unit specs for `detectFlapping`: synthetic timelines covering
  episode splitting on `maxGapSeconds`, below-threshold rejection,
  row-succession revives, MTTR/gap stats, sorting.
- Integration: seed alert_events + successor rows, run the SQL + fold,
  assert the report row.
- Smoke/Playwright: not in M11 scope (admin-only, low risk); page renders
  covered by the integration check if cheap, otherwise manual.

## 6. Follow-up (NOT in this milestone)

Scheduled detection: self-rescheduling `flap_detector_jobs` +
`flap_reports` persistence (schema mirrors `FlapReport` 1:1) + trending
view. Deferred until the on-demand report proves the parameters.

## 7. Versioning

Minor version bump (new feature, backwards compatible) in
`Halemans.cabal` + `Application/Version.hs` together.
