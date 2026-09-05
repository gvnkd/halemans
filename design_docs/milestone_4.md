# Milestone 4 — LLM auto-enrichment (Phase 4)

Goal: add the LLM subsystem from `01_highlevel.md` §9 + §17 Phase 4 on top of the milestone-3 context stack: provider abstraction (OpenAI-compatible HTTP), automatic per-alert analysis, versioned DB-stored prompt templates, feedback loop, cost/rate controls, and optional read-only tool access (CMDB lookup, Jira search) for the model.

Builds on the milestone-3 stack: same devenv processes, same smoke harness. CMDB excerpts (`cmdb_entries.excerpt`) and Jira links (`jira_links`) are consumed as-is — M3 persisted them in exactly the shapes the prompt builder needs. The same M3 service clients (`Application/Service/Cmdb.hs`, `Application/Service/Jira.hs`) back the LLM tools. Retention, source-health alerting, and audit exports stay Phase 5 and are out of scope.

## 1. Deliverables

| # | Deliverable | Acceptance |
|---|---|---|
| D1 | Phase-4 schema delta (llm_analyses, llm_prompt_templates, llm_feedback, llm_budget_counters) | Migrations apply clean; `build/Generated/Types.hs` regenerated |
| D2 | `LlmProvider` abstraction + OpenAI-compatible HTTP implementation per §9 | Same client drives local llama.cpp-style endpoint and hosted APIs; provider config via env |
| D3 | `LlmAnalysisJob`: auto-enqueue on new alert (pipeline step 8 extension), manual "Re-analyze" on alert card | New alert → analysis appears on card when done; re-analyze appends a new analysis, card shows latest |
| D4 | Prompt builder: versioned DB templates filled with alert fields, recent AlertEvents, CMDB excerpt, similar past alerts, linked Jira tickets; hard token budget | Rendered prompt stays within documented per-field truncation budget; template version recorded on the analysis row |
| D4a | Optional tool access: `cmdb_lookup` + `jira_search` exposed to the model via OpenAI-style tool calling, read-only, per-provider toggle | With tools enabled the model can pull fresh CMDB/Jira context mid-analysis; tool calls recorded on the analysis row; disabled → prompt-injected context only |
| D5 | Structured output: markdown summary + jsonb (`probable_cause`, `confidence`, `suggested_actions[]`, `references[]`) rendered on alert card with provider/model/timestamp | Card panel renders all fields; malformed model output degrades to raw-markdown display, never crashes the card |
| D6 | Feedback widget (👍/👎) per analysis, stored for prompt tuning | Feedback persists per user per analysis; admin prompt-template page shows aggregate score |
| D7 | Cost/rate controls per §9: per-provider rate limiter, daily budget counter, prompt-hash dedupe within a window | Refire storm with identical context → one analysis; budget exceeded → soft-skip with card notice + `AlertEvent` |
| D8 | Safety per §9: advisory only — LLM output never triggers state changes, notifications, or external calls; soft-fail with retry backoff | No code path from analysis result into pipeline actions; provider down → card shows "analysis unavailable" |
| D9 | Dev fixtures: mock OpenAI-compatible LLM server in devenv (seeded deterministic completions) | Smoke/integration run without a real model |
| D10 | Smoke/Playwright suites extended; `nix flake check --impure` green | Check green |

## 2. Schema delta (§3 of highlevel, Phase-4 slice)

Migrations in `Application/Migration/$(date +%s)-*.sql` per project convention. Same Schema.sql parser limits as before (columns inline in `CREATE TABLE`; FKs as top-level `ALTER ... ADD CONSTRAINT`).

New tables:
- `llm_analyses` — alert ref, provider text, model text, prompt_template ref + version int, prompt_hash text, status (`queued|running|done|failed`), error text nullable, result_md text nullable, result jsonb nullable (`probable_cause`, `confidence`, `suggested_actions[]`, `references[]`), tokens_in/tokens_out int nullable, created/updated. Index on (alert ref, created_at desc) — card reads latest `done`.
- `llm_prompt_templates` — name, version int, body text (template with named placeholders), active bool, notes, created/updated. Unique (name, version); partial unique index: one active per name.
- `llm_feedback` — analysis ref, user ref, score int (1 / -1), comment text nullable, created_at. Unique (analysis ref, user ref) — one vote per user, re-vote updates.
- `llm_budget_counters` — provider text, day date, tokens_in bigint, tokens_out bigint, requests int. Unique (provider, day); upserted atomically by the job.

Modified tables: none. `alerts`, `cmdb_entries`, `jira_links` untouched — prompt builder reads them read-only.

Deferred (still NOT created): retention config table (Phase 5).

## 3. Provider abstraction (§9)

```haskell
class LlmProvider p where
  complete :: p -> Prompt -> IO (Either LlmError Completion)
```

- `Application/Service/Llm.hs` — v1 implementation: OpenAI-compatible chat-completions HTTP client (covers local llama.cpp/vLLM and hosted APIs). Config: endpoint, model, api key via env (`${LLM_ENDPOINT}` / `${LLM_MODEL}` / `${LLM_API_KEY}` pattern, never in DB plaintext — §14). Key optional for local endpoints.
- `Completion` carries content text + usage (tokens_in/out) when the endpoint reports it; usage feeds `llm_budget_counters`.
- Provider selectable per analysis type via config key; v1 has exactly one analysis type (`alert_enrichment`), so a single configured provider — the per-type indirection exists so later types (e.g. weekly summary) slot in without schema change.
- Timeouts: connect 5s, total 120s (configurable); HTTP 429/5xx are retriable `LlmError`s, 4xx terminal.

## 4. Analysis pipeline (extends `Application/Helper/Ingest.hs` step 8)

- **On new alert only** (not dedupe hits): after `EnrichAlertJob` enqueue, insert `llm_analyses` row (status `queued`, prompt_hash computed) and enqueue `LlmAnalysisJob(analysisId)`.
- **Manual**: "Re-analyze" button on the alert card (`view` privilege — advisory, non-destructive) inserts a fresh row (same prompt builder, current context) and enqueues the job. Card shows latest `done`; history expandable.
- **Job flow**: load analysis → check budget + rate limit + prompt-hash dedupe (§6) → build prompt (§5) → `complete` → parse structured fields out of the response ( fenced-json block convention, lenient parse) → store markdown + jsonb + usage → status `done`; publish on `halemans_events` (alert scope, kind `enriched` — reuse the M3 fragment/WS path so an open card live-updates).
- **Failure**: retriable errors retry with backoff (max 3); terminal failure → status `failed` + error, card shows "analysis unavailable", `AlertEvent(llm_failed)`. Soft-fail only (§9).
- Ordering vs `EnrichAlertJob`: the LLM job does not wait for enrichment — prompt builder reads `cmdb_entries`/`jira_links` if present at build time and tolerates absence (fields render as "unknown"). A later M5 polish may re-trigger after enrichment lands; out of scope here.

## 5. Prompt builder

`Application/Service/Llm/Prompt.hs`:

- Template: `llm_prompt_templates` active row for name `alert_enrichment`; placeholders `{{alert.title}}`, `{{alert.description}}`, `{{events}}`, `{{cmdb_excerpt}}`, `{{similar_alerts}}`, `{{jira_links}}` etc. Template body versioned — every analysis records template ref + version (replay/tuning per §9).
- Fill sources:
  - alert fields (title, description, severity, env/host/service names, labels, annotations);
  - recent `AlertEvent`s (last 10, compact one-line rendering);
  - `cmdb_entries.excerpt` for the alert's host/service (already truncated to its documented budget by M3);
  - similar past alerts: same fingerprint or same check name, last 5 `resolved`/`closed`, with close reason + ack comments as the "how it was fixed" signal;
  - `jira_links` for the alert (key, summary, status).
- **Token budget**: hard cap configurable (default 4096 prompt tokens, chars/4 heuristic — no tokenizer dependency). Per-field truncation order documented: similar_alerts → events → cmdb_excerpt → jira_links → description (title/labels never truncated). Truncation points marked `…[truncated]` in the rendered prompt.
- Prompt hash: sha256 of rendered prompt text — feeds dedupe (§6).
- Expected-output contract appended by the builder (not the template): markdown analysis, then a ```json fenced block with `probable_cause`, `confidence`, `suggested_actions[]`, `references[]`. Parser treats the json block as best-effort — absent/malformed → markdown-only result (D5).

## 6. Cost & rate controls (§9)

- **Rate limiter**: per-provider token bucket in the job (requests/min, default 20) — job re-queues itself with delay when over limit rather than failing.
- **Daily budget**: `llm_budget_counters` upsert after each completion; configurable daily token cap per provider (default 1M tokens). Over budget → analysis row `failed` with error `budget_exceeded`, card notice, `AlertEvent(llm_skipped)`. Counter resets by date row, no job needed.
- **Prompt-hash dedupe**: identical prompt_hash with status `done` within a window (default 1h) → new row is marked `done` as a copy of the prior result (`result`/`result_md` copied, note `deduped` in error-free status field via a `deduped_from` nullable self-ref column). Refire storms cost one analysis (§9).
- All knobs via env/config with the documented defaults; admin page shows today's counters per provider.

## 7. UI additions (server-rendered HSX)

- **Alert card**: "LLM analysis" panel — markdown summary, structured fields (probable cause, confidence badge, suggested actions list, reference links), provider/model/template-version/timestamp footer, 👍/👎 feedback widget (htmx-less: plain form POST + WS refresh), "Re-analyze" button, history toggle for older analyses. Failed → "analysis unavailable" chip with retry state; deduped copies marked.
- **Admin → LLM**: prompt template list/edit/new-version (edit creates version+1, activate flips the partial-unique active row transactionally), per-template aggregate feedback score, budget counters + rate-limit config display, connection test button (env presence + models-list ping).
- **Fragments**: analysis panel renderer in `Web/View/Fragments.hs` shared with the WS broadcaster (kind `enriched`, alert scope) per the M3 live-update pattern.

## 8. Background jobs

| Job | Schedule | Purpose |
|---|---|---|
| `LlmAnalysisJob` (new) | enqueued on alert creation / manual re-analyze | prompt build → provider call → store (§4) |
| `EnrichAlertJob` | unchanged | LLM job enqueued alongside it in pipeline step 8 |
| `PollZabbixJob` / `PollGrafanaJob`, `WriteBackJob`, `JiraSyncJob`, `EscalationJob`, `AutoCloseJob`, `PushNotificationJob` | unchanged | — |

## 9. Dev fixtures & mocks

- devenv gains a mock OpenAI-compatible server (python stdlib, same pattern as mock-confluence/mock-jira in nix/mocks/): `POST /v1/chat/completions` answering deterministic completions keyed by prompt content markers (contains "disk" → disk-cause analysis; else generic), with a `/debug` backdoor to force 429/500/malformed-json for failure-path tests. Port 18084 (18080–18083 taken per MEMORIES.md — verify before use).
- `seed-halemans` (and the inline copy in smoke-check.sh — keep both in sync per project convention) seeds: provider config pointing at the mock, one active `llm_prompt_templates` row (`alert_enrichment` v1), and env vars `LLM_ENDPOINT`/`LLM_MODEL` in env.sh.
- `stack-status` reports mock LLM health alongside the other services.

## 10. Testing

- Unit: prompt builder fill + truncation order + budget cap; prompt-hash stability (same inputs → same hash); structured-output parser (well-formed, missing fence, malformed json, extra prose); template placeholder rendering; budget/dedupe decision logic (pure functions over counter rows); rate-limiter delay computation.
- Integration: new alert → analysis row → job against mock → card data present; dedupe — two alerts with identical context in-window → one provider call, second row copied; budget exceeded → soft-skip path; mock 429 → retry, mock 500 ×3 → `failed`; malformed-json completion → markdown-only result; feedback upsert (vote, re-vote changes score).
- Playwright: analysis panel renders after WS `enriched` push (no reload); re-analyze appends and latest wins; 👍/👎 round-trip; admin template edit bumps version and a new analysis records v2; forced mock failure → "analysis unavailable" chip.
- Smoke: extend `tests/smoke/run.sh` — fire alert, wait for `llm_analyses` status `done`, assert result fields non-empty.
- Canonical gate: `nix flake check --impure`.

## 11. Acceptance checklist (end of Milestone 4)

- [ ] New alert → LLM panel fills in via WS without reload: markdown + probable cause + suggested actions + references, footer shows mock provider/model/template v1
- [ ] Identical-context refire within 1h → exactly one provider call; second analysis marked as deduped copy
- [ ] "Re-analyze" appends a new analysis; card shows latest; older ones expandable in history
- [ ] 👍/👎 persists; admin template page shows the aggregate; re-voting flips the score
- [ ] Template edit in admin creates v2; next analysis records version 2 and renders with the new body
- [ ] Forced mock 429 → job retries; forced 500 ×3 → panel shows "analysis unavailable" + timeline event
- [ ] Malformed-json completion → markdown shown, structured section absent, no card error
- [ ] Daily budget cap (lowered in test) → new analyses soft-skip with notice + `AlertEvent`
- [ ] LLM output never affects alert status/notifications (grep-verified: no pipeline import from Llm result code path)
- [ ] `nix flake check --impure` green (unit + integration + smoke + Playwright)
- [ ] No secrets committed; mock config only in `.devenv/state/`

## 12. Decisions

- **Mock LLM over a real local model for dev/smoke**: deterministic seeded completions keep CI fast and assertions stable; the OpenAI-compatible client is identical either way, so pointing at a real llama.cpp/vLLM is a pure env change. Recorded-response stubs stay for Haskell unit tests of the client.
- **Analyses are append-only rows, latest-wins on display** (per §9 "new analysis appended on retrigger"): preserves prompt/version lineage for tuning; no in-place update of results.
- **Prompt-hash dedupe copies the prior result into a new row** rather than pointing the card at the old one: keeps per-alert history self-contained and the card query trivial (latest by alert).
- **Token counting by chars/4 heuristic**, no tokenizer dependency: the budget is a safety cap, not billing accuracy; documented in the admin UI.
- **Structured output via fenced-json convention, best-effort parse** instead of provider-specific function-calling: keeps the client portable across llama.cpp/vLLM/hosted APIs; degradation mode is markdown-only, never an error.
- **No enrichment→LLM ordering dependency**: prompt tolerates missing CMDB/Jira context; a re-trigger after enrichment lands is deferred (likely Phase 5 polish) to avoid cross-job orchestration in v1.
- **Feedback is per-user-per-analysis with re-vote upsert** — simplest shape that still gives the tuning signal §9 asks for.

## 13. Implementation notes

- Schema: `error` column renamed to `error_message` (generated field would clash with `Prelude.error` in every `Generated.Types` consumer). `tool_calls jsonb` records the D4a tool-call log.
- Prompt hash is computed in the job at build time, not at ingest (§4 says insert-time; building the prompt needs template + context reads that don't belong in the webhook path). Dedupe therefore triggers on identical rendered prompts at job time — in practice same-alert re-analysis, since AlertEvent timestamps make cross-alert prompts unique.
- Retry budget counts `llm_analysis_jobs` rows per analysis (requeued rows reset `attempts_count`); with the default `[60,60]` backoff that's 2 retries, 3 provider calls. `HALEMANS_LLM_BACKOFF_SECONDS` overrides in tests.
- `LlmAnalysisJob.queuePollInterval` = 10s; connect timeout isn't settable in this http-client version (response timeout 120s only).
- Markdown renders as escaped `<pre>` (no markdown library in the dependency set); structured section renders from `result` jsonb; malformed/absent fence → markdown-only, never a card error.
- D8 verified structurally: nothing under `Application/Service/Llm*` or `Application/Job/LlmAnalysis` imports pipeline/notify modules; the result path only writes `llm_analyses`, `llm_budget_counters`, `AlertEvent`, and the WS notification.
- Deviation in smoke/Playwright: dedupe-copy assertion lives in the integration suite only; the grafana-absence reconcile can mutate the event log between analyses and make the copy legitimately not happen.
