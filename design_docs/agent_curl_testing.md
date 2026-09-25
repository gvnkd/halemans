# Halemans agent — curl testing cheat-sheet (stage stand)

Target: the local stage stand at `http://127.0.0.1:28080` (devenv). For a
docker deployment replace the base URL and use the provisioned admin account.
All chat endpoints are **session-authed** (login cookie); the agent acts as
the logged-in user with real RBAC. Passwords for the dev stand live in
`.devenv/state/halemans/{admin,sre,viewer}-password`.

## 1. Login (cookie jar)

```bash
BASE=http://127.0.0.1:28080
curl -s -c jar.txt -o /dev/null $BASE/NewSession
curl -s -b jar.txt -c jar.txt -o /dev/null -w "%{http_code}\n" \
  -X POST $BASE/CreateSession \
  --data-urlencode "email=admin@dev" \
  --data-urlencode "password=$(cat .devenv/state/halemans/admin-password)"
# expect 303; jar.txt now holds the session
```

Use `viewer@dev` (view only) or `sre@dev` (view+ack+close) to test RBAC
denials — the same tool call must return "forbidden: …" for them and succeed
for admin.

## 2. Send a chat message (plain JSON)

```bash
curl -s -b jar.txt -X POST $BASE/agent/chat \
  -H 'Content-Type: application/json' \
  -d '{"message":"List the environments","page_context":{"url":"/alerts?sort=title","title":"Alerts"}}'
```

Response: `{"session_id": "...", "replies": [{"content": "...", "tool_calls": [...], "trace": {...}}]}`.
Keep `session_id` and pass it back to continue the conversation.

## 3. Streaming (SSE) — how to watch progress live

```bash
curl -s -N -b jar.txt -X POST $BASE/agent/chat \
  -H 'Content-Type: application/json' \
  -d '{"message":"How many open alerts?","stream":true}' | tee sse.txt
```

Frames: `event: token` (words/elapsed_ms), `event: tool` (name), then
`event: done` with the replies payload. **The `stream` flag must be in the
JSON body** — a `?stream=1` query param is ignored (IHP only exposes the JSON
payload to param lookup on JSON requests). Useful flags: `--max-time 600`
for slow local models.

## 4. History and sessions (resume)

```bash
curl -s -b jar.txt $BASE/agent/sessions                        # id + title list
curl -s -b jar.txt $BASE/agent/chat/<session_id>               # full history (content, tool_calls, trace)
```

## 5. The two-phase confirm flow (mutations)

Mutating tools (`create_blackout`, `create_dashboard`, `delete_team`, …)
refuse to apply on first call:

```bash
# returns a plan + "confirmation required…"
curl -s -b jar.txt -X POST $BASE/agent/chat -H 'Content-Type: application/json' \
  -d '{"message":"Create a blackout for env dev from 2026-09-25T18:00:00Z to 2026-09-25T20:00:00Z"}'
# only after the user (you) agrees, repeat with explicit confirmation;
# the model then calls the tool with confirmed=true
```

## 6. RBAC checks (same call, different users)

```bash
# viewer (view only) — must contain "forbidden:"
curl -s -b viewer_jar.txt -X POST $BASE/agent/chat -H 'Content-Type: application/json' \
  -d '{"message":"Acknowledge alert <uuid>"}'
```

## 7. Traces (diagnosing stalls/failures)

- Widget: per-message `ⓘ …` footer (click to expand raw round detail).
- Ask the agent: `"explain your last turn"` — the `explain_last_turn` tool
  renders the session's own trace (durations, tool timings, errors).
- DB: `psql … -c "SELECT created_at, content, trace FROM agent_messages ORDER BY created_at DESC LIMIT 5;"`
- A stalled provider stream aborts with `stream stalled: no data for 90s`
  after 90s (server-side watchdog) and the widget labels it from 20s.

## 8. Internal API + MCP (for completeness)

Internal API (disabled unless `HALEMANS_INTERNAL_TOKEN` is set):

```bash
curl -s -H "X-Halemans-Internal: 1" \
     -H "Authorization: Bearer $HALEMANS_INTERNAL_TOKEN" \
     -H "X-Act-As: admin@dev" \
     "$BASE/api/internal/environments"
```

MCP stdio server (spawn per user; act-as via env):

```bash
HALEMANS_MCP_USER=admin@dev /path/to/HalemansMcp <<'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}
{"jsonrpc":"2.0","id":2,"method":"tools/list"}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"list_environments","arguments":{}}}
EOF
```

## 9. Provider/budget knobs (when answers fail oddly)

- `admin/llm` → Agent configuration page: agent prompt template
  (`internal_agent`, slots `{{user_name}} {{user_email}} {{language}}
  {{current_page_url}} {{current_page_title}}`), agent budget, global budget.
- Connection checks: "Test connection" (GET /v1/models) and "Test
  integration" (ping non-streaming + streaming) on the same page.
- No provider configured → the agent answers exactly that.

## 10. Known failure signatures (what to expect)

| Symptom | Meaning |
|---|---|
| `forbidden: the acting user lacks the X privilege` | RBAC gate working as designed |
| `confirmation required:` + plan | two-phase flow, reply to approve |
| `invalid arguments for <tool>: …` | bad args, soft-fail (turn continues) |
| `The LLM provider request failed (retriable): stream stalled: no data for 90s` | provider stopped mid-stream; watchdog fired |
| `The agent's daily LLM token budget is exhausted` | agent budget cap (admin/LLM → Agent) |
| replies containing `{{...}}` | should never happen; report it (slots must render) |
