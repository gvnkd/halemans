set -euo pipefail

# Perf harness (design_docs/milestone_5.md §6): dev tooling, not a flake
# check gate. Seeds a scratch postgres with 10k active alerts across 5
# environments and times the hot endpoints against the prod binary, asserts
# the §15 targets (dashboard p95 < 300ms, WS fan-out < 1s) and EXPLAINs the
# hot queries to prove index usage.
#
# Deviation from the design text: the dataset is seeded with psql
# generate_series instead of a Haskell seeder module — 10k alerts + 200k
# events through the ORM would take minutes; SQL does it in seconds. The
# harness produces no tracked artifacts (all state under $TMPDIR).
#
# Usage: nix develop .#default --impure -c bash nix/scripts/perf-harness.sh
# (builds the prod server via nix if PERF_RUN_PROD_SERVER is unset)

T="${TMPDIR:-/tmp}/halemans-perf"
rm -rf "$T"
mkdir -p "$T"
PORT="${PERF_PORT:-39080}"
APP="http://127.0.0.1:$PORT"

pids=""
cleanup() {
    for p in $pids; do kill "$p" 2>/dev/null || true; done
    pg_ctl -D "$T/pgdata" stop -m immediate > /dev/null 2>&1 || true
}
trap cleanup EXIT

RUN_PROD_SERVER="${PERF_RUN_PROD_SERVER:-}"
if [ -z "$RUN_PROD_SERVER" ]; then
    root="$(git rev-parse --show-toplevel)"
    mkdir -p "$root/.devenv"
    printf %s "$root" > "$root/.devenv/root"
    nix build "$root#unoptimized-prod-server" --impure --offline \
        --override-input devenv-root "file+file://$root/.devenv/root" \
        -o "$T/prod-server"
    RUN_PROD_SERVER="$T/prod-server/bin/RunProdServer"
fi

# --- postgres ---------------------------------------------------------------
export PGDATA="$T/pgdata" PGHOST="$T/pghost"
mkdir -p "$PGHOST"
initdb -D "$PGDATA" --no-locale --encoding=UTF8 > /dev/null
echo "unix_socket_directories = '$PGHOST'" >> "$PGDATA/postgresql.conf"
echo "listen_addresses = ''" >> "$PGDATA/postgresql.conf"
pg_ctl -D "$PGDATA" -l "$T/pg.log" -w start > /dev/null
createdb -h "$PGHOST" perf
export DATABASE_URL="postgres:///perf?host=$PGHOST"

root="$(git rev-parse --show-toplevel)"
ihp_schema="${IHP_LIB:-${IHP:-}}/IHPSchema.sql"
[ -f "$ihp_schema" ] || ihp_schema="$root/build/ihp/IHPSchema.sql"
psql -h "$PGHOST" -d perf -v ON_ERROR_STOP=1 -q \
    -f "$ihp_schema" -f "$root/Application/Schema.sql"

# --- dataset: 10k active alerts / 5 envs / ~30% grouped / 20 events each ----
echo "perf: seeding 10k-alert dataset"
psql -h "$PGHOST" -d perf -v ON_ERROR_STOP=1 -q <<'SQL'
INSERT INTO environments (name)
SELECT 'perf-env-' || g FROM generate_series(0, 4) g;

INSERT INTO alert_groups (group_key, title, environment_id, status, worst_severity, member_count)
SELECT 'perf-group-' || g, 'perf group ' || g, e.id, 'firing', 'high', 6
FROM generate_series(0, 499) g
JOIN environments e ON e.name = 'perf-env-' || (g % 5);

INSERT INTO alerts (fingerprint, title, severity, status, env, host, check_name,
                    environment_id, group_id, started_at, first_seen_at, last_seen_at)
SELECT 'perf-fp-' || g,
       'perf alert ' || g,
       (ARRAY['critical','high','warning','info'])[1 + (g % 4)],
       (ARRAY['firing','firing','firing','ack','resolved'])[1 + (g % 5)],
       'perf-env-' || (g % 5),
       'perf-host-' || (g % 100),
       'perf-check-' || (g % 25),
       e.id,
       CASE WHEN g % 10 < 3 THEN
           (SELECT gr.id FROM alert_groups gr WHERE gr.group_key = 'perf-group-' || (g % 500))
       END,
       NOW() - (g || ' minutes')::interval,
       NOW() - (g || ' minutes')::interval,
       NOW() - ((g % 60) || ' seconds')::interval
FROM generate_series(1, 10000) g
JOIN environments e ON e.name = 'perf-env-' || (g % 5);

INSERT INTO alert_events (alert_id, kind, payload, created_at)
SELECT a.id, (ARRAY['created','repeated','notified','external'])[1 + (n % 4)],
       '{}'::jsonb, a.created_at + (n || ' seconds')::interval
FROM alerts a CROSS JOIN generate_series(1, 20) n;

INSERT INTO cmdb_entries (host_id, page_id, title, excerpt, url)
SELECT h.id, 'P' || h.id, h.fqdn, 'perf cmdb excerpt', '/pages/1'
FROM hosts h;

INSERT INTO llm_analyses (alert_id, provider, model, status, result_md, prompt_hash)
SELECT a.id, 'default', 'perf-model', 'done', 'perf analysis', md5(a.id::text)
FROM alerts a WHERE a.fingerprint LIKE 'perf-fp-1%';

ANALYZE alerts;
ANALYZE alert_events;
SQL

# --- users -------------------------------------------------------------------
HASH=$(python3 "$root/nix/scripts/hash-password.py" "perf-admin-password")
psql -h "$PGHOST" -d perf -v ON_ERROR_STOP=1 -q -v hash="$HASH" <<'SQL'
INSERT INTO roles (name, privileges) VALUES ('admin', '{admin}');
INSERT INTO users (email, password_hash, display_name)
VALUES ('admin@perf', :'hash', 'admin@perf');
INSERT INTO user_roles (user_id, role_id)
SELECT u.id, r.id FROM users u, roles r WHERE u.email = 'admin@perf' AND r.name = 'admin';
SQL

# --- app ----------------------------------------------------------------------
cd "$T"
PORT="$PORT" DATABASE_URL="$DATABASE_URL" "$RUN_PROD_SERVER" > "$T/app.log" 2>&1 &
pids="$pids $!"
for i in $(seq 1 60); do
    curl -s -o /dev/null "$APP/alerts" && break
    sleep 1
done

COOKIES="$T/cookies.txt"
curl -sf -c "$COOKIES" "$APP/NewSession" > /dev/null
curl -sf -b "$COOKIES" -c "$COOKIES" -o /dev/null \
    -d "email=admin@perf" -d "password=perf-admin-password" "$APP/CreateSession"

ALERT_ID=$(psql -h "$PGHOST" -d perf -tA -c "SELECT id FROM alerts WHERE fingerprint = 'perf-fp-1'")

# --- endpoint timings ---------------------------------------------------------
time_endpoint() { # <name> <path> <requests>
    local name="$1" path="$2" n="$3"
    for i in $(seq 1 "$n"); do
        curl -sf -b "$COOKIES" -o /dev/null -w '%{time_total}\n' "$APP$path"
    done | python3 -c '
import sys
samples = sorted(float(line) * 1000 for line in sys.stdin)
def pct(p):
    return samples[min(len(samples) - 1, int(p / 100 * len(samples)))]
print(f"{pct(50):.1f} {pct(95):.1f} {pct(99):.1f}")
'
}

echo
echo "endpoint          p50(ms)  p95(ms)  p99(ms)"
printf "%-16s %8s %8s %8s\n" "dashboard /" $(time_endpoint "dashboard" "/" 40)
printf "%-16s %8s %8s %8s\n" "env page" $(time_endpoint "env" "/env/perf-env-0" 40)
printf "%-16s %8s %8s %8s\n" "alert card" $(time_endpoint "card" "/alerts/$ALERT_ID" 40)

dash_p95=$(time_endpoint "dashboard" "/" 40 | cut -d' ' -f2)

# --- EXPLAIN: hot queries must use the phase-5 indexes -------------------------
echo
echo "=== EXPLAIN: fingerprint dedupe lookup ==="
psql -h "$PGHOST" -d perf -c "EXPLAIN SELECT id FROM alerts WHERE fingerprint = 'perf-fp-1' AND status <> 'closed'"
echo "=== EXPLAIN: environment page ==="
psql -h "$PGHOST" -d perf -c "EXPLAIN SELECT id FROM alerts WHERE environment_id = (SELECT id FROM environments WHERE name = 'perf-env-0') AND status = 'firing' ORDER BY last_seen_at DESC LIMIT 200"
echo "=== EXPLAIN: alert card event timeline ==="
psql -h "$PGHOST" -d perf -c "EXPLAIN SELECT id FROM alert_events WHERE alert_id = '$ALERT_ID' ORDER BY created_at DESC"

explain_out=$(psql -h "$PGHOST" -d perf -tA -c "EXPLAIN SELECT id FROM alerts WHERE fingerprint = 'perf-fp-1' AND status <> 'closed'")
echo "$explain_out" | grep -q "alerts_fingerprint_active_idx" \
    && echo "index ok: alerts_fingerprint_active_idx" || { echo "MISSING INDEX: fingerprint partial" >&2; exit 1; }
explain_env=$(psql -h "$PGHOST" -d perf -tA -c "EXPLAIN SELECT id FROM alerts WHERE environment_id = (SELECT id FROM environments WHERE name = 'perf-env-0') AND status = 'firing' ORDER BY last_seen_at DESC LIMIT 200")
echo "$explain_env" | grep -q "alerts_environment_status_idx" \
    && echo "index ok: alerts_environment_status_idx" || { echo "MISSING INDEX: environment+status" >&2; exit 1; }
explain_events=$(psql -h "$PGHOST" -d perf -tA -c "EXPLAIN SELECT id FROM alert_events WHERE alert_id = '$ALERT_ID' ORDER BY created_at DESC")
echo "$explain_events" | grep -q "alert_events_alert" \
    && echo "index ok: alert_events by alert_id" || { echo "MISSING INDEX: alert_events alert_id" >&2; exit 1; }

# --- WS fan-out ---------------------------------------------------------------
echo
echo "=== WS fan-out (10 clients) ==="
psql -h "$PGHOST" -d perf -tA -c \
    "SELECT json_build_object('alertId', id, 'env', env, 'kind', 'created', 'title', title, 'severity', severity, 'status', status)::text FROM alerts WHERE id = '$ALERT_ID'" \
    > "$T/ws-payload.txt"
SESSION_COOKIE=$(grep "SESSION" "$COOKIES" | awk '{print $NF}' | tail -1)
if [ -z "$SESSION_COOKIE" ]; then
    echo "no SESSION cookie in jar" >&2; exit 1
fi
fanout_ms=$(python3 - "$PORT" "$SESSION_COOKIE" "$T/ws-payload.txt" "$DATABASE_URL" "$PGHOST" <<'PY'
import base64
import json
import os
import socket
import subprocess
import sys
import time

port = int(sys.argv[1])
cookie = sys.argv[2]
payload = open(sys.argv[3]).read().strip()
db_host = sys.argv[5]

def ws_connect():
    sock = socket.create_connection(("127.0.0.1", port), timeout=10)
    key = base64.b64encode(os.urandom(16)).decode()
    req = (f"GET /ws HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\nUpgrade: websocket\r\n"
           f"Connection: Upgrade\r\nSec-WebSocket-Key: {key}\r\nSec-WebSocket-Version: 13\r\n"
           f"Cookie: SESSION={cookie}\r\n\r\n")
    sock.sendall(req.encode())
    buf = b""
    while b"\r\n\r\n" not in buf:
        buf += sock.recv(4096)
    assert b"101" in buf.split(b"\r\n")[0], buf.split(b"\r\n")[0]
    sock.settimeout(5)
    return sock

def read_frame(sock):
    header = b""
    while len(header) < 2:
        chunk = sock.recv(2 - len(header))
        if not chunk:
            raise ConnectionError("closed")
        header += chunk
    length = header[1] & 0x7F
    if length == 126:
        ext = b""
        while len(ext) < 2:
            ext += sock.recv(2 - len(ext))
        length = int.from_bytes(ext, "big")
    elif length == 127:
        ext = b""
        while len(ext) < 8:
            ext += sock.recv(8 - len(ext))
        length = int.from_bytes(ext, "big")
    body = b""
    while len(body) < length:
        chunk = sock.recv(length - len(body))
        if not chunk:
            raise ConnectionError("closed")
        body += chunk
    return body

def send_frame(sock, text):
    data = text.encode()
    mask = os.urandom(4)
    header = bytearray([0x81])
    n = len(data)
    if n < 126:
        header.append(0x80 | n)
    elif n < 65536:
        header.append(0x80 | 126)
        header += n.to_bytes(2, "big")
    else:
        header.append(0x80 | 127)
        header += n.to_bytes(8, "big")
    masked = bytes(b ^ mask[i % 4] for i, b in enumerate(data))
    sock.sendall(bytes(header) + mask + masked)

clients = [ws_connect() for _ in range(10)]
try:
    for sock in clients:
        send_frame(sock, '{"type":"env","name":"perf-env-0"}')
    time.sleep(0.2)
    start = time.monotonic()
    subprocess.run(["psql", "-h", db_host, "-d", "perf", "-q", "-c",
                    f"SELECT pg_notify('halemans_events', '{payload}')"], check=True,
                   stdout=subprocess.DEVNULL)
    worst = 0.0
    for sock in clients:
        read_frame(sock)
        worst = max(worst, time.monotonic() - start)
    print(f"{worst * 1000:.1f}")
finally:
    for sock in clients:
        sock.close()
PY
)
echo "ws fan-out to 10 clients: ${fanout_ms} ms"

# --- assertions (§15) -----------------------------------------------------------
fail=0
python3 - "$dash_p95" <<'PY' || fail=1
import sys
p95 = float(sys.argv[1])
assert p95 < 300, f"dashboard p95 {p95}ms >= 300ms"
PY
python3 - "$fanout_ms" <<'PY' || fail=1
import sys
ms = float(sys.argv[1])
assert ms < 1000, f"ws fan-out {ms}ms >= 1000ms"
PY

if [ "$fail" = 0 ]; then
    echo "perf harness: OK (dashboard p95 ${dash_p95}ms < 300ms, ws fan-out ${fanout_ms}ms < 1s)"
else
    echo "perf harness: TARGET MISSED" >&2
    exit 1
fi
