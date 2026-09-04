#!/usr/bin/env bash
# Halemans milestone-0 smoke suite (design_docs/milestone_0.md §6).
# Runs against the stack booted by `devenv up`. Invoke via `smoke-test`.
set -uo pipefail

APP_URL="${HALEMANS_APP_URL:-http://127.0.0.1:28080}"
ZABBIX_URL="${HALEMANS_ZABBIX_URL:-http://127.0.0.1:10080}"
GRAFANA_URL="${HALEMANS_GRAFANA_URL:-http://127.0.0.1:3001}"
AM_URL="${HALEMANS_ALERTMANAGER_URL:-http://127.0.0.1:9093}"
STATE="${DEVENV_STATE:?}"

failures=0
pass() { printf '  PASS %s\n' "$1"; }
fail() { printf '  FAIL %s\n' "$1"; failures=$((failures + 1)); }

# wait_sql <description> <timeout-seconds> <sql-returning-nonempty-when-done>
wait_sql() {
    local desc="$1" timeout="$2" sql="$3" i
    for i in $(seq 1 "$timeout"); do
        if [ -n "$(psql "${DATABASE_URL:?}" -tA -c "$sql" 2>/dev/null)" ]; then
            return 0
        fi
        sleep 1
    done
    echo "  timeout waiting for: $desc" >&2
    return 1
}

alert_id_by_fingerprint_prefix() { # <prefix> <title>
    psql "$DATABASE_URL" -tA -c \
        "SELECT id FROM alerts WHERE fingerprint LIKE '$1%' AND title = '$2' ORDER BY created_at DESC LIMIT 1" 2>/dev/null
}

# assert_alert_card <title-substring> <fingerprint-prefix>
assert_alert_card() {
    local title="$1" prefix="$2" id
    id=$(alert_id_by_fingerprint_prefix "$prefix" "$title")
    if [ -z "$id" ]; then
        return 1
    fi
    curl -sf "$APP_URL/alerts/$id" | grep -q "$title"
}

scenario() { printf 'scenario: %s\n' "$1"; }

# ---------------------------------------------------------------- stack health
scenario "stack health"
curl -sf "$APP_URL/alerts" > /dev/null && pass "app reachable" || fail "app reachable"
psql "$DATABASE_URL" -tA -c "SELECT 1" > /dev/null 2>&1 && pass "app db reachable" || fail "app db reachable"
[ -n "$(psql "$DATABASE_URL" -tA -c "SELECT 1 FROM poll_zabbix_jobs WHERE status = 'job_status_succeeded' LIMIT 1" 2>/dev/null)" ] \
    && pass "worker runs poller jobs" || fail "worker runs poller jobs"
if [ "${SMOKE_ZABBIX:-1}" != 0 ]; then
    curl -sf -H 'Content-Type: application/json' -H "Authorization: Bearer $(cat "$STATE/zabbix/token")" \
        -d '{"jsonrpc":"2.0","method":"problem.get","params":{"limit":1},"id":1}' \
        "$ZABBIX_URL/api_jsonrpc.php" | grep -q '"result"' && pass "zabbix api + token" || fail "zabbix api + token"
fi
curl -sf -H "Authorization: Bearer $(cat "$STATE/grafana/token")" \
    "$GRAFANA_URL/api/serviceaccounts/search?query=halemans-dev" > /dev/null \
    && pass "grafana api + token" || fail "grafana api + token"
curl -sf "$AM_URL/-/healthy" > /dev/null && pass "alertmanager healthy" || fail "alertmanager healthy"

# ---------------------------------------------------------------- zabbix
# SMOKE_ZABBIX=0 is kept for running a native-only subset manually.
if [ "${SMOKE_ZABBIX:-1}" != 0 ]; then
scenario "zabbix"
# On a fresh boot the worker's first GHCi compile can take minutes; wait for
# the poller loop before firing so the 60s windows below stay meaningful.
wait_sql "worker poller warm" 600 "SELECT 1 FROM poll_zabbix_jobs WHERE status = 'job_status_succeeded' LIMIT 1" \
    || fail "worker poller warm"
fire-test-alert-zabbix || fail "zabbix: fire push accepted"
if wait_sql "zabbix alert in db" 60 "SELECT 1 FROM alerts WHERE fingerprint LIKE 'zabbix:trigger:%' AND title = 'halemans test trigger' AND status = 'firing' LIMIT 1" \
    && assert_alert_card "halemans test trigger" "zabbix:trigger:"; then
    pass "zabbix alert arrived and renders"
else
    fail "zabbix alert arrived and renders"
fi
fire-test-alert-zabbix resolve || fail "zabbix: resolve push accepted"
wait_sql "zabbix alert resolved" 60 "SELECT 1 FROM alerts WHERE fingerprint LIKE 'zabbix:trigger:%' AND title = 'halemans test trigger' AND status = 'resolved' LIMIT 1" \
    && pass "zabbix alert resolved" || fail "zabbix alert resolved"
else
    echo "scenario: zabbix (skipped, SMOKE_ZABBIX=0)"
fi

# ---------------------------------------------------------------- alertmanager
scenario "alertmanager"
fire-test-alert-alertmanager || fail "alertmanager: fire posted"
if wait_sql "alertmanager alert in db" 60 "SELECT 1 FROM alerts WHERE fingerprint LIKE 'alertmanager:%' AND status = 'firing' LIMIT 1" \
    && assert_alert_card "Alertmanager dev test alert" "alertmanager:"; then
    pass "alertmanager alert arrived and renders"
else
    fail "alertmanager alert arrived and renders"
fi
fire-test-alert-alertmanager resolve || fail "alertmanager: resolve posted"
wait_sql "alertmanager alert resolved" 60 "SELECT 1 FROM alerts WHERE fingerprint LIKE 'alertmanager:%' AND status = 'resolved' LIMIT 1" \
    && pass "alertmanager alert resolved" || fail "alertmanager alert resolved"

# ---------------------------------------------------------------- grafana
scenario "grafana"
fire-test-alert-grafana || fail "grafana: fire threshold set"
if wait_sql "grafana alert in db" 90 "SELECT 1 FROM alerts WHERE fingerprint LIKE 'grafana:%' AND status = 'firing' LIMIT 1" \
    && assert_alert_card "Dev CPU simulation alert" "grafana:"; then
    pass "grafana alert arrived and renders"
else
    fail "grafana alert arrived and renders"
fi
fire-test-alert-grafana resolve || fail "grafana: resolve threshold set"
wait_sql "grafana alert resolved" 90 "SELECT 1 FROM alerts WHERE fingerprint LIKE 'grafana:%' AND status = 'resolved' LIMIT 1" \
    && pass "grafana alert resolved" || fail "grafana alert resolved"

echo
if [ "$failures" = 0 ]; then
    echo "smoke: all scenarios passed"
else
    echo "smoke: $failures failure(s)"
fi
exit "$failures"
