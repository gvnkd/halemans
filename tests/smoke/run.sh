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

# Auth (milestone 1): UI assertions run as sre@dev in a cookie jar.
COOKIES="$TMPDIR/halemans-smoke-cookies.txt"
login_as() { # <email> <password>
    rm -f "$COOKIES"
    curl -sf -c "$COOKIES" "$APP_URL/NewSession" > /dev/null || return 1
    curl -sf -b "$COOKIES" -c "$COOKIES" -o /dev/null \
        -d "email=$1" -d "password=$2" "$APP_URL/CreateSession" || return 1
    curl -sf -b "$COOKIES" -o /dev/null "$APP_URL/"
}

# assert_alert_card <title-substring> <fingerprint-prefix>
assert_alert_card() {
    local title="$1" prefix="$2" id
    id=$(alert_id_by_fingerprint_prefix "$prefix" "$title")
    if [ -z "$id" ]; then
        return 1
    fi
    curl -sf -b "$COOKIES" "$APP_URL/alerts/$id" | grep -q "$title"
}

scenario() { printf 'scenario: %s\n' "$1"; }

# ---------------------------------------------------------------- stack health
scenario "stack health"
[ "$(curl -s -o /dev/null -w '%{http_code}' "$APP_URL/alerts")" = "302" ] \
    && pass "anonymous /alerts redirects to login" || fail "anonymous /alerts redirects to login"
login_as "sre@dev" "$(cat "$STATE/halemans/sre-password")" \
    && pass "login as sre@dev" || fail "login as sre@dev"
curl -sf -b "$COOKIES" "$APP_URL/alerts" > /dev/null && pass "app reachable (authed)" || fail "app reachable (authed)"
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

# ---------------------------------------------------------------- rbac
scenario "rbac"
# All source scenarios above end resolved; fire a fresh alert to probe against.
generic_token=$(cat "$STATE/halemans/generic-hook-token")
now_rfc=$(date -u +%Y-%m-%dT%H:%M:%SZ)
curl -sf -H 'Content-Type: application/json' -d "{\"version\":\"4\",\"status\":\"firing\",\"receiver\":\"halemans\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertname\":\"rbac-probe\",\"env\":\"dev\",\"host\":\"dev-host-01\",\"severity\":\"warning\",\"check\":\"rbac\"},\"annotations\":{\"summary\":\"rbac probe\"},\"startsAt\":\"$now_rfc\",\"fingerprint\":\"rbac-probe-$RANDOM\"}]}" \
    "$APP_URL/hooks/generic/$generic_token" > /dev/null || fail "rbac: probe alert post"
rbac_fp=$(psql "$DATABASE_URL" -tA -c "SELECT fingerprint FROM alerts WHERE check_name = 'rbac' ORDER BY created_at DESC LIMIT 1" 2>/dev/null)
wait_sql "rbac probe alert firing" 30 "SELECT 1 FROM alerts WHERE fingerprint = '$rbac_fp' AND status = 'firing' LIMIT 1" || true
firing_id=$(psql "$DATABASE_URL" -tA -c "SELECT id FROM alerts WHERE fingerprint = '$rbac_fp' LIMIT 1" 2>/dev/null)
if [ -n "$firing_id" ]; then
    rm -f "$COOKIES"
    login_as "viewer@dev" "$(cat "$STATE/halemans/viewer-password")" > /dev/null 2>&1
    [ "$(curl -s -b "$COOKIES" -o /dev/null -w '%{http_code}' -X POST "$APP_URL/alerts/$firing_id/ack")" = "403" ] \
        && pass "viewer cannot ack (403)" || fail "viewer cannot ack (403)"
    [ "$(curl -s -b "$COOKIES" -o /dev/null -w '%{http_code}' -X POST "$APP_URL/alerts/$firing_id/close")" = "403" ] \
        && pass "viewer cannot close (403)" || fail "viewer cannot close (403)"
    login_as "sre@dev" "$(cat "$STATE/halemans/sre-password")" > /dev/null 2>&1
else
    fail "rbac (no firing alert to probe)"
fi

echo
if [ "$failures" = 0 ]; then
    echo "smoke: all scenarios passed"
else
    echo "smoke: $failures failure(s)"
fi
exit "$failures"
