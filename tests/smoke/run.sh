#!/usr/bin/env bash
# Halemans milestone-0 smoke suite (design_docs/milestone_0.md §6).
# Runs against the stack booted by `devenv up`. Invoke via `smoke-test`.
set -uo pipefail

APP_URL="${HALEMANS_APP_URL:-http://127.0.0.1:28080}"
ZABBIX_URL="${HALEMANS_ZABBIX_URL:-http://127.0.0.1:10080}"
GRAFANA_URL="${HALEMANS_GRAFANA_URL:-http://127.0.0.1:3001}"
AM_URL="${HALEMANS_ALERTMANAGER_URL:-http://127.0.0.1:9093}"
MOCK_JIRA_URL="${HALEMANS_JIRA_URL:-http://127.0.0.1:18083}"
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

# milestone 2: dual-path grafana (webhook fast path + poller reconcile, §8).
wait_sql "grafana poller loop runs" 90 "SELECT 1 FROM poll_grafana_jobs WHERE status = 'job_status_succeeded' LIMIT 1" \
    && pass "grafana poller loop runs" || fail "grafana poller loop runs"
grafana_fp=$(psql "$DATABASE_URL" -tA -c "SELECT fingerprint FROM alerts WHERE fingerprint LIKE 'grafana:%' ORDER BY created_at DESC LIMIT 1" 2>/dev/null)
[ -n "$grafana_fp" ] \
    && [ "$(psql "$DATABASE_URL" -tA -c "SELECT count(*) FROM alerts WHERE fingerprint = '$grafana_fp'" 2>/dev/null)" = "1" ] \
    && pass "webhook+poller dedupe onto one alert" || fail "webhook+poller dedupe onto one alert"

# milestone 2: correlation — seeded env+host grouping rule rolled the alerts
# into a group (§6).
wait_sql "alerts grouped" 60 "SELECT 1 FROM alert_groups WHERE member_count >= 1 LIMIT 1" \
    && pass "alert groups rollup maintained" || fail "alert groups rollup maintained"
[ -n "$(psql "$DATABASE_URL" -tA -c "SELECT 1 FROM alerts WHERE group_id IS NOT NULL LIMIT 1" 2>/dev/null)" ] \
    && pass "alerts carry group_id" || fail "alerts carry group_id"

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
    for admin_path in admin/teams admin/grouping-rules admin/notification-rules admin/escalation-policies; do
        [ "$(curl -s -b "$COOKIES" -o /dev/null -w '%{http_code}' "$APP_URL/$admin_path")" = "403" ] \
            && pass "viewer 403 on /$admin_path" || fail "viewer 403 on /$admin_path"
    done
    login_as "sre@dev" "$(cat "$STATE/halemans/sre-password")" > /dev/null 2>&1
else
    fail "rbac (no firing alert to probe)"
fi

# ------------------------------------------------- grafana poller reconcile
# milestone 2 §8: with the webhook token removed, the poller must still pick
# up the fire AND the resolve (single fingerprint, no duplicate).
scenario "grafana poller reconcile"
generic_token=$(cat "$STATE/halemans/generic-hook-token")
grafana_source_id=$(psql "$DATABASE_URL" -tA -c "SELECT id FROM sources WHERE type = 'grafana' LIMIT 1")
psql "$DATABASE_URL" -c "DELETE FROM webhook_tokens WHERE token = '$generic_token'" > /dev/null 2>&1
marker=$(psql "$DATABASE_URL" -tA -c "SELECT now()")
fire-test-alert-grafana || fail "reconcile: fire threshold set"
# The dev-cpu-sim fingerprint is stable, so this refires the earlier resolved
# alert (last_seen_at moves), it does not create a new row.
if wait_sql "poller refired the alert (webhook dead)" 120 "SELECT 1 FROM alerts WHERE fingerprint LIKE 'grafana:%' AND last_seen_at > '$marker' AND status = 'firing' LIMIT 1"; then
    pass "poller reconcile: firing state arrived without webhook"
else
    fail "poller reconcile: firing state arrived without webhook"
fi
reconcile_fp=$(psql "$DATABASE_URL" -tA -c "SELECT fingerprint FROM alerts WHERE fingerprint LIKE 'grafana:%' AND last_seen_at > '$marker' ORDER BY last_seen_at DESC LIMIT 1" 2>/dev/null)
fire-test-alert-grafana resolve || fail "reconcile: resolve threshold set"
# absence-based reconcile has a 60s grace window against listing lag
wait_sql "poller resolved the alert" 180 "SELECT 1 FROM alerts WHERE fingerprint = '$reconcile_fp' AND status = 'resolved' AND last_seen_at > '$marker' LIMIT 1" \
    && pass "poller reconcile: resolve arrived without webhook" || fail "poller reconcile: resolve arrived without webhook"
[ -n "$reconcile_fp" ] \
    && [ "$(psql "$DATABASE_URL" -tA -c "SELECT count(*) FROM alerts WHERE fingerprint = '$reconcile_fp'" 2>/dev/null)" = "1" ] \
    && pass "poller reconcile: single fingerprint, no duplicate" || fail "poller reconcile: single fingerprint, no duplicate"
# restore the webhook token (same statement as seed-halemans)
psql "$DATABASE_URL" -c "INSERT INTO webhook_tokens (source_id, token) VALUES ('$grafana_source_id', '$generic_token') ON CONFLICT (token) DO NOTHING" > /dev/null 2>&1 \
    && pass "webhook token restored" || fail "webhook token restored"

# ---------------------------------------------------------- milestone 3
# The zabbix-backed scenarios share one alert row (fingerprint is
# trigger-scoped, so refiring reuses it). $enrich_id threads through
# scenarios 1-5.
enrich_id=""
zbx_event=""
if [ "${SMOKE_ZABBIX:-1}" != 0 ]; then
scenario "enrichment"
fire-test-alert-zabbix || fail "enrichment: fire push accepted"
if wait_sql "enrichment alert firing" 60 "SELECT 1 FROM alerts WHERE fingerprint LIKE 'zabbix:trigger:%' AND title = 'halemans test trigger' AND status = 'firing' LIMIT 1"; then
    enrich_id=$(alert_id_by_fingerprint_prefix "zabbix:trigger:" "halemans test trigger")
fi
if [ -n "$enrich_id" ] \
    && wait_sql "cmdb entry cached for dev-host-01" 60 "SELECT 1 FROM cmdb_entries c JOIN hosts h ON h.id = c.host_id WHERE h.fqdn = 'dev-host-01' LIMIT 1" \
    && wait_sql "jira auto-link DEV-101" 60 "SELECT 1 FROM jira_links WHERE alert_id = '$enrich_id' AND ticket_key = 'DEV-101' AND origin = 'auto' LIMIT 1"; then
    pass "enrichment: cmdb cache row + jira auto-link"
else
    fail "enrichment: cmdb cache row + jira auto-link"
fi
login_as "sre@dev" "$(cat "$STATE/halemans/sre-password")" > /dev/null 2>&1
if [ -n "$enrich_id" ]; then
    card_html=$(curl -sf -b "$COOKIES" "$APP_URL/alerts/$enrich_id")
    echo "$card_html" | grep -q 'cmdb-panel' && echo "$card_html" | grep -q 'jira-link' \
        && pass "enrichment: card renders cmdb + jira panels" || fail "enrichment: card renders cmdb + jira panels"
else
    fail "enrichment: card renders cmdb + jira panels (no alert)"
fi

# ------------------------------------------------- milestone 3: jira create
scenario "jira manual create"
if [ -n "$enrich_id" ]; then
    curl -sf -b "$COOKIES" -o /dev/null -X POST \
        -d "issueType=Task" -d "summary=Smoke-ticket" -d "body=smoke" \
        "$APP_URL/alerts/$enrich_id/jira" || fail "jira manual create: post"
    wait_sql "manual jira link stored" 60 "SELECT 1 FROM jira_links WHERE alert_id = '$enrich_id' AND origin = 'manual' LIMIT 1" \
        && pass "jira manual create: link stored (origin manual)" || fail "jira manual create: link stored (origin manual)"
    # key comes from the stored link (DEV-102 on a fresh mock, later keys on a
    # reused dev-stack mock)
    manual_key=$(psql "$DATABASE_URL" -tA -c "SELECT ticket_key FROM jira_links WHERE alert_id = '$enrich_id' AND origin = 'manual' ORDER BY created_at DESC LIMIT 1" 2>/dev/null)
    [ -n "$manual_key" ] \
        && curl -sf -H "Authorization: Bearer test-jira-token" "$MOCK_JIRA_URL/rest/api/3/issue/$manual_key" | grep -q 'Smoke-ticket' \
        && pass "jira manual create: ticket exists in mock jira" || fail "jira manual create: ticket exists in mock jira"
else
    fail "jira manual create (no alert)"
fi

# ------------------------------------------------- milestone 3: jira sync
scenario "jira sync drift"
if [ -n "$enrich_id" ]; then
    curl -sf -H 'Content-Type: application/json' -d '{"status":"In Progress"}' \
        "$MOCK_JIRA_URL/debug/issue/DEV-101/status" > /dev/null || fail "jira sync drift: mock backdoor set"
    # the self-rescheduler runs every 5min; a fresh row with default run_at
    # is picked up promptly by the worker
    psql "$DATABASE_URL" -c "INSERT INTO jira_sync_jobs DEFAULT VALUES" > /dev/null 2>&1
    wait_sql "jira link status drifted to In Progress" 90 "SELECT 1 FROM jira_links WHERE alert_id = '$enrich_id' AND ticket_key = 'DEV-101' AND status = 'In Progress' LIMIT 1" \
        && pass "jira sync drift: status mirrored" || fail "jira sync drift: status mirrored"
    curl -sf -H 'Content-Type: application/json' -d '{"status":"Open"}' \
        "$MOCK_JIRA_URL/debug/issue/DEV-101/status" > /dev/null || fail "jira sync drift: mock backdoor restore"
    psql "$DATABASE_URL" -c "INSERT INTO jira_sync_jobs DEFAULT VALUES" > /dev/null 2>&1
    wait_sql "jira link status restored to Open" 90 "SELECT 1 FROM jira_links WHERE alert_id = '$enrich_id' AND ticket_key = 'DEV-101' AND status = 'Open' LIMIT 1" \
        && pass "jira sync drift: status restored" || fail "jira sync drift: status restored"
else
    fail "jira sync drift (no alert)"
fi
else
    echo "scenario: enrichment / jira (skipped, SMOKE_ZABBIX=0)"
fi

# ------------------------------------------------- milestone 3: write-back
scenario "write-back ack"
fire-test-alert-alertmanager || fail "write-back ack: fire posted"
am_id=""
# count of non-expired write-back silences in the alertmanager (0 on any error)
silence_count() {
    local n
    n=$(curl -sf "$AM_URL/api/v2/silences" 2>/dev/null | jq '[.[] | select(.comment | contains("ack via Halemans")) | select(.status.state != "expired")] | length' 2>/dev/null)
    echo "${n:-0}"
}
if wait_sql "write-back ack: am alert firing" 60 "SELECT 1 FROM alerts WHERE fingerprint LIKE 'alertmanager:%' AND title = 'Alertmanager dev test alert' AND status = 'firing' LIMIT 1"; then
    am_id=$(alert_id_by_fingerprint_prefix "alertmanager:" "Alertmanager dev test alert")
fi
login_as "sre@dev" "$(cat "$STATE/halemans/sre-password")" > /dev/null 2>&1
if [ -n "$am_id" ]; then
    curl -sf -b "$COOKIES" -o /dev/null -X POST "$APP_URL/alerts/$am_id/ack" || fail "write-back ack: am ack post"
    if wait_sql "am ack attempt done" 90 "SELECT 1 FROM write_back_attempts WHERE alert_id = '$am_id' AND action = 'ack' AND status = 'done' LIMIT 1" \
        && [ "$(silence_count)" -ge 1 ]; then
        pass "write-back ack: silence present in alertmanager"
    else
        fail "write-back ack: silence present in alertmanager"
    fi
    curl -sf -b "$COOKIES" -o /dev/null -X POST "$APP_URL/alerts/$am_id/unack" || fail "write-back ack: am unack post"
    silence_gone=0
    if wait_sql "am unack attempt done" 90 "SELECT 1 FROM write_back_attempts WHERE alert_id = '$am_id' AND action = 'unack' AND status = 'done' LIMIT 1"; then
        for i in $(seq 1 30); do
            if [ "$(silence_count)" = "0" ]; then
                silence_gone=1
                break
            fi
            sleep 1
        done
    fi
    [ "$silence_gone" = 1 ] && pass "write-back unack: silence removed" || fail "write-back unack: silence removed"
    fire-test-alert-alertmanager resolve > /dev/null || fail "write-back ack: resolve posted"
    wait_sql "am alert resolved" 60 "SELECT 1 FROM alerts WHERE id = '$am_id' AND status = 'resolved' LIMIT 1" \
        && pass "write-back ack: am alert resolved" || fail "write-back ack: am alert resolved"
else
    fail "write-back ack (no am alert)"
fi

if [ "${SMOKE_ZABBIX:-1}" != 0 ]; then
zbx_token="${ZABBIX_TOKEN:-$(cat "$STATE/zabbix/token")}"
if [ -n "$enrich_id" ]; then
    zbx_event=$(psql "$DATABASE_URL" -tA -c "SELECT external_id FROM alerts WHERE id = '$enrich_id'" 2>/dev/null)
    curl -sf -b "$COOKIES" -o /dev/null -X POST "$APP_URL/alerts/$enrich_id/ack" || fail "write-back ack: zabbix ack post"
    if wait_sql "zabbix ack attempt done" 90 "SELECT 1 FROM write_back_attempts WHERE alert_id = '$enrich_id' AND action = 'ack' AND status = 'done' LIMIT 1" \
        && [ "$(curl -sf -H 'Content-Type: application/json' -H "Authorization: Bearer $zbx_token" \
            -d "$(jq -n --arg e "$zbx_event" '{jsonrpc:"2.0",method:"event.get",params:{eventids:[$e],selectAcknowledges:"extend"},id:1}')" \
            "$ZABBIX_URL/api_jsonrpc.php" | jq -r '.result[0].acknowledged // empty')" = "1" ]; then
        pass "write-back ack: zabbix event acknowledged"
    else
        fail "write-back ack: zabbix event acknowledged"
    fi
    curl -sf -b "$COOKIES" -o /dev/null -X POST "$APP_URL/alerts/$enrich_id/unack" || fail "write-back ack: zabbix unack post"
    if wait_sql "zabbix unack attempt done" 90 "SELECT 1 FROM write_back_attempts WHERE alert_id = '$enrich_id' AND action = 'unack' AND status = 'done' LIMIT 1" \
        && [ "$(curl -sf -H 'Content-Type: application/json' -H "Authorization: Bearer $zbx_token" \
            -d "$(jq -n --arg e "$zbx_event" '{jsonrpc:"2.0",method:"event.get",params:{eventids:[$e],selectAcknowledges:"extend"},id:1}')" \
            "$ZABBIX_URL/api_jsonrpc.php" | jq -r '.result[0].acknowledged // empty')" = "0" ]; then
        pass "write-back unack: zabbix event unacknowledged"
    else
        fail "write-back unack: zabbix event unacknowledged"
    fi
else
    fail "write-back ack (no zabbix alert)"
fi

# --------------------------------------- milestone 3: external ack reconcile
scenario "external ack reconcile (zabbix)"
# refires the same trigger-scoped fingerprint (fire script is idempotent)
fire-test-alert-zabbix || fail "reconcile: fire push accepted"
if [ -n "$enrich_id" ] && [ -n "$zbx_event" ] \
    && wait_sql "reconcile: alert refired" 60 "SELECT 1 FROM alerts WHERE id = '$enrich_id' AND status = 'firing' LIMIT 1"; then
    curl -sf -H 'Content-Type: application/json' -H "Authorization: Bearer $zbx_token" \
        -d "$(jq -n --arg e "$zbx_event" '{jsonrpc:"2.0",method:"event.acknowledge",params:{eventids:[$e],action:6,message:"smoke"},id:1}')" \
        "$ZABBIX_URL/api_jsonrpc.php" | grep -q '"result"' || fail "reconcile: zabbix ack api"
    wait_sql "reconcile: external ack mirrored" 60 "SELECT 1 FROM alerts a WHERE a.id = '$enrich_id' AND a.status = 'ack' AND EXISTS (SELECT 1 FROM alert_events e WHERE e.alert_id = a.id AND e.kind = 'external' AND e.payload->>'source' = 'zabbix' AND e.payload->>'action' = 'ack')" \
        && pass "reconcile: source ack mirrored (external event)" || fail "reconcile: source ack mirrored (external event)"
    # zabbix ack clocks are second-precision; keep sourceAt strictly newer
    # than the local mirror's acknowledgedAt for the un-mirror LWW check
    sleep 2
    curl -sf -H 'Content-Type: application/json' -H "Authorization: Bearer $zbx_token" \
        -d "$(jq -n --arg e "$zbx_event" '{jsonrpc:"2.0",method:"event.acknowledge",params:{eventids:[$e],action:20,message:"smoke unack"},id:1}')" \
        "$ZABBIX_URL/api_jsonrpc.php" | grep -q '"result"' || fail "reconcile: zabbix unack api"
    wait_sql "reconcile: external unack mirrored" 60 "SELECT 1 FROM alerts WHERE id = '$enrich_id' AND status = 'firing' LIMIT 1" \
        && pass "reconcile: source unack mirrored" || fail "reconcile: source unack mirrored"
    fire-test-alert-zabbix resolve > /dev/null || fail "reconcile: resolve push accepted"
    wait_sql "reconcile: alert resolved" 60 "SELECT 1 FROM alerts WHERE id = '$enrich_id' AND status = 'resolved' LIMIT 1" \
        && pass "reconcile: alert resolved" || fail "reconcile: alert resolved"
else
    fail "external ack reconcile (no alert)"
fi
else
    echo "scenario: write-back ack (zabbix) / external ack reconcile (skipped, SMOKE_ZABBIX=0)"
fi

# -------------------------------------------- milestone 3: write-back chip
scenario "write-back failure chip"
dead_insert_out=$(psql "$DATABASE_URL" -tA -c "INSERT INTO sources (type, name, base_url, config) VALUES ('zabbix', 'smoke-dead-source', 'http://127.0.0.1:9', '{\"writeBack\": true}'::jsonb) RETURNING id" 2>&1)
dead_source_id=$(printf '%s\n' "$dead_insert_out" | grep -oE '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' | head -1)
dead_id=""
if [ -n "$dead_source_id" ]; then
    dead_insert_out=$(psql "$DATABASE_URL" -tA -c "INSERT INTO alerts (fingerprint, source_id, external_id, title, severity, status) VALUES ('smoke:dead-source', '$dead_source_id', '999', 'smoke dead source alert', 'warning', 'firing') RETURNING id" 2>&1)
    dead_id=$(printf '%s\n' "$dead_insert_out" | grep -oE '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' | head -1)
fi
if [ -z "$dead_source_id" ]; then echo "source insert error: $dead_insert_out" >&2; fi
if [ -n "$dead_source_id" ] && [ -z "$dead_id" ]; then echo "alert insert error: $dead_insert_out" >&2; fi
login_as "sre@dev" "$(cat "$STATE/halemans/sre-password")" > /dev/null 2>&1
if [ -n "$dead_id" ]; then
    curl -sf -b "$COOKIES" -o /dev/null -X POST "$APP_URL/alerts/$dead_id/ack" || fail "write-back failure chip: ack post"
    # backoff env is flattened to 0s by smoke-check, so retries burn through
    # max attempts quickly
    if wait_sql "dead-source attempt failed" 120 "SELECT 1 FROM write_back_attempts WHERE alert_id = '$dead_id' AND status = 'failed' LIMIT 1"; then
        card_html=$(curl -sf -b "$COOKIES" "$APP_URL/alerts/$dead_id")
        echo "$card_html" | grep -q 'writeback-status' && echo "$card_html" | grep -q 'failed' \
            && pass "write-back failure chip: card shows failed chip" || fail "write-back failure chip: card shows failed chip"
    else
        fail "write-back failure chip: attempt never failed"
    fi
    psql "$DATABASE_URL" -c "DELETE FROM alert_events WHERE alert_id = '$dead_id'; DELETE FROM write_back_attempts WHERE alert_id = '$dead_id'; DELETE FROM alerts WHERE id = '$dead_id'; DELETE FROM sources WHERE id = '$dead_source_id'" > /dev/null 2>&1
else
    fail "write-back failure chip (insert failed)"
fi

echo
if [ "$failures" = 0 ]; then
    echo "smoke: all scenarios passed"
else
    echo "smoke: $failures failure(s)"
fi
exit "$failures"
