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
failed_tests=()
pass() { printf '  PASS %s\n' "$1"; }
fail() { printf '  FAIL %s\n' "$1"; failures=$((failures + 1)); failed_tests+=("$1"); }

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
        echo "  assert-fail: no alert row titled '$title' with fingerprint prefix '$prefix'; newest matching rows:" >&2
        psql "$DATABASE_URL" -tA -c "SELECT title, status, created_at FROM alerts WHERE fingerprint LIKE '$prefix%' ORDER BY created_at DESC LIMIT 3" >&2
        return 1
    fi
    local body status
    body=$(curl -s -b "$COOKIES" -w '\n%{http_code}' "$APP_URL/alerts/$id")
    status=$(printf '%s' "$body" | tail -1)
    if [ "$status" != "200" ]; then
        echo "  assert-fail: GET /alerts/$id returned HTTP $status" >&2
        return 1
    fi
    if ! printf '%s' "$body" | grep -q "$title"; then
        echo "  assert-fail: card /alerts/$id does not render title '$title'" >&2
        return 1
    fi
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

# ------------------------------------------------- grafana metric chart
# Lazy-load widget against the mock grafana (provisioning rule + ds/query).
scenario "grafana metric chart"
login_as "sre@dev" "$(cat "$STATE/halemans/sre-password")" > /dev/null 2>&1
mock_alert_id=$(psql "$DATABASE_URL" -tA -c "SELECT id FROM alerts WHERE fingerprint = 'grafana-mock-rule-cpu' LIMIT 1" 2>/dev/null)
if [ -n "$mock_alert_id" ]; then
    show_body=$(curl -s -b "$COOKIES" "$APP_URL/alerts/$mock_alert_id")
    printf '%s' "$show_body" | grep -q 'data-testid="metric-chart-load"' \
        && pass "metric chart button renders on alert page" || fail "metric chart button renders on alert page"
    chart_body=$(curl -s -b "$COOKIES" "$APP_URL/alerts/$mock_alert_id/metrics-chart")
    printf '%s' "$chart_body" | grep -q 'data-testid="metric-chart-svg"' \
        && pass "metric chart endpoint returns SVG" || fail "metric chart endpoint returns SVG"
    printf '%s' "$chart_body" | grep -q 'instance-1' \
        && pass "metric chart renders mock series" || fail "metric chart renders mock series"
else
    fail "grafana metric chart (no mock alert row)"
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

# ------------------------------------------------- milestone 4: llm analysis
scenario "llm analysis"
login_as "sre@dev" "$(cat "$STATE/halemans/sre-password")" > /dev/null 2>&1
# unique fingerprint: a fresh alert gets its own analysis row (refires of an
# existing alert never enqueue one, and its event log would have drifted, so
# no dedupe hit — integration covers the identical-context copy precisely)
llm_fp="smoke-llm-$(date +%s)"
curl -sf -X POST "$APP_URL/hooks/generic/$(cat "$STATE/halemans/generic-hook-token")" \
    -H 'Content-Type: application/json' \
    -d "{\"version\":\"4\",\"status\":\"firing\",\"receiver\":\"halemans\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertname\":\"smoke-llm\",\"env\":\"dev\",\"host\":\"dev-host-01\",\"severity\":\"high\",\"check\":\"smoke-llm\"},\"annotations\":{\"summary\":\"smoke llm analysis probe\"},\"startsAt\":\"$(date -u +%Y-%m-%dT%H:%M:%SZ)\",\"fingerprint\":\"$llm_fp\"}]}" > /dev/null \
    || fail "llm analysis: fire posted"
llm_id=""
if wait_sql "llm analysis: alert firing" 60 "SELECT 1 FROM alerts WHERE fingerprint = 'grafana:$llm_fp' AND status = 'firing' LIMIT 1"; then
    llm_id=$(psql "$DATABASE_URL" -tA -c "SELECT id FROM alerts WHERE fingerprint = 'grafana:$llm_fp' LIMIT 1" 2>/dev/null)
fi
if [ -n "$llm_id" ]; then
    wait_sql "llm analysis done" 120 "SELECT 1 FROM llm_analyses WHERE alert_id = '$llm_id' AND status = 'done' LIMIT 1" \
        && pass "llm analysis: done" || fail "llm analysis: done"
    wait_sql "llm result fields populated" 30 "SELECT 1 FROM llm_analyses WHERE alert_id = '$llm_id' AND status = 'done' AND result_md IS NOT NULL AND result ? 'probable_cause' AND tokens_in IS NOT NULL LIMIT 1" \
        && pass "llm analysis: result fields" || fail "llm analysis: result fields"
    card_html=$(curl -sf -b "$COOKIES" "$APP_URL/alerts/$llm_id" || true)
    echo "$card_html" | grep -q 'llm-panel' && echo "$card_html" | grep -q 'llm-probable-cause' \
        && pass "llm analysis: card panel renders" || fail "llm analysis: card panel renders"
    # re-analyze appends a fresh row and the card shows the latest (dedupe of
    # identical context is asserted deterministically in the integration
    # suite; here the grafana-absence reconcile can add events mid-scenario)
    curl -sf -b "$COOKIES" -o /dev/null -X POST "$APP_URL/alerts/$llm_id/reanalyze" || fail "llm analysis: re-analyze post"
    wait_sql "re-analyze appended" 90 "SELECT 1 FROM (SELECT 1, COUNT(*) OVER () AS n FROM llm_analyses WHERE alert_id = '$llm_id' AND status = 'done') t WHERE n >= 2 LIMIT 1" \
        && pass "llm analysis: re-analyze appended" || fail "llm analysis: re-analyze appended"    # feedback round-trip: up-vote then re-vote down
    analysis_id=$(psql "$DATABASE_URL" -tA -c "SELECT id FROM llm_analyses WHERE alert_id = '$llm_id' AND status = 'done' ORDER BY created_at DESC LIMIT 1" 2>/dev/null | grep -oE '[0-9a-f-]{36}' | head -1)
    if [ -n "$analysis_id" ]; then
        curl -sf -b "$COOKIES" -o /dev/null -X POST -d "score=1" "$APP_URL/alerts/$llm_id/analyses/$analysis_id/feedback" || fail "llm feedback: up-vote post"
        curl -sf -b "$COOKIES" -o /dev/null -X POST -d "score=-1" "$APP_URL/alerts/$llm_id/analyses/$analysis_id/feedback" || fail "llm feedback: down-vote post"
        vote=$(psql "$DATABASE_URL" -tA -c "SELECT score FROM llm_feedback WHERE analysis_id = '$analysis_id'" 2>/dev/null)
        [ "$vote" = "-1" ] \
            && pass "llm feedback: re-vote flips score" || fail "llm feedback: re-vote flips score (got '$vote')"
    else
        fail "llm feedback (no analysis id)"
    fi
    # ------------------------------------------------- milestone 8: assets
    # dev-host-01 is seeded in the mock Assets Capacity CMDB; the enrich job
    # caches + links it, the card panel renders from the cache, and the
    # assets excerpt reaches the provider (the mock echoes it back).
    wait_sql "assets: object cached and linked" 90 "SELECT 1 FROM asset_alert_links l JOIN assets_objects o ON o.id = l.assets_object_id WHERE l.alert_id = '$llm_id' AND o.label = 'dev-host-01' LIMIT 1" \
        && pass "assets: dev-host-01 cached and linked" || fail "assets: dev-host-01 cached and linked"
    curl -sf -b "$COOKIES" -o /dev/null -X POST "$APP_URL/alerts/$llm_id/reanalyze" || fail "assets: re-analyze post"
    wait_sql "assets: analysis reflects owner/cluster" 120 "SELECT 1 FROM llm_analyses WHERE alert_id = '$llm_id' AND status = 'done' AND result_md LIKE '%team-sre%' AND result_md LIKE '%prod-eu-1%' LIMIT 1" \
        && pass "assets: llm analysis text reflects owner/cluster" || fail "assets: llm analysis text reflects owner/cluster"
    card_html=$(curl -sf -b "$COOKIES" "$APP_URL/alerts/$llm_id" || true)
    echo "$card_html" | grep -q 'assets-panel' && echo "$card_html" | grep -q 'CHCMDB-10001' && echo "$card_html" | grep -q 'team-sre' \
        && pass "assets: card panel renders asset fields" || fail "assets: card panel renders asset fields"
    # icons are served from the app (cached in assets_icon_cache), never the
    # jira origin — browsers have no Assets session.
    icon_src=$(echo "$card_html" | grep -oE '/assets/objects/[0-9a-f-]+/icon' | head -1)
    if [ -n "$icon_src" ]; then
        icon_ct=$(curl -sf -b "$COOKIES" -o /dev/null -w '%{content_type}' "$APP_URL$icon_src" || true)
        if [ "$icon_ct" = "image/png" ] \
            && psql "$DATABASE_URL" -tA -c "SELECT 1 FROM assets_icon_cache LIMIT 1" 2>/dev/null | grep -q 1; then
            pass "assets: icon served from app cache"
        else
            fail "assets: icon served from app cache (content-type '$icon_ct')"
        fi
    else
        fail "assets: card icon img points at the app"
    fi
else
    fail "llm analysis (no alert)"
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

# ------------------------------------------------- milestone 5: source health
scenario "source health"
# A polled zabbix source with a dead base_url: the poller records failures,
# backs off, and raises an internal alert through the normal pipeline (§4).
health_insert_out=$(psql "$DATABASE_URL" -tA -c "INSERT INTO sources (type, name, base_url, poll_interval_seconds, config) VALUES ('zabbix', 'smoke-health-source', 'http://127.0.0.1:9', 5, '{\"tokenEnv\":\"ZABBIX_TOKEN\"}'::jsonb) RETURNING id" 2>&1)
health_src_id=$(printf '%s\n' "$health_insert_out" | grep -oE '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' | head -1)
if [ -z "$health_src_id" ]; then echo "health source insert error: $health_insert_out" >&2; fi
if [ -n "$health_src_id" ]; then
    wait_sql "source-health alert firing" 120 "SELECT 1 FROM alerts WHERE fingerprint = 'halemans:source-health:$health_src_id' AND status = 'firing' AND severity = 'warning' LIMIT 1" \
        && pass "source health: internal alert created" || fail "source health: internal alert created"
    wait_sql "backoff state persisted" 30 "SELECT 1 FROM sources WHERE id = '$health_src_id' AND consecutive_failures >= 1 AND last_error IS NOT NULL AND next_poll_at IS NOT NULL LIMIT 1" \
        && pass "source health: backoff state on source row" || fail "source health: backoff state on source row"
    # recovery: point the source at the real zabbix; the next due poll
    # succeeds, resets the backoff state and resolves the internal alert
    psql "$DATABASE_URL" -c "UPDATE sources SET base_url = 'http://127.0.0.1:10080' WHERE id = '$health_src_id'" > /dev/null 2>&1
    wait_sql "source-health alert resolved" 180 "SELECT 1 FROM alerts WHERE fingerprint = 'halemans:source-health:$health_src_id' AND status = 'resolved' LIMIT 1" \
        && pass "source health: recovery resolves the alert" || fail "source health: recovery resolves the alert"
    wait_sql "backoff state reset" 30 "SELECT 1 FROM sources WHERE id = '$health_src_id' AND consecutive_failures = 0 AND next_poll_at IS NULL LIMIT 1" \
        && pass "source health: backoff reset on recovery" || fail "source health: backoff reset on recovery"
    psql "$DATABASE_URL" -c "UPDATE sources SET enabled = false WHERE id = '$health_src_id'" > /dev/null 2>&1
else
    fail "source health (insert failed)"
fi

# ------------------------------------------------- milestone 5: retention
scenario "retention"
old_raw_out=$(psql "$DATABASE_URL" -tA -c "INSERT INTO raw_events (payload) VALUES ('{\"smoke\":\"old\"}'::jsonb) RETURNING id" 2>&1)
old_raw_id=$(printf '%s\n' "$old_raw_out" | grep -oE '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' | head -1)
psql "$DATABASE_URL" -c "UPDATE raw_events SET received_at = NOW() - INTERVAL '40 days' WHERE id = '$old_raw_id'" > /dev/null 2>&1
fresh_raw_out=$(psql "$DATABASE_URL" -tA -c "INSERT INTO raw_events (payload) VALUES ('{\"smoke\":\"fresh\"}'::jsonb) RETURNING id" 2>&1)
fresh_raw_id=$(printf '%s\n' "$fresh_raw_out" | grep -oE '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' | head -1)
psql "$DATABASE_URL" -c "INSERT INTO retention_jobs DEFAULT VALUES" > /dev/null 2>&1
for i in $(seq 1 90); do
    if [ -z "$(psql "$DATABASE_URL" -tA -c "SELECT 1 FROM raw_events WHERE id = '$old_raw_id'" 2>/dev/null)" ]; then break; fi
    sleep 1
done
[ -z "$(psql "$DATABASE_URL" -tA -c "SELECT 1 FROM raw_events WHERE id = '$old_raw_id'" 2>/dev/null)" ] \
    && pass "retention: old raw event pruned" || fail "retention: old raw event pruned"
[ -n "$(psql "$DATABASE_URL" -tA -c "SELECT 1 FROM raw_events WHERE id = '$fresh_raw_id'" 2>/dev/null)" ] \
    && pass "retention: fresh raw event kept" || fail "retention: fresh raw event kept"

# ------------------------------------------------- milestone 5: audit export
scenario "audit export"
login_as "admin@dev" "$(cat "$STATE/halemans/admin-password")" > /dev/null 2>&1 \
    || fail "audit export: login as admin@dev"
export_body=$(curl -sf -b "$COOKIES" "$APP_URL/admin/audit/export?format=csv" || true)
echo "$export_body" | grep -q '^event_id,created_at,alert_id' \
    && pass "audit export: csv header" || fail "audit export: csv header"
wait_sql "audit_exports row recorded" 30 "SELECT 1 FROM audit_exports WHERE format = 'csv' AND row_count > 0 LIMIT 1" \
    && pass "audit export: audit_exports row with row count" || fail "audit export: audit_exports row with row count"
curl -sf -b "$COOKIES" "$APP_URL/admin/audit/export?format=jsonl" | head -1 | grep -q '"event_id"' \
    && pass "audit export: jsonl rows" || fail "audit export: jsonl rows"
curl -sf -b "$COOKIES" "$APP_URL/admin/audit" | grep -q 'audit-exports-table' \
    && pass "audit export: admin page lists exports" || fail "audit export: admin page lists exports"
curl -sf -b "$COOKIES" "$APP_URL/admin" | grep -q 'job-metrics-table' \
    && pass "job metrics: admin page renders counters" || fail "job metrics: admin page renders counters"
login_as "viewer@dev" "$(cat "$STATE/halemans/viewer-password")" > /dev/null 2>&1
[ "$(curl -s -b "$COOKIES" -o /dev/null -w '%{http_code}' "$APP_URL/admin/audit/export?format=csv")" = "403" ] \
    && pass "audit export: viewer denied (403)" || fail "audit export: viewer denied (403)"
login_as "sre@dev" "$(cat "$STATE/halemans/sre-password")" > /dev/null 2>&1

# ------------------------------------------------- milestone 8: assets + roles admin
scenario "assets + agent roles admin"
login_as "admin@dev" "$(cat "$STATE/halemans/admin-password")" > /dev/null 2>&1 \
    || fail "assets admin: login as admin@dev"
assets_page=$(curl -sf -b "$COOKIES" "$APP_URL/admin/assets" || true)
echo "$assets_page" | grep -q 'assets-configs' && echo "$assets_page" | grep -q 'assets-dev' \
    && pass "assets admin: info source listed" || fail "assets admin: info source listed"
assets_config_id=$(psql "$DATABASE_URL" -tA -c "SELECT id FROM assets_configs WHERE name = 'assets-dev' LIMIT 1" 2>/dev/null)
if [ -n "$assets_config_id" ]; then
    [ "$(curl -s -b "$COOKIES" -o /dev/null -w '%{http_code}' -X POST "$APP_URL/admin/assets/$assets_config_id/test")" = "302" ] \
        && pass "assets admin: connection test accepted" || fail "assets admin: connection test accepted"
else
    fail "assets admin: no assets-dev config id"
fi
llm_page=$(curl -sf -b "$COOKIES" "$APP_URL/admin/llm" || true)
echo "$llm_page" | grep -q 'llm-roles' && echo "$llm_page" | grep -q 'default-enricher' \
    && pass "agent roles admin: default role listed" || fail "agent roles admin: default role listed"
login_as "viewer@dev" "$(cat "$STATE/halemans/viewer-password")" > /dev/null 2>&1
[ "$(curl -s -b "$COOKIES" -o /dev/null -w '%{http_code}' "$APP_URL/admin/assets")" = "403" ] \
    && pass "assets admin: viewer denied (403)" || fail "assets admin: viewer denied (403)"
login_as "sre@dev" "$(cat "$STATE/halemans/sre-password")" > /dev/null 2>&1

# ------------------------------------------------- milestone 6: public API + metrics
scenario "public API"
API_TOKEN="${HALEMANS_API_TOKEN:-}"
if [ -z "$API_TOKEN" ] && [ -f "$STATE/halemans/api-token" ]; then API_TOKEN="$(cat "$STATE/halemans/api-token")"; fi
if [ -z "$API_TOKEN" ]; then
    fail "public API (no demo token seeded)"
else
    api_body=$(curl -sf -H "Authorization: Bearer $API_TOKEN" "$APP_URL/api/v1/alerts?environment=dev&limit=500" || true)
    echo "$api_body" | grep -q '"alerts"' \
        && pass "api: alerts list responds" || fail "api: alerts list responds"
    echo "$api_body" | grep -q '"check_name":"rbac"' \
        && pass "api: fired probe alert appears in filtered list" || fail "api: fired probe alert appears in filtered list"
    echo "$api_body" | grep -q '"next_cursor"' \
        && pass "api: list envelope has next_cursor" || fail "api: list envelope has next_cursor"
    detail_id=$(psql "$DATABASE_URL" -tA -c "SELECT id FROM alerts WHERE check_name = 'rbac' ORDER BY created_at DESC LIMIT 1" 2>/dev/null)
    detail_body=$(curl -sf -H "Authorization: Bearer $API_TOKEN" "$APP_URL/api/v1/alerts/$detail_id" || true)
    echo "$detail_body" | grep -q '"timeline"' \
        && echo "$detail_body" | grep -q '"llm_analysis"' \
        && pass "api: alert detail has timeline + llm_analysis" || fail "api: alert detail has timeline + llm_analysis"
    curl -sf -H "Authorization: Bearer $API_TOKEN" "$APP_URL/api/v1/environments" | grep -q '"worst_severity"' \
        && pass "api: environments rollup" || fail "api: environments rollup"
    [ "$(curl -s -o /dev/null -w '%{http_code}' "$APP_URL/api/v1/alerts")" = "401" ] \
        && pass "api: missing token -> 401" || fail "api: missing token -> 401"
    bad_body=$(curl -s -H "Authorization: Bearer $API_TOKEN" "$APP_URL/api/v1/alerts?cursor=garbage")
    echo "$bad_body" | grep -q '"error":"bad_request"' \
        && pass "api: bad cursor -> 400 json" || fail "api: bad cursor -> 400 json"
    page1=$(curl -sf -H "Authorization: Bearer $API_TOKEN" "$APP_URL/api/v1/alerts?limit=1" || true)
    cursor1=$(echo "$page1" | grep -o '"next_cursor":"[^"]*"' | head -1 | cut -d'"' -f4)
    if [ -n "$cursor1" ]; then
        curl -sf -H "Authorization: Bearer $API_TOKEN" "$APP_URL/api/v1/alerts?limit=1&cursor=$cursor1" | grep -q '"alerts"' \
            && pass "api: cursor page 2" || fail "api: cursor page 2"
    else
        fail "api: cursor page 2 (no next_cursor)"
    fi
    wait_sql "api: last_used_at touched" 15 "SELECT 1 FROM api_tokens WHERE last_used_at IS NOT NULL LIMIT 1" \
        && pass "api: last_used_at touched" || fail "api: last_used_at touched"

    # metrics-only token: allowed on /metrics, forbidden on the alerts API,
    # 401 once revoked.
    METRICS_TOKEN="halemans-smoke-metrics-token-v1"
    metrics_hash=$(printf %s "$METRICS_TOKEN" | sha256sum | cut -d' ' -f1)
    metrics_token_id=$(psql "$DATABASE_URL" -tA -c \
        "INSERT INTO api_tokens (user_id, name, token_hash, prefix, scopes)
         SELECT id, 'smoke-metrics', '$metrics_hash', '${METRICS_TOKEN:0:8}', '{metrics}' FROM users WHERE email = 'sre@dev'
         RETURNING id" 2>&1 | grep -oE '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' | head -1)
    [ "$(curl -s -o /dev/null -w '%{http_code}' -H "Authorization: Bearer $METRICS_TOKEN" "$APP_URL/api/v1/alerts")" = "403" ] \
        && pass "api: metrics-only token -> 403 on alerts" || fail "api: metrics-only token -> 403 on alerts"
    metrics_body=$(curl -sf -H "Authorization: Bearer $METRICS_TOKEN" "$APP_URL/metrics" || true)
    echo "$metrics_body" | grep -q 'halemans_alerts{' \
        && echo "$metrics_body" | grep -q 'halemans_source_healthy{' \
        && echo "$metrics_body" | grep -q 'halemans_build_info{' \
        && pass "metrics: scrape exposes alert/source/build series" || fail "metrics: scrape exposes alert/source/build series"
    [ "$(curl -s -o /dev/null -w '%{http_code}' "$APP_URL/metrics")" = "401" ] \
        && pass "metrics: missing token -> 401" || fail "metrics: missing token -> 401"

    # rate limit (default 6/min for /metrics): hammer until 429, then recover.
    got_429=""
    for i in $(seq 1 10); do
        code=$(curl -s -o /dev/null -w '%{http_code}' -H "Authorization: Bearer $METRICS_TOKEN" "$APP_URL/metrics")
        if [ "$code" = "429" ]; then got_429=1; break; fi
    done
    [ -n "$got_429" ] \
        && pass "metrics: rate limit -> 429" || fail "metrics: rate limit -> 429"
    retry_after=$(curl -s -D - -o /dev/null -H "Authorization: Bearer $METRICS_TOKEN" "$APP_URL/metrics" | grep -i '^retry-after:' | tr -d '\r' | cut -d' ' -f2)
    [ -n "$retry_after" ] \
        && pass "metrics: 429 carries Retry-After" || fail "metrics: 429 carries Retry-After"
    sleep 12
    [ "$(curl -s -o /dev/null -w '%{http_code}' -H "Authorization: Bearer $METRICS_TOKEN" "$APP_URL/metrics")" = "200" ] \
        && pass "metrics: bucket refills after waiting" || fail "metrics: bucket refills after waiting"

    psql "$DATABASE_URL" -c "UPDATE api_tokens SET revoked_at = NOW() WHERE id = '$metrics_token_id'" > /dev/null 2>&1
    [ "$(curl -s -o /dev/null -w '%{http_code}' -H "Authorization: Bearer $METRICS_TOKEN" "$APP_URL/metrics")" = "401" ] \
        && pass "metrics: revoked token -> 401" || fail "metrics: revoked token -> 401"
fi

# ---------------------------------------------------------------- provision (milestone 7)
# Restarts the app process against a provision config. Only runs when the
# harness owns the app process (checks.smoke sets SMOKE_APP_MANAGED=1); the
# dev stack's process-compose owns it there, so the scenario skips.
if [ "${SMOKE_APP_MANAGED:-0}" = "1" ]; then
scenario "provision (milestone 7)"

restart_app() { # <provision-config-path-or-empty>
    kill "$SMOKE_APP_PID" 2>/dev/null
    for _ in $(seq 1 30); do kill -0 "$SMOKE_APP_PID" 2>/dev/null || break; sleep 1; done
    if [ -n "$1" ]; then export HALEMANS_PROVISION_CONFIG="$1"; else unset HALEMANS_PROVISION_CONFIG; fi
    PORT=28080 DATABASE_URL="$DATABASE_URL" "$RUN_PROD_SERVER" >> "$SMOKE_APP_LOG" 2>&1 &
    SMOKE_APP_PID=$!
    for _ in $(seq 1 60); do
        curl -sf -o /dev/null "$APP_URL/NewSession" 2>/dev/null && return 0
        sleep 1
    done
    echo "  app did not come up after provision restart; log tail:" >&2
    tail -20 "$SMOKE_APP_LOG" >&2
    return 1
}

PROV_DIR="$TMPDIR/provision"
mkdir -p "$PROV_DIR"
gen_out="$(halemans-gen-password "smoke-prov@dev")"
prov_password="$(printf '%s\n' "$gen_out" | awk '/^password:/ {print $2}')"
prov_hash="$(printf '%s\n' "$gen_out" | awk '/^passwordHash:/ {print $2}')"
[ -n "$prov_password" ] && [ -n "$prov_hash" ] \
    && pass "gen-password produced plaintext + hash" || fail "gen-password produced plaintext + hash"

cat > "$PROV_DIR/pass1.json" <<EOF
{
  "users": {"smoke-prov@dev": {"passwordHash": "$prov_hash", "roles": ["viewer"], "settings": {"theme": "dark"}}},
  "sources": {"smoke-prov-hook": {"type": "webhook", "enabled": false}},
  "teams": {"smoke-prov-team": {"members": {"smoke-prov@dev": {"role": "lead"}}}},
  "llm": {"smoke-prov": {"endpoint": "http://127.0.0.1:18084", "model": "mock-llm-1", "enabled": true}}
}
EOF

restart_app "$PROV_DIR/pass1.json" && pass "app boots with provision config" || fail "app boots with provision config"
psql "$DATABASE_URL" -tA -c "SELECT 1 FROM users WHERE email = 'smoke-prov@dev'" 2>/dev/null | grep -q 1 \
    && pass "provisioned user row exists" || fail "provisioned user row exists"
[ "$(psql "$DATABASE_URL" -tA -c "SELECT tm.team_role FROM team_members tm JOIN teams t ON t.id = tm.team_id JOIN users u ON u.id = tm.user_id WHERE t.name = 'smoke-prov-team' AND u.email = 'smoke-prov@dev'" 2>/dev/null)" = "lead" ] \
    && pass "provisioned team + membership exist" || fail "provisioned team + membership exist"
psql "$DATABASE_URL" -tA -c "SELECT 1 FROM sources WHERE name = 'smoke-prov-hook' AND enabled = false" 2>/dev/null | grep -q 1 \
    && pass "provisioned disabled source exists" || fail "provisioned disabled source exists"
[ "$(psql "$DATABASE_URL" -tA -c "SELECT provider_name || ':' || endpoint FROM llm_configs WHERE enabled" 2>/dev/null)" = "smoke-prov:http://127.0.0.1:18084" ] \
    && pass "llm_configs row is the active config" || fail "llm_configs row is the active config"
login_as "smoke-prov@dev" "$prov_password" \
    && pass "provisioned user logs in with generated password" || fail "provisioned user logs in with generated password"

# Second pass: strict teams with the extra team removed (sre kept with its
# seeded membership) deletes smoke-prov-team and nothing else.
# Second pass: global strict with ONLY the teams section present (absent
# sections stay untouched) deletes smoke-prov-team and nothing else.
cat > "$PROV_DIR/pass2.json" <<EOF
{
  "strict": true,
  "teams": {
    "sre": {"members": {"sre@dev": {"role": "lead"}, "admin@dev": {"role": "member"}}}
  }
}
EOF
restart_app "$PROV_DIR/pass2.json" && pass "app reboots with strict teams" || fail "app reboots with strict teams"
[ -z "$(psql "$DATABASE_URL" -tA -c "SELECT 1 FROM teams WHERE name = 'smoke-prov-team'" 2>/dev/null)" ] \
    && pass "strict teams deleted the extra team" || fail "strict teams deleted the extra team"
psql "$DATABASE_URL" -tA -c "SELECT 1 FROM teams WHERE name = 'sre'" 2>/dev/null | grep -q 1 \
    && pass "strict teams kept the sre team" || fail "strict teams kept the sre team"
login_as "smoke-prov@dev" "$prov_password" \
    && pass "provisioned user still logs in after strict reboot" || fail "provisioned user still logs in after strict reboot"
login_as "sre@dev" "$(cat "$STATE/halemans/sre-password")" > /dev/null 2>&1 || true
fi

echo
if [ "$failures" = 0 ]; then
    echo "smoke: all scenarios passed"
else
    echo "smoke: $failures failure(s):"
    printf '  FAILED %s\n' "${failed_tests[@]}"
fi
exit "$failures"
