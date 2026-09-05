set -uo pipefail

# Per-service health + token presence report (doc §7).
ok=0
check() { # name ok?
    if [ "$2" = 0 ]; then printf '  %-22s OK\n' "$1"; else printf '  %-22s FAIL\n' "$1"; ok=1; fi
}

echo "services:"
curl -sf "${HALEMANS_ZABBIX_URL:-http://127.0.0.1:10080}/api_jsonrpc.php" \
    -H 'Content-Type: application/json' \
    -d '{"jsonrpc":"2.0","method":"apiinfo.version","params":{},"id":1}' > /dev/null 2>&1
check "zabbix api (:10080)" $?

curl -sf "${HALEMANS_GRAFANA_URL:-http://127.0.0.1:3001}/api/health" > /dev/null 2>&1
check "grafana (:3001)" $?

curl -sf "${HALEMANS_ALERTMANAGER_URL:-http://127.0.0.1:9093}/-/healthy" > /dev/null 2>&1
check "alertmanager (:9093)" $?

curl -sf "${HALEMANS_CONFLUENCE_URL:-http://127.0.0.1:18082}/health" > /dev/null 2>&1
check "mock-confluence (:18082)" $?

curl -sf "${HALEMANS_JIRA_URL:-http://127.0.0.1:18083}/health" > /dev/null 2>&1
check "mock-jira (:18083)" $?

curl -sf "${LLM_ENDPOINT:-http://127.0.0.1:18084}/health" > /dev/null 2>&1
check "mock-llm (:18084)" $?

pg_isready -q -h "${PGHOST:-/tmp}" > /dev/null 2>&1
check "postgres" $?

curl -sf "${HALEMANS_APP_URL:-http://127.0.0.1:28080}" > /dev/null 2>&1
check "halemans app (:28080)" $?

echo "tokens:"
state="${DEVENV_STATE:-.devenv/state}"
for f in "halemans/am-hook-token" "halemans/generic-hook-token" "halemans/confluence-token" "halemans/jira-token" "zabbix/token" "grafana/token"; do
    if [ -s "$state/$f" ]; then check "$f" 0; else check "$f" 1; fi
done

exit "$ok"
