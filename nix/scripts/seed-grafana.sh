set -euo pipefail

GRAFANA_URL="${HALEMANS_GRAFANA_URL:-http://127.0.0.1:3001}"
AUTH="admin:admin"
STATE="${DEVENV_STATE:?}/grafana"
TOKEN_FILE="$STATE/token"
RULE_UID="dev-cpu-sim"

mkdir -p "$STATE"

echo "seed-grafana: waiting for $GRAFANA_URL/api/health"
for i in $(seq 1 60); do
    if curl -sf "$GRAFANA_URL/api/health" > /dev/null 2>&1; then break; fi
    if [ "$i" = 60 ]; then echo "seed-grafana: grafana not healthy" >&2; exit 1; fi
    sleep 2
done

# --- service account + dev token -------------------------------------------
sa_id=$(curl -sf -u "$AUTH" "$GRAFANA_URL/api/serviceaccounts/search?query=halemans-dev" \
    | jq -r '.serviceAccounts[]? | select(.name=="halemans-dev") | .id' | head -1)
if [ -z "$sa_id" ]; then
    sa_id=$(curl -sf -u "$AUTH" -H 'Content-Type: application/json' \
        -d '{"name":"halemans-dev","role":"Admin"}' \
        "$GRAFANA_URL/api/serviceaccounts" | jq -r '.id')
fi

if [ -f "$TOKEN_FILE" ] \
    && curl -sf -H "Authorization: Bearer $(cat "$TOKEN_FILE")" \
        "$GRAFANA_URL/api/serviceaccounts/search?query=halemans-dev" > /dev/null 2>&1; then
    echo "seed-grafana: existing service-account token still valid, keeping it"
else
    # Old tokens are unreadable after creation; drop and re-issue.
    for tid in $(curl -sf -u "$AUTH" "$GRAFANA_URL/api/serviceaccounts/$sa_id/tokens" \
        | jq -r '.[]? | select(.name=="halemans-dev") | .id'); do
        curl -sf -u "$AUTH" -X DELETE \
            "$GRAFANA_URL/api/serviceaccounts/$sa_id/tokens/$tid" > /dev/null
    done
    token=$(curl -sf -u "$AUTH" -H 'Content-Type: application/json' \
        -d '{"name":"halemans-dev"}' \
        "$GRAFANA_URL/api/serviceaccounts/$sa_id/tokens" | jq -r '.key')
    printf '%s' "$token" > "$TOKEN_FILE"
    echo "seed-grafana: wrote dev service-account token to $TOKEN_FILE"
fi

# --- alert rule dev-cpu-sim --------------------------------------------------
# Threshold +1e9 -> never fires. fire-test-alert-grafana flips it to -1e9.
folder_uid=$(curl -sf -u "$AUTH" "$GRAFANA_URL/api/folders" \
    | jq -r '.[]? | select(.title=="Dev") | .uid' | head -1)
if [ -z "$folder_uid" ]; then
    folder_uid=$(curl -sf -u "$AUTH" -H 'Content-Type: application/json' \
        -d '{"title":"Dev"}' "$GRAFANA_URL/api/folders" | jq -r '.uid')
fi

rule_payload() {
    jq -n --arg folder "$folder_uid" '{
        uid: "dev-cpu-sim",
        title: "dev-cpu-sim",
        condition: "C",
        folderUID: $folder,
        ruleGroup: "dev-alerts",
        for: "0s",
        noDataState: "OK",
        execErrState: "Error",
        labels: { env: "dev", host: "dev-host-01", check: "dev-cpu-sim" },
        annotations: { summary: "Dev CPU simulation alert", severity: "warning" },
        data: [
            { refId: "A", datasourceUid: "testdata",
              relativeTimeRange: { from: 60, to: 0 },
              model: { refId: "A", scenarioId: "random_walk",
                       datasource: { type: "testdata", uid: "testdata" },
                       intervalMs: 1000, maxDataPoints: 100 } },
            { refId: "B", datasourceUid: "__expr__",
              relativeTimeRange: { from: 0, to: 0 },
              model: { refId: "B", type: "reduce", expression: "A", reducer: "last",
                       datasource: { type: "__expr__", uid: "__expr__" },
                       intervalMs: 1000, maxDataPoints: 100 } },
            { refId: "C", datasourceUid: "__expr__",
              relativeTimeRange: { from: 0, to: 0 },
              model: { refId: "C", type: "threshold", expression: "B",
                       datasource: { type: "__expr__", uid: "__expr__" },
                       intervalMs: 1000, maxDataPoints: 100,
                       conditions: [ { type: "query",
                                       query: { params: ["B"] },
                                       reducer: { type: "last", params: [] },
                                       evaluator: { type: "gt", params: [1000000000] },
                                       operator: { type: "and" } } ] } }
        ]
    }'
}

if curl -sf -u "$AUTH" "$GRAFANA_URL/api/v1/provisioning/alert-rules/$RULE_UID" > /dev/null 2>&1; then
    # Upsert: PUT the canonical definition (repairs drift, keeps threshold at
    # the non-firing default).
    rule_payload | curl -sf -u "$AUTH" -H 'Content-Type: application/json' -X PUT \
        -d @- "$GRAFANA_URL/api/v1/provisioning/alert-rules/$RULE_UID" > /dev/null
    echo "seed-grafana: alert rule $RULE_UID updated to canonical definition"
else
    rule_payload | curl -sf -u "$AUTH" -H 'Content-Type: application/json' \
        -d @- "$GRAFANA_URL/api/v1/provisioning/alert-rules" > /dev/null
    echo "seed-grafana: created alert rule $RULE_UID"
fi

# Fast evaluation for dev: 10s rule-group interval (grafana scheduler tick).
curl -sf -u "$AUTH" -H 'Content-Type: application/json' -X PUT \
    -d '{"title": "dev-alerts", "interval": 10}' \
    "$GRAFANA_URL/api/v1/provisioning/folder/$folder_uid/rule-groups/dev-alerts" > /dev/null

echo "seed-grafana: done"
