set -euo pipefail

# Usage: fire-test-alert-grafana [resolve]
# Flips the dev-cpu-sim rule threshold so it always fires (-1e9) or never
# fires (+1e9). Deterministic: random_walk is unbounded either way relative
# to the flipped bound.
GRAFANA_URL="${HALEMANS_GRAFANA_URL:-http://127.0.0.1:3001}"
AUTH="admin:admin"
RULE_UID="dev-cpu-sim"

if [ "${1:-fire}" = "resolve" ]; then
    threshold=1000000000
else
    threshold=-1000000000
fi

rule=$(curl -sf -u "$AUTH" "$GRAFANA_URL/api/v1/provisioning/alert-rules/$RULE_UID")
echo "$rule" \
    | jq --argjson t "$threshold" '
        .data = (.data | map(
            if .refId == "C" then
                .model.conditions[0].evaluator.params = [$t]
            else . end))
        | del(.updated, .provenance, .id, .notification_settings)
      ' \
    | curl -sf -u "$AUTH" -H 'Content-Type: application/json' -X PUT \
        -d @- "$GRAFANA_URL/api/v1/provisioning/alert-rules/$RULE_UID" > /dev/null

echo "fire-test-alert-grafana: threshold set to $threshold ($([ "$threshold" -lt 0 ] && echo firing || echo resolving))"
