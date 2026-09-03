set -euo pipefail

# Usage: fire-test-alert-alertmanager [resolve]
# Posts a fixed-labelset test alert to the local alertmanager; alertmanager
# webhooks it into Halemans. resolve sets endsAt in the past.
AM_URL="${HALEMANS_ALERTMANAGER_URL:-http://127.0.0.1:9093}"

now=$(date -u +%Y-%m-%dT%H:%M:%SZ)
if [ "${1:-fire}" = "resolve" ]; then
    ends_at="$now"
else
    ends_at=$(date -u -d '+1 hour' +%Y-%m-%dT%H:%M:%SZ)
fi

jq -n --arg starts "$now" --arg ends "$ends_at" '[{
    labels: {
        alertname: "halemans-test",
        env: "dev",
        host: "dev-host-01",
        severity: "warning",
        check: "am-test"
    },
    annotations: { summary: "Alertmanager dev test alert", description: "fired by fire-test-alert-alertmanager" },
    startsAt: $starts,
    endsAt: $ends
}]' | curl -sf -H 'Content-Type: application/json' -d @- "$AM_URL/api/v2/alerts" > /dev/null

echo "fire-test-alert-alertmanager: posted (${1:-fire})"
