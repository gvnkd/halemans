set -euo pipefail

# Usage: fire-test-alert-zabbix [resolve]
# Pushes a value into the trapper item halemans.test.value via history.push:
#   1 -> trigger "halemans test trigger" fires, 0 -> recovers.
ZABBIX_URL="${HALEMANS_ZABBIX_URL:-http://127.0.0.1:10080}"
API="$ZABBIX_URL/api_jsonrpc.php"
TOKEN_FILE="${DEVENV_STATE:?}/zabbix/token"
ITEM_KEY="halemans.test.value"

if [ "${1:-fire}" = "resolve" ]; then value=0; else value=1; fi

[ -f "$TOKEN_FILE" ] || { echo "fire-test-alert-zabbix: no token at $TOKEN_FILE; run seed-zabbix first" >&2; exit 1; }
TOKEN=$(cat "$TOKEN_FILE")

item_id=$(curl -sf -H 'Content-Type: application/json' -H "Authorization: Bearer $TOKEN" \
    -d "$(jq -n --arg k "$ITEM_KEY" '{jsonrpc:"2.0",method:"item.get",params:{filter:{key_:$k}},id:1}')" \
    "$API" | jq -r '.result[0].itemid // empty')
[ -n "$item_id" ] || { echo "fire-test-alert-zabbix: item $ITEM_KEY not found; run seed-zabbix first" >&2; exit 1; }

curl -sf -H 'Content-Type: application/json' -H "Authorization: Bearer $TOKEN" \
    -d "$(jq -n --arg i "$item_id" --arg v "$value" \
        '{jsonrpc:"2.0",method:"history.push",params:[{itemid:$i,value:$v}],id:1}')" \
    "$API" | jq -e '.result' > /dev/null

echo "fire-test-alert-zabbix: pushed value=$value to $ITEM_KEY"
