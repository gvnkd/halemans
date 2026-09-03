set -euo pipefail

ZABBIX_URL="${HALEMANS_ZABBIX_URL:-http://127.0.0.1:10080}"
API="$ZABBIX_URL/api_jsonrpc.php"
STATE="${DEVENV_STATE:?}/zabbix"
TOKEN_FILE="$STATE/token"
HOST_NAME="dev-host-01"
ITEM_KEY="halemans.test.value"
TRIGGER_DESC="halemans test trigger"

mkdir -p "$STATE"

# api <method> <params-json> [bearer-token]
api() {
    local method="$1" params="$2" token="${3:-}"
    local args=(-sf -H 'Content-Type: application/json')
    if [ -n "$token" ]; then args+=(-H "Authorization: Bearer $token"); fi
    curl "${args[@]}" \
        -d "$(jq -n --arg m "$method" --argjson p "$params" \
            '{jsonrpc:"2.0",method:$m,params:$p,id:1}')" \
        "$API"
}

echo "seed-zabbix: waiting for zabbix API at $API"
for i in $(seq 1 90); do
    if api apiinfo.version '{}' 2>/dev/null | jq -e '.result' > /dev/null 2>&1; then break; fi
    if [ "$i" = 90 ]; then echo "seed-zabbix: zabbix API not reachable" >&2; exit 1; fi
    sleep 5
done

# --- dev API token -----------------------------------------------------------
if [ -f "$TOKEN_FILE" ] \
    && api problem.get '{"limit":1}' "$(cat "$TOKEN_FILE")" 2>/dev/null \
        | jq -e '.result' > /dev/null 2>&1; then
    echo "seed-zabbix: existing API token still valid, keeping it"
else
    session=$(api user.login '{"username":"Admin","password":"zabbix"}' | jq -r '.result')
    # token secrets are only shown by token.generate; drop stale ones and re-issue.
    old=$(api token.get '{"filter":{"name":["halemans-dev"]}}' "$session" \
        | jq -r '[.result[].tokenid] | join(",")')
    if [ -n "$old" ]; then
        api token.delete "[\"${old//,/\",\"}\"]" "$session" > /dev/null
    fi
    token_id=$(api token.create '[{"name":"halemans-dev","description":"Halemans dev connector token"}]' "$session" \
        | jq -r '.result.tokenids[0]')
    token=$(api token.generate "[\"$token_id\"]" "$session" | jq -r '.result[0].token')
    printf '%s' "$token" > "$TOKEN_FILE"
    echo "seed-zabbix: wrote dev API token to $TOKEN_FILE"
fi
TOKEN=$(cat "$TOKEN_FILE")

# --- host group ---------------------------------------------------------------
group_id=$(api hostgroup.get '{"filter":{"name":["dev"]}}' "$TOKEN" \
    | jq -r '.result[0].groupid // empty')
if [ -z "$group_id" ]; then
    group_id=$(api hostgroup.create '{"name":"dev"}' "$TOKEN" | jq -r '.result.groupids[0]')
fi

# --- host dev-host-01 ----------------------------------------------------------
host_id=$(api host.get "{\"filter\":{\"host\":[\"$HOST_NAME\"]}}" "$TOKEN" \
    | jq -r '.result[0].hostid // empty')
if [ -z "$host_id" ]; then
    host_id=$(api host.create "$(jq -n --arg g "$group_id" '{
        host: "dev-host-01",
        interfaces: [ { type: 1, main: 1, useip: 1, ip: "127.0.0.1", dns: "", port: "10050" } ],
        groups: [ { groupid: $g } ],
        tags: [ { tag: "env", value: "dev" } ]
    }')" "$TOKEN" | jq -r '.result.hostids[0]')
    echo "seed-zabbix: created host $HOST_NAME ($host_id)"
fi

# --- trapper item + trigger -----------------------------------------------------
item_id=$(api item.get "{\"hostids\":\"$host_id\",\"filter\":{\"key_\":\"$ITEM_KEY\"}}" "$TOKEN" \
    | jq -r '.result[0].itemid // empty')
if [ -z "$item_id" ]; then
    item_id=$(api item.create "$(jq -n --arg h "$host_id" '{
        name: "halemans test value",
        key_: "halemans.test.value",
        hostid: $h,
        type: 2,
        value_type: 0
    }')" "$TOKEN" | jq -r '.result.itemids[0]')
    echo "seed-zabbix: created trapper item $ITEM_KEY ($item_id)"
fi

trigger_id=$(api trigger.get "{\"hostids\":\"$host_id\",\"filter\":{\"description\":\"$TRIGGER_DESC\"}}" "$TOKEN" \
    | jq -r '.result[0].triggerid // empty')
if [ -z "$trigger_id" ]; then
    trigger_id=$(api trigger.create '{
        "description": "halemans test trigger",
        "expression": "last(/dev-host-01/halemans.test.value)>0.5",
        "priority": 4,
        "comments": "Deterministic dev trigger flipped by fire-test-alert-zabbix"
    }' "$TOKEN" | jq -r '.result.triggerids[0]')
    echo "seed-zabbix: created trigger '$TRIGGER_DESC' ($trigger_id)"
fi

echo "seed-zabbix: done"
