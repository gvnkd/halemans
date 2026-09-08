#!/usr/bin/env bash
#
# assets_api_readonly_test.sh
#
# Full READ-ONLY test of the Atlassian Assets (Jira Insights) REST API.
#
# SAFETY GUARANTEES
#   - The ONLY network call in this script is ro_get() below.
#   - ro_get() never sets -d / --data / -X / -T / -F / --upload, so the
#     HTTP method is always GET (curl default when none of the above are
#     used; -G merely moves --data-urlencode into the query string).
#   - No POST/PUT/PATCH/DELETE operation is referenced anywhere in this file.
#   - No request body is ever sent.
#
# Usage:
#   export JIRA_TOKEN=...            # bearer token
#   ./assets_api_readonly_test.sh    # optional env: ASSETS_BASE, MAX_SCHEMAS, MAX_TYPES, DELAY
#
# Exit code: 0 = all executed tests passed, 1 = at least one failure.

set -euo pipefail

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
JIRA_HOST="https://jira.example.com"
B="${ASSETS_BASE:-https://${JIRA_HOST}/rest/assets/latest}"
TOKEN="${JIRA_TOKEN:?JIRA_TOKEN is not set (export it first)}"
MAX_SCHEMAS="${MAX_SCHEMAS:-3}"
MAX_TYPES="${MAX_TYPES:-3}"
DELAY="${DELAY:-0.2}"
CURL_TIMEOUT=30

PASS=0
FAIL=0
SKIP=0
FAILED_LIST=()

# ---------------------------------------------------------------------------
# ro_get LABEL PATH [extra curl args...]
#   Single read-only entry point. Always GET. Extra args may include
#   -G --data-urlencode for query parameters and -H for extra headers.
#   Sets global RO_BODY and RO_CODE.
# ---------------------------------------------------------------------------
ro_get() {
  local label="$1" path="$2"
  shift 2
  local resp curl_rc=0
  resp="$(curl --silent --show-error --max-time "$CURL_TIMEOUT" \
      -H "Authorization: Bearer ${TOKEN}" \
      -H 'Accept: application/json' \
      -w $'\n%{http_code}' \
      "$@" "$B${path}" 2>&1)" || curl_rc=$?
  RO_CODE="${resp##*$'\n'}"
  RO_BODY="${resp%$'\n'*}"
  if [ "$curl_rc" -ne 0 ]; then
    RO_CODE="000"
    RO_BODY="curl error: ${resp}"
  fi
}

# result LABEL EXPECTED(2xx|probe)
#   2xx   -> 200..299 pass, anything else fails
#   probe -> 2xx pass, 404/400 counted as no-data (skip), anything else fails
result() {
  local label="$1" expect="$2" code="$3"
  local body_snippet=""
  case "$code" in
    2*)
      PASS=$((PASS + 1))
      if command -v jq >/dev/null 2>&1 && printf '%s' "$RO_BODY" | jq -e . >/dev/null 2>&1; then
        body_snippet="$(printf '%s' "$RO_BODY" | jq -c . 2>/dev/null | cut -c1-160)"
      else
        body_snippet="$(printf '%s' "$RO_BODY" | cut -c1-120)"
      fi
      printf '  [PASS] %-58s HTTP %s  %s\n' "$label" "$code" "$body_snippet"
      ;;
    404|400)
      if [ "$expect" = "probe" ]; then
        SKIP=$((SKIP + 1))
        printf '  [NO-DATA] %-56s HTTP %s\n' "$label" "$code"
      else
        FAIL=$((FAIL + 1))
        FAILED_LIST+=("$label")
        printf '  [FAIL] %-58s HTTP %s  %s\n' "$label" "$code" "$(printf '%s' "$RO_BODY" | cut -c1-120)"
      fi
      ;;
    *)
      FAIL=$((FAIL + 1))
      FAILED_LIST+=("$label")
      printf '  [FAIL] %-58s HTTP %s  %s\n' "$label" "$code" "$(printf '%s' "$RO_BODY" | cut -c1-120)"
      ;;
  esac
  sleep "$DELAY"
}

jq_get() { # jq_get JQFILTER  (operates on RO_BODY, prints nothing on error)
  printf '%s' "$RO_BODY" | jq -r "$1" 2>/dev/null || true
}

# items_ids: print .id of every item in RO_BODY, accepting either a bare
# JSON array or an object wrapper (values/objects/icons/objectEntries/items/results)
# NOTE: type check must come first - indexing an array with ".values" is a jq error
items_ids() {
  jq_get '(if type=="array" then . else (.values // .objects // .icons // .objectEntries // .items // .results // empty) end)[]? | select(.id != null) | .id'
}

first_id() {
  items_ids | head -n 1
}

echo "=============================================="
echo " Assets REST API - READ-ONLY test (GET only)"
echo " Base: $B"
echo " Limits: schemas<=$MAX_SCHEMAS types/per-schema<=$MAX_TYPES"
echo "=============================================="

# ---------------------------------------------------------------------------
# 1. Schemas
# ---------------------------------------------------------------------------
echo
echo "--- Schemas ---"
ro_get "GET /objectschema/list" "/objectschema/list"
result "GET /objectschema/list" 2xx "$RO_CODE"

mapfile -t SCHEMA_IDS < <(jq_get '.objectschemas[].id' | head -n "$MAX_SCHEMAS")
if [ "${#SCHEMA_IDS[@]}" -eq 0 ]; then
  echo "  No schemas found - cannot continue deeper tests."
  echo
  echo "=== SUMMARY: pass=$PASS fail=$FAIL no-data=$SKIP ==="
  exit 0
fi
echo "  Schemas found: ${SCHEMA_IDS[*]}"

SCHEMA1=""
FIRST_TYPE_NAME=""
FIRST_TYPE_ID=""
FIRST_OBJECT_ID=""
SCHEMA_NAMES=()

for s in "${SCHEMA_IDS[@]}"; do
  echo
  echo "--- Schema $s ---"

  ro_get "GET /objectschema/{id}" "/objectschema/$s"
  result "GET /objectschema/$s" 2xx "$RO_CODE"
  [ -z "$SCHEMA1" ] && SCHEMA1="$s"

  SCHEMA_NAMES+=("$(jq_get '.name')")

  # object types (tree)
  ro_get "GET /objectschema/{id}/objecttypes" "/objectschema/$s/objecttypes?excludeAbstract=false"
  result "GET /objectschema/$s/objecttypes" 2xx "$RO_CODE"
  mapfile -t TREE_TYPE_IDS < <(items_ids | head -n "$MAX_TYPES")
  [ -z "$FIRST_TYPE_ID" ] && [ "${#TREE_TYPE_IDS[@]}" -gt 0 ] && FIRST_TYPE_ID="${TREE_TYPE_IDS[0]}"

  # object types (flat, with object counts)
  ro_get "GET /objectschema/{id}/objecttypes/flat" \
    "/objectschema/$s/objecttypes/flat" -G --data-urlencode "includeObjectCounts=true"
  result "GET /objectschema/$s/objecttypes/flat" 2xx "$RO_CODE"
  mapfile -t TYPE_IDS < <(items_ids | head -n "$MAX_TYPES")
  # on some instances the tree endpoint returns [] while flat has the data
  if [ "${#TYPE_IDS[@]}" -eq 0 ]; then
    TYPE_IDS=("${TREE_TYPE_IDS[@]+"${TREE_TYPE_IDS[@]}"}")
  fi
  if [ -z "$FIRST_TYPE_NAME" ] && [ "${#TYPE_IDS[@]}" -gt 0 ]; then
    FIRST_TYPE_NAME="$(jq_get "(if type==\"array\" then . else (.values // .objectEntries // empty) end)[]? | select(.id == ${TYPE_IDS[0]}) | .name")"
  fi

  # schema-level attributes
  ro_get "GET /objectschema/{id}/attributes" \
    "/objectschema/$s/attributes" -G --data-urlencode 'query=""'
  result "GET /objectschema/$s/attributes" 2xx "$RO_CODE"

  # object type details + attributes
  for t in ${TYPE_IDS[@]+"${TYPE_IDS[@]}"}; do
    ro_get "GET /objecttype/{id}" "/objecttype/$t"
    result "GET /objecttype/$t" 2xx "$RO_CODE"

    ro_get "GET /objecttype/{id}/attributes" \
      "/objecttype/$t/attributes" -G --data-urlencode 'query=""' --data-urlencode "includeChildren=false"
    result "GET /objecttype/$t/attributes" 2xx "$RO_CODE"
  done
done

# ---------------------------------------------------------------------------
# 2. Objects (via GET-only AQL search - the POST /object/aql endpoint is
#    intentionally NOT used because it is a POST method)
# ---------------------------------------------------------------------------
echo
echo "--- Objects ---"
AQL_TRYS=()
[ -n "$FIRST_TYPE_NAME" ] && AQL_TRYS+=("objectType = \"${FIRST_TYPE_NAME}\"")
for n in ${SCHEMA_NAMES[@]+"${SCHEMA_NAMES[@]}"}; do
  [ -n "$n" ] && AQL_TRYS+=("objectSchema = \"$n\"")
done

for q in ${AQL_TRYS[@]+"${AQL_TRYS[@]}"}; do
  [ -n "$FIRST_OBJECT_ID" ] && break
  ro_get "GET /aql/objects" \
    "/aql/objects" -G \
    --data-urlencode "qlQuery=$q" \
    --data-urlencode "page=1" \
    --data-urlencode "resultPerPage=5" \
    --data-urlencode "includeAttributes=true"
  result "GET /aql/objects (qlQuery=$q)" 2xx "$RO_CODE"
  FIRST_OBJECT_ID="$(first_id)"
done
echo "  First object id: ${FIRST_OBJECT_ID:-<none found>}"

if [ -n "$FIRST_OBJECT_ID" ]; then
  ro_get "GET /object/{id} (DETAIL)" "/object/$FIRST_OBJECT_ID"
  result "GET /object/$FIRST_OBJECT_ID (object detail)" 2xx "$RO_CODE"

  ro_get "GET /object/{id}/attributes" "/object/$FIRST_OBJECT_ID/attributes"
  result "GET /object/$FIRST_OBJECT_ID/attributes" 2xx "$RO_CODE"

  ro_get "GET /object/{id}/history" "/object/$FIRST_OBJECT_ID/history?asc=true"
  result "GET /object/$FIRST_OBJECT_ID/history" 2xx "$RO_CODE"

  ro_get "GET /object/{id}/referenceinfo" "/object/$FIRST_OBJECT_ID/referenceinfo"
  result "GET /object/$FIRST_OBJECT_ID/referenceinfo" 2xx "$RO_CODE"

  ro_get "GET /objectconnectedtickets/{objectId}/tickets" \
    "/objectconnectedtickets/$FIRST_OBJECT_ID/tickets"
  result "GET /objectconnectedtickets/$FIRST_OBJECT_ID/tickets" 2xx "$RO_CODE"
else
  SKIP=$((SKIP + 4))
  echo "  No object found to drill into - /object/{id} detail tests skipped."
fi

# legacy IQL search (informational)
if [ -n "$FIRST_TYPE_NAME" ]; then
  ro_get "GET /iql/objects (legacy)" \
    "/iql/objects" -G \
    --data-urlencode "iql=objectType = \"${FIRST_TYPE_NAME}\""
  result "GET /iql/objects (legacy iql)" probe "$RO_CODE"
fi

# ---------------------------------------------------------------------------
# 3. Status types
# ---------------------------------------------------------------------------
echo
echo "--- Config: status types ---"
if [ -n "$SCHEMA1" ]; then
  ro_get "GET /config/statustype" \
    "/config/statustype" -G --data-urlencode "objectSchemaId=$SCHEMA1"
  result "GET /config/statustype (schema $SCHEMA1)" 2xx "$RO_CODE"
  first_status_id="$(first_id)"
  if [ -n "$first_status_id" ]; then
    ro_get "GET /config/statustype/{id}" "/config/statustype/$first_status_id"
    result "GET /config/statustype/$first_status_id" 2xx "$RO_CODE"
  else
    # list is empty - probe a few well-known ids (404s are fine)
    probed=0
    for sid in 1 2 3; do
      ro_get "GET /config/statustype/{id}" "/config/statustype/$sid"
      result "GET /config/statustype/$sid (probe)" probe "$RO_CODE"
      probed=$((probed + 1))
    done
  fi
fi

# ---------------------------------------------------------------------------
# 4. Icons
# ---------------------------------------------------------------------------
echo
echo "--- Icons ---"
ro_get "GET /icon/global" "/icon/global"
result "GET /icon/global" 2xx "$RO_CODE"
first_icon_id="$(first_id)"
if [ -n "$first_icon_id" ]; then
  ro_get "GET /icon/{id}" "/icon/$first_icon_id"
  result "GET /icon/$first_icon_id" 2xx "$RO_CODE"
else
  SKIP=$((SKIP + 1))
  echo "  [NO-DATA] GET /icon/{id} - no icon id found"
fi

# ---------------------------------------------------------------------------
# 5. Experimental: structure templates
# ---------------------------------------------------------------------------
echo
echo "--- Experimental ---"
ro_get "GET /operations/template/list" \
  "/operations/template/list" -H 'X-Experimental: true'
result "GET /operations/template/list (X-Experimental)" probe "$RO_CODE"

# ---------------------------------------------------------------------------
# Skipped by design (would require IDs not discoverable via GET endpoints,
# or only exist as write operations in the spec):
#   /importsource/**            - needs import-source uuid (not listable via GET)
#   /progress/category/imports/{id} - needs import progress id
#   /object/aql, /object/navlist/*   - POST-only in spec (write-family verbs banned here)
#   /objectschema|objecttype|object|statustype create/update/delete - banned (non-GET)
# ---------------------------------------------------------------------------

echo
echo "=============================================="
echo "=== SUMMARY: pass=$PASS  fail=$FAIL  no-data/skipped=$SKIP ==="
if [ "$FAIL" -gt 0 ]; then
  echo "Failed endpoints:"
  printf '  - %s\n' "${FAILED_LIST[@]}"
  exit 1
fi
echo "All executed read-only tests passed."
exit 0

