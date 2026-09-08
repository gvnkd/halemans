# Assets (Jira Insights) REST API — Integration Guide

Target: `https://jira.officesvc.bz` (Jira Server **10.3.23**)
Plugin: **Assets / "Активы"** — RIA Labs *Insight* `com.riadalabs.jira.plugins.insight` v**20.3.23**
Audience: developers of the Haskell Assets module for the Halemans app.

> Status legend used throughout:
> **[VERIFIED]** — exercised successfully against this instance on 2026-09-08 with a real account.
> **[SPEC]** — present in the official Atlassian Assets OpenAPI spec / generated client, not yet
> exercised here.
> **[BROKEN-HERE]** — route exists but does not behave as spec on this instance.

---

## 1. What this API is

The plugin serves the **Atlassian Assets** REST API (schema / object type / object model,
AQL query language) on the Jira Server, plus a legacy RIA Labs "Insight" API underneath.
Two bases exist on the same host:

| Base | Purpose |
|---|---|
| `https://jira.officesvc.bz/rest/assets/latest` | **Primary. Use this.** Assets-compatible API. All paths in §4 are relative to it. |
| `https://jira.officesvc.bz/rest/insight/1.0` (also `/rest/insight/latest`) | Legacy RIA Labs API. Same data. Note: icon/avatar URLs *returned inside* responses often point here (e.g. `.../rest/insight/1.0/icon/25/icon.png`), while AQL results embed `/rest/assets/latest/...` avatar URLs. Normalize both. |

Full spec (all endpoints, request/response schemas):
`https://dac-static.atlassian.com/cloud/assets/swagger.v3.json`

The npm package `jira-insights-api@2.1.2` (used by `3rd_party/jira-insights-mcp`) is a
generated client from that spec. Its method→path table lives in
`DefaultService.js` (methods in §7 show the generated-client name in parentheses).

---

## 2. Authentication

**[VERIFIED]** Bearer API token works:

```
Authorization: Bearer <JIRA_TOKEN>
```

The generated client instead uses HTTP Basic `base64(email:apiToken)` — Jira Server accepts
both. For the Haskell module: primary Bearer, Basic as fallback behind a config flag.

- Unauthenticated / invalid token on a *known* route → `401` (JSON error body).
- Unauthenticated on an *unknown* route → `302` redirect to the login page (HTML).
  Treat `302` as "not found / not authenticated", **never** follow redirects in the client.

---

## 3. The three error shapes (important for parsing)

| # | Shape | When | Example |
|---|---|---|---|
| A | Plugin JSON error | route exists, business-level failure (incl. entity not found) | HTTP 404 `{"errorMessages":["NotFoundInsightException: Не удалось найти элемент «Объект» с идентификатором «1»"],"errors":{}}` |
| B | Jira framework XML | route **not registered** under `/rest/assets/...` | HTTP 404 `<?xml ...?><status><status-code>404</status-code><message>HTTP 404 Not Found</message></status>` |
| C | `302` → login HTML | unauthenticated fallback / unknown base | — |

Notes:
- Error text in shape A is in **Russian** (`NotFoundInsightException: ...`). Detect errors
  by HTTP code + `errorMessages` array, and match only the stable `NotFoundInsightException:`
  prefix if you need to distinguish "not found" from other 4xx.
- Shape A 404 vs shape B 404 is a reliable way to probe whether an endpoint exists.
- `200` bodies are JSON, `Content-Type: application/json`.

---

## 4. Endpoint reference (relative to `/rest/assets/latest`)

### 4.1 Schemas

| Method & Path | Status | Returns |
|---|---|---|
| `GET /objectschema/list` | **[VERIFIED]** | `{"objectschemas": [ObjectSchema, ...]}` — all schemas visible to the user. No pagination params needed in practice (3 schemas here). |
| `GET /objectschema/{id}` | **[VERIFIED]** | `ObjectSchema` |
| `POST /objectschema/create` | [SPEC] | body `{"name":..,"objectSchemaKey":..,"description":..}` (client: `schemaCreate`) |
| `PUT /objectschema/{id}` | [SPEC] | body `ObjectSchemaIn` (client: `schemaUpdate`) |
| `DELETE /objectschema/{id}` | [SPEC] | (client: `schemaDelete`) |
| `GET /objectschema/{id}/objecttypes?excludeAbstract=false` | **[BROKEN-HERE]** | Returns `[]` for a non-admin user even when the schema has types. **Do not rely on it. Use the flat variant below.** |
| `GET /objectschema/{id}/objecttypes/flat?includeObjectCounts=true` | **[VERIFIED]** | Bare array `[ObjectType, ...]` — *the* type list that works. |
| `GET /objectschema/{id}/attributes?query=""` | **[VERIFIED]** | `[]` (empty for this user; shape = bare array) |

`ObjectSchema` fields observed: `id` (int), `name`, `objectSchemaKey` (e.g. `"ACCESS"`),
`status` (`"Ok"`), `description`, `created`, `updated`, `objectCount`, `objectTypeCount`.

### 4.2 Object types

| Method & Path | Status | Returns |
|---|---|---|
| `GET /objecttype/{id}` | **[VERIFIED]** | `ObjectType` |
| `GET /objecttype/{id}/attributes?query=""&includeChildren=false` | **[VERIFIED]** | **Bare array** of `ObjectTypeAttribute`. (Empty `[]` for this test user — treat empty as "not accessible / no data", not as error.) Optional query params: `onlyValueEditable`, `orderByName`, `includeValuesExist`, `excludeParentAttributes`, `orderByRequired` (all bool). |
| `POST /objecttype/create` | [SPEC] | body `ObjectTypeIn` |
| `PUT /objecttype/{id}` | [SPEC] | body `ObjectTypeIn` |
| `DELETE /objecttype/{id}` | [SPEC] | — |
| `POST /objecttype/{id}/position` | [SPEC] | reorder (body `ObjectTypePosition`) |
| `POST /objecttypeattribute/{objectTypeId}` | [SPEC] | create attribute (body `ObjectTypeAttributeCreate`) |
| `PUT /objecttypeattribute/{objectTypeId}/{id}` | [SPEC] | update attribute |
| `DELETE /objecttypeattribute/{id}` | [SPEC] | delete attribute |

`ObjectType` fields observed: `id` (int), `name`, `type` (0 = concrete), `description?,icon
{name,id,url16,url48}, objectSchemaId, position, created, updated, objectCount, parentObjectTypeId?, inherited, abstractObjectType`.

Types form a **parent/child hierarchy** (`parentObjectTypeId`). AQL `objectType =` matches
only the exact type (see §5.3).

### 4.3 Objects

| Method & Path | Status | Returns |
|---|---|---|
| `GET /object/{id}` | **[VERIFIED]** | `Object` (full detail incl. `attributes` array) |
| `GET /object/{id}/attributes` | **[VERIFIED]** | Bare array `[ObjectAttribute, ...]` |
| `GET /object/{id}/history?asc=true` | **[VERIFIED]** | Bare array `[ObjectHistory, ...]` |
| `GET /object/{id}/referenceinfo` | **[VERIFIED]** | Bare array; entry contains `referenceTypes: [ReferenceType, ...]` |
| `GET /objectconnectedtickets/{objectId}/tickets` | **[VERIFIED]** | `{"tickets": [Ticket, ...], "allTicketsQuery": "<URL-encoded JQL>"}` |
| `POST /object/create` | [SPEC] | body `{"objectTypeId":..,"attributes":[{objectTypeAttributeId, objectAttributeValues:[{value}]}]}` |
| `PUT /object/{id}` | [SPEC] | body `ObjectIn` |
| `DELETE /object/{id}` | [SPEC] | — |

`Object` fields observed (superset in spec): `id` (int — *numeric string in spec, int in JSON*),
`label` (display name, e.g. `"1С:Управляйка"`), `objectKey` (`"ACCESS-3445723"` =
`<objectSchemaKey>-<id>`), `avatar {url16,url48}`, `objectType {id,name,...}` (id of the
*actual* concrete/child type), `created`, `updated`, `attributes: [ObjectAttribute]`.

`ObjectAttribute`: `{ id, objectTypeAttribute: {id, name, label, type, defaultType:{id,name}, editable, system, indexed, ...}, objectAttributeValues: [ObjectAttributeValue] }`.

`ObjectAttributeValue` (one of the sub-fields is populated, depending on attribute kind):
`value?` (raw string), `displayValue` (formatted text), `searchValue?`,
`referencedObject?` (another `Object`), `user?`, `group?`, `status?`, `additionalValue?`.

`ObjectHistory`: `{ actor: {name, displayName, avatarUrl}, id, created, type, objectId,
affectedAttribute?, oldValue?, newValue? }`.

Attribute `type` enum (from `ObjectTypeAttribute.type`): `0`=default/text, `1`=object
reference, `2`=user, `4`=group, `7`=status.

### 4.4 Search (AQL / IQL)

| Method & Path | Status | Returns |
|---|---|---|
| `GET /aql/objects` | **[VERIFIED]** | `ObjectListResult`. Query params: `qlQuery` (required), `page` (1-based), `resultPerPage`, `includeAttributes=true`, `includeAttributesDeep=1`, `includeTypeAttributes=false`, `includeExtendedInfo=false`. **URL-encode `qlQuery`** (use `curl -G --data-urlencode`, or `urlEncode` in Haskell). |
| `GET /iql/objects` | **[VERIFIED]** | Same `ObjectListResult`. Query params: `iql`, `page`, `resultPerPage`, same includes. Legacy query language; same result shape. Empty result observed for a type-scoped query (see §5.3). |
| `POST /object/aql` | [SPEC] | body `{"qlQuery": "..."}` + query params `startAt`, `maxResults` (0-based offset), `includeAttributes`. Rate limit per spec: **500 req/min**. Client: `objectsByAql`. |
| `POST /object/aql/totalcount` | [SPEC] | body `{"qlQuery":"..."}` → total count |
| `POST /object/navlist/aql` | [SPEC] | body `ObjectFilterParams` (nested tree navigation) |

`ObjectListResult` — observed subset:

```json
{
  "objectEntries": [ { "id": 3445723, "label": "...", "objectKey": "ACCESS-3445723", "avatar": {...}, ... } ],
  "objectTypeAttributes": [],
  "objectTypeId": 0,
  "objectTypeIsInherited": false,
  "abstractObjectType": false,
  "totalFilterCount": 0,
  "startIndex": 1,
  "toIndex": ...
}
```

Spec fields to expect when populated: `pageObjectSize`, `pageNumber`, `qlQuery`,
`qlQuerySearchResult`, `conversionPossible`.

**Pagination model (GET):** `page` starts at **1**; `totalFilterCount` = total matches;
`startIndex`/`toIndex` = object position range of the current page. Iterate while
`toIndex < totalFilterCount`. (POST variant uses offset/limit: `startAt` 0-based, `maxResults`.)

---

## 5. AQL (Assets Query Language)

### 5.1 Grammar overview

```
<condition> [ AND | OR ] <condition> ... [ order by <attr|label> [asc|desc] ]
<condition> := <attribute|.attr.ref> <op> <value>
             | objectSchema = "<schema name>"
             | objectType = "<type name>"
             | objectType in objectTypeAndChildren("<type name>")
             | objectId = <numeric id>
             | Key = "<SCHEMAKEY>-<id>"
             | object having <refFn>(<AQL>|<JQL>)
```

- Case-insensitive language. `AND`/`OR` must be **uppercase**.
- String values with spaces (or any string) → double quotes: `Name = "John Doe"`.
- Escape inner quotes with backslash: `Screen = "15\""`.
- `==` case-sensitive equality, `=` case-insensitive.

Operators: `=`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `like` (substring, ci), `not like`,
`in (v1, v2, ...)`, `not in (...)`, `startswith`, `endswith`, `is EMPTY`, `is not EMPTY`.

### 5.2 Functions

| Category | Functions |
|---|---|
| Date/time | `now()`, `now(-2h 15m)` (offsets), `startOfDay()`, `endOfDay()`, `startOfMonth()`, `endOfMonth()`, `startOfYear()`, `endOfYear()` |
| Users | `currentUser()`, `currentReporter()`, `user(...)` |
| Groups | `group("jira-users")` (`User in group(...)`) |
| Projects | `currentProject()` |
| References | `inboundReferences(aql)` / `inR(aql)`, `outboundReferences(aql)` / `outR(aql)`, `connectedTickets(jql)`, `objectTypeAndChildren("<name>")` |

Examples:

```
objectType = "Server" AND "Operating System" = "Ubuntu"
"Belongs to Department".Name = HR                    -- dot notation walks references
object having connectedTickets(Project = VK)         -- JQL inside
objectType in objectTypeAndChildren("Access")        -- type + all descendants
Created > now(-30d)
objectType = "Employee" order by Name desc
```

### 5.3 Gotchas observed on this instance

1. **`objectType = "X"` does NOT include child types.** A query for the top type of the
   `Access` schema (`id 2191`) returned 0 objects even though schema objects exist —
   the actual objects live in child types (e.g. `id 7603`). **Use
   `objectType in objectTypeAndChildren("X")`** for type-scoped searches, or prefer
   `objectSchema = "<schema name>"` [VERIFIED working] which always covers the whole schema.
2. Schema name in AQL is the **schema display name** (`"Access"`), not the key (`"ACCESS"`).
   (Test the key variant only if names are ambiguous/duplicated.)
3. Query strings must be URL-encoded when sent as `qlQuery=...`.

### 5.4 Building AQL safely in Haskell

- Always double-quote the RHS string and escape `\` and `"`.
- Validate identifiers (attribute names may contain spaces — keep a local whitelist of
  attribute names fetched from `/objecttype/{id}/attributes` if possible).
- Keep the query a single `Text`; log it verbatim when debugging.

---

## 6. Auxiliary endpoints

| Method & Path | Status | Notes |
|---|---|---|
| `GET /config/statustype?objectSchemaId={id}` | **[VERIFIED]** | Per-schema list — returns `[]` here (statuses are global). Bare array. |
| `GET /config/statustype/{id}` | **[VERIFIED]** | `{id, name, category}`. Global ids observed: `1`=Active, `2`=Running, `3`=In Service, `4`=Support Requested, `5`=Action Needed, `6`=Stopped, `7`=Closed (category: 1=green, 2=yellow, 0=red). Probe ids if list is empty. |
| `POST /config/statustype` [SPEC], `PUT/DELETE /config/statustype/{id}` [SPEC] | | status type management |
| `GET /icon/global` | **[VERIFIED]** | Bare array `[Icon...]` (~1700 icons). |
| `GET /icon/{id}` | **[VERIFIED]** | `Icon` = `{id, name, url16, url48}`. |
| `GET /operations/template/list` + header `X-Experimental: true` | **[VERIFIED 404]** | Feature not enabled on this build. Skip in tests. |
| `POST /operations/structurefromtemplate` | [SPEC] | experimental structure creation |
| `GET /importsource/{uuid}/...` (6 routes), `GET /progress/category/imports/{id}` | [SPEC] | Import pipeline. UUIDs are not discoverable via GET endpoints — out of scope for read-only tooling. The import JSON-schema contract is documented in `3rd_party/jira-insights-mcp/docs/external imports - schem and mapping.md` and served at `https://api.atlassian.com/jsm/insight/imports/external/schema/versions/2023_10_19`. |
| `POST /global/config/objectschema/{id}/property` | [SPEC] | global schema properties |

---

## 7. Client method ↔ endpoint map (from `jira-insights-api` @2.1.2)

Useful when cross-referencing the MCP code in `3rd_party/jira-insights-mcp`:

| Generated-client method | HTTP |
|---|---|
| `schemaList()` | GET `/objectschema/list` |
| `schemaFind({id})` | GET `/objectschema/{id}` |
| `schemaCreate / schemaUpdate / schemaDelete` | POST `/objectschema/create`, PUT/DELETE `/objectschema/{id}` |
| `schemaFindAllObjectTypes({id, excludeAbstract})` | GET `/objectschema/{id}/objecttypes` |
| `schemaFindAllObjectTypesFlat({id, query, exclude, includeObjectCounts})` | GET `/objectschema/{id}/objecttypes/flat` |
| `schemaFindAllAttributes({id, onlyValueEditable, extended, query})` | GET `/objectschema/{id}/attributes` |
| `objectTypeFind / Update / Delete` | GET/PUT/DELETE `/objecttype/{id}` |
| `objectTypeCreate` | POST `/objecttype/create` |
| `objectTypeFindAllAttributes({id, ...})` | GET `/objecttype/{id}/attributes` |
| `objectTypeAttributeCreate / Update / Delete` | POST `/objecttypeattribute/{objectTypeId}`, PUT `/objecttypeattribute/{objectTypeId}/{id}`, DELETE `/objecttypeattribute/{id}` |
| `objectFind({id})` | GET `/object/{id}` |
| `objectFindAttributes / objectFindHistoryEntries / objectFindReferences` | GET `/object/{id}/attributes|history|referenceinfo` |
| `objectCreate / objectUpdate / objectDelete` | POST `/object/create`, PUT/DELETE `/object/{id}` |
| `objectsByAql({requestBody, startAt, maxResults, includeAttributes})` | POST `/object/aql` |
| `postObjectAqlTotalcount` | POST `/object/aql/totalcount` |
| `aqlFindObjects({qlQuery, page, resultPerPage, ...})` | GET `/aql/objects` |
| `iqlFindObjects({iql, page, resultPerPage, ...})` | GET `/iql/objects` |
| `objectNavigatorList / objectNavigatorListDeprecated` | POST `/object/navlist/aql` / `/object/navlist/iql` |
| `objectConnectedTickets({objectId})` | GET `/objectconnectedtickets/{objectId}/tickets` |
| `statusList / statusCreate / statusFind / statusUpdate / statusDelete` | GET/POST/PUT/DELETE `/config/statustype[/{id}]` |
| `iconFind / iconFindGlobalIcons` | GET `/icon/{id}` / `/icon/global` |
| `generalConfigurationUpdate` | POST `/global/config/objectschema/{id}/property` |
| `operationsListOfTemplates / operationsCreateAStructure` | GET/POST `/operations/template/list`, `/operations/structurefromtemplate` |

---

## 8. Observed behavioral quirks (design around these)

1. **Mixed response envelopes.** The same logical collection comes back as a *bare JSON
   array* (`/objecttypes/flat`, `/icon/global`, `/object/{id}/attributes`, `/object/{id}/history`,
   `/config/statustype`, `/objecttype/{id}/attributes`) or wrapped
   (`/objectschema/list` → `objectschemas`, `/aql/objects` → `objectEntries`).
   The Haskell `FromJSON` instances must accept both forms (e.g. a helper
   `eitherArrayOr [a] <- withObjectEnvelope` for `.values/.objects/.objectEntries/.icons/...`).
2. **IDs are JSON numbers** on this server (spec types them as strings). Parse as `Int64`
   and render into URLs as text.
3. **Non-admin visibility:** account used for verification is *not* an admin
   (`insightAdministrator: false`). For such users: `/objectschema/{id}/objecttypes` (tree)
   → `[]`, `/objectschema/{id}/attributes` → `[]`, `/objecttype/{id}/attributes` → `[]`,
   yet objects themselves are fully readable. If the module must also serve admins, re-test
   the three endpoints above with an admin token.
4. **Icon/avatar URL bases differ** between responses (`/rest/insight/1.0/...` vs
   `/rest/assets/latest/...`). Store them verbatim; do not reconstruct.
5. **Russian error text** in `errorMessages` — match structure, not language.
6. **No redirects:** the server 302s unknown/unauth paths to an HTML login page. The client
   must not follow redirects and must classify 3xx as an error.
7. **Locale:** UI/errors localized to `ru_RU`; do not localize-dependent anything.
8. Server timezone `Europe/Moscow` (+03:00); timestamps in responses are ISO-8601 with offset
   (e.g. `2026-09-08T09:46:32.609+0300`) or without millis.

---

## 9. Suggested Haskell module design

Proposed layout (inside the Halemans repo — pick the existing conventions):

```
halemans-assets/
  App/Halemans/Assets/Client.hs     -- newtype AssetsClient { baseUrl, token, manager }
                                    -- http request plumbing; no redirect following;
                                    -- parses the three error shapes (§3)
  App/Halemans/Assets/Types.hs      -- ObjectSchema, ObjectType, ObjectTypeAttribute,
                                    -- Object, ObjectAttribute, ObjectAttributeValue,
                                    -- ObjectHistory, ObjectListResult, Ticket, Status, Icon
                                    -- (aeson FromJSON/ToJSON; tolerant of bare array
                                    --  and wrapped envelopes; IDs as Int64)
  App/Halemans/Assets/Aql.hs        -- Aql = single Text + safe builders:
                                    --   eq attr value, inAttr attr (set values),
                                    --   schemaEq name, typeInChildren name,
                                    --   andAql, orAql, orderBy attr dir,
                                    -- string escaping (\" and \\) (§5.4)
  App/Halemans/Assets/Errors.hs     -- data AssetsError = AuthFailed | NotFound Int64
                                    --                        | Upstream Int Text
                                    --                        | InvalidResponse Int Text
```

Sketch of the core read operations:

```haskell
listSchemas   :: AssetsClient -> IO [ObjectSchema]     -- GET /objectschema/list
listTypes     :: AssetsClient -> ObjectSchemaId -> IO [ObjectType]
                                                -- GET /objectschema/{id}/objecttypes/flat
objectDetail  :: AssetsClient -> ObjectId -> IO Object -- GET /object/{id}
searchObjects :: AssetsClient -> Aql -> Int --page (1-based) -> Int --perPage -> IO ObjectListResult
                                                -- GET /aql/objects?qlQuery=.. (url-encoded)
connected     :: AssetsClient -> ObjectId -> IO [Ticket]
                                                -- GET /objectconnectedtickets/{id}/tickets
```

Client requirements checklist:
- `Accept: application/json`, no redirect following, timeout ~30 s, retry only on 5xx/timeout
  (idempotent reads; max 2 retries, backoff).
- Map HTTP `401` → `AuthFailed`; `404` + shape-A JSON containing `NotFoundInsightException:`
  → `NotFound id`; other non-2xx → `Upstream code bodyText`; shape-B XML or shape-C HTML on a
  *known* path → `InvalidResponse` (log the first 200 chars).
- Pagination helper: `searchAll client aql` that pages through `totalFilterCount`.
- Config via env or config file: `ASSETS_BASE_URL` (default
  `https://jira.officesvc.bz/rest/assets/latest`), `JIRA_TOKEN`; optional `ASSETS_API_URL_MODE`
  (bearer|basic) + `JIRA_EMAIL` for basic mode.
- Property tests for the tolerant envelope decoder (bare array vs wrapper) and the AQL
  escaper (quotes, backslashes, Unicode, Cyrllic — data on this instance is ru/en mixed).

Test plan (unit, recorded fixtures):
1. Fixture set from the 2026-09-08 run: schema list, flat types for schema 110,
   `GET /object/3445723` + attributes + history + referenceinfo + connected tickets,
   AQL result for `objectSchema = "Access"`, status `1..3`, icon `25`.
2. Error fixtures: shape A 404 (Russian text), shape B XML 404, 302 to login.

Acceptance smoke test (live, read-only only — all GET): run
`./assets_api_readonly_test.sh` at the repo root (45 checks, exit 0 = green).

---

## 10. Canonical read flow (the pattern all other features build on)

```
1. GET /objectschema/list                                → schema ids + keys
2. GET /objectschema/{id}/objecttypes/flat               → concrete types (id, name, counts)
3. GET /aql/objects?qlQuery=<URL-encoded AQL>            → objectEntries (id, objectKey, label)
      e.g. objectSchema = "Capacity CMDB"
           or objectType in objectTypeAndChildren("Host")
           or Key = "CHCMDB-18955539"
4. GET /object/{id}                                      → full detail incl. attributes
5. optional: /object/{id}/history, /referenceinfo,
             /objectconnectedtickets/{id}/tickets
```

Worked example (verified data):

```bash
B=https://jira.officesvc.bz/rest/assets/latest
curl -s -H "Authorization: Bearer $JIRA_TOKEN" \
  -G "$B/aql/objects" \
  --data-urlencode 'qlQuery=objectSchema = "Capacity CMDB"' \
  --data-urlencode page=1 --data-urlencode resultPerPage=5 | jq '.objectEntries[0] | {id, objectKey, label, objectType: .objectType.name}'
# → {"id":18955539,"objectKey":"CHCMDB-18955539","label":"10auth-cc01","objectType":"..."}

curl -s -H "Authorization: Bearer $JIRA_TOKEN" "$B/object/3445723" | jq '{id, label,
      objectKey, type: .objectType.name, created, updated,
      nAttrs: (.attributes | length)}'
```

---

## 11. References

- OpenAPI spec (authoritative for [SPEC] entries): `https://dac-static.atlassian.com/cloud/assets/swagger.v3.json`
- Generated client with all paths: npm `jira-insights-api@2.1.2` → `dist/generated/services/DefaultService.js`
- MCP wrapper (usage examples, AQL syntax guide, import contract): `3rd_party/jira-insights-mcp/`
  (`src/handlers/*.ts`, `src/handlers/resource-handlers.ts` AQL guide, `docs/external imports - schem and mapping.md`)
- Read-only regression script: `./assets_api_readonly_test.sh` (repo root)
- Verification run: 2026-09-08, 45 PASS / 0 FAIL / 1 expected NO-DATA, user `sbubnov` (non-admin)

