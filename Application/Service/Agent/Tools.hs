module Application.Service.Agent.Tools (
    AgentContext (..),
    agentToolDefinitions,
    agentToolDefinitionsFor,
    executeAgentTool,
    requiredPrivilegeFor,
    channelTokenEnv,
) where

import Application.Helper.Controller (userPrivileges)
import Application.Helper.DashboardConfig (DashboardCard (..), FacetRef (..), MatchClause (..), MatchOp (..), decodeDashboardConfig, encodeDashboardConfig, facetRefText, parseFacetRef)
import Application.Pipeline.Actions (ackAlert, addComment, closeAlert, unackAlert)
import Application.Pipeline.Blackouts (blackoutEndsLabel, openEndedBlackoutEndsAt)
import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText, parseAlertField)
import Application.Service.AlertScope (alertVisibleWith, scopeForUser)
import Application.Service.Api.Alerts (AlertFilters (..), AlertPage (..), defaultFilters, listAlertsPage)
import Application.Service.Api.Cursor (decodeCursor)
import Application.Service.Api.Token (newApiToken)
import Application.Service.DashboardCards (cardBaseQuery)
import Application.Service.HostGroups (hostGroupsToJson)
import Application.Service.Llm (LlmProviderConfig (..), ToolCall (..))
import Application.Service.Llm.DbConfig (currentLlmConfig)
import Application.Service.Mattermost.Render (mattermostTemplateNames)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, void, when)
import Data.Aeson (Value (..), object, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as Text
import Data.Text.Read ()
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Traversable (traverse)
import qualified Data.Vector as Vector
import Generated.Types hiding (createDashboard)
import IHP.Fetch (fetch, fetchCount, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, limit, orderByAsc, orderByDesc, query)
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
import Text.Read (readMaybe)

-- Agent tool registry (internal API milestone). One implementation backs the
-- web chat, the internal HTTP API and the MCP server. Every tool declares
-- the privilege it needs; the executor enforces it against the act-as
-- user's REAL roles BEFORE dispatch (never the model's discretion), and
-- agentToolDefinitionsFor hides tools the user cannot call from the prompt.
-- Mutating tools follow the two-phase confirm flow (confirmed flag).

data AgentContext = AgentContext
    { acUser :: User
    -- ^ The user the agent acts on behalf of (act-as). Tool-level privilege
    -- checks run against this user's roles.
    , acLanguage :: Text
    -- ^ Display language name (users.settings.language) for hint text.
    , acSessionId :: Maybe (Id AgentSession)
    -- ^ Current chat session when invoked from the web agent; Nothing on
    -- the internal HTTP API / MCP server. Tools like explain_last_turn
    -- need it to locate the conversation.
    }

-- (name, description, properties, required privilege)
toolCatalog :: [(Text, Text, [(Text, Value)], Maybe Text)]
toolCatalog =
    [ -- P1: day-2 ops
      tool "search_alerts" "Search alerts newest first (any status unless filtered). env/host/service accept shell globs (*, ?) or exact values; title is a glob match; status/severity exact. limit caps the page (max 100); when the output has a next_cursor line, pass it as the cursor argument for the next page. The header reports the total match count. Requires the view privilege." [envP, hostP, serviceP, sevP, statusP, titleP, limitP, cursorP] (Just "view")
    , tool "get_alert" "Get one alert by id: the full record regardless of status (firing/ack/resolved/stalled/closed) — status, severity, effective env/host/service, title, muted flag, fingerprint, occurrences, timestamps (first/last seen, acked/resolved/closed), ack/close details, group, comments and escalation state. Use this when you have an alert URL or id that search_alerts does not explain. Requires the view privilege." [req "alert_id" "alert UUID"] (Just "view")
    , tool "list_environments" "List monitoring environments with the count of currently open (non-closed) alerts in each. Requires the view privilege." [] (Just "view")
    , tool "ack_alert" "Acknowledge an alert by id (stops escalation). Requires the ack privilege." [req "alert_id" "alert UUID", opt "comment" "ack comment"] (Just "ack")
    , tool "unack_alert" "Remove the acknowledgement from an alert (restarts escalation). Requires the ack privilege." [req "alert_id" "alert UUID"] (Just "ack")
    , tool "close_alert" "Close an alert by id (it is resolved/done). Only acked or stalled alerts can be closed — ack a firing alert first. Requires the close privilege." [req "alert_id" "alert UUID", opt "reason" "close reason"] (Just "close")
    , tool "ack_alerts" "Acknowledge several alerts by id in one call (all or nothing: any unknown/invisible id aborts before anything changes). Returns per-id results. Two-phase (confirmed). Requires the ack privilege." [req "alert_ids" "JSON array of alert UUID strings, e.g. [\"id1\",\"id2\"]", opt "comment" "ack comment", optC "confirmed"] (Just "ack")
    , tool "close_alerts" "Close several alerts by id in one call (all or nothing: any unknown/invisible id aborts before anything changes). Only acked or stalled alerts can be closed — firing alerts are reported as skipped, ack them first. Returns per-id results. Two-phase (confirmed). Requires the close privilege." [req "alert_ids" "JSON array of alert UUID strings, e.g. [\"id1\",\"id2\"]", opt "reason" "close reason", optC "confirmed"] (Just "close")
    , tool "ack_by_match" "Acknowledge all open (non-closed) alerts matching a dashboard-style match, limited to what the acting user can see: a JSON object mapping facets to globs (e.g. {\"host\": \"web-*\"}, keys may be plain field names, field:/attr:/label: references) or an array of dashboard match clauses. Returns per-id results. Two-phase (confirmed). Requires the ack privilege." [req "match" "match JSON (object of facet->glob or clause array)", opt "comment" "ack comment", opt "limit" "max alerts, default 50, max 200", optC "confirmed"] (Just "ack")
    , tool "close_by_match" "Close all open (non-closed) alerts matching a dashboard-style match, limited to what the acting user can see: a JSON object mapping facets to globs (e.g. {\"host\": \"web-*\"}, keys may be plain field names, field:/attr:/label: references) or an array of dashboard match clauses. Only acked or stalled alerts can be closed — firing alerts are reported as skipped, ack them first (ack_by_match). Returns per-id results. Two-phase (confirmed). Requires the close privilege." [req "match" "match JSON (object of facet->glob or clause array)", opt "reason" "close reason", opt "limit" "max alerts, default 50, max 200", optC "confirmed"] (Just "close")
    , tool "comment_alert" "Add a comment to an alert by id. Requires the view privilege." [req "alert_id" "alert UUID", req "body" "comment text"] (Just "view")
    , tool "list_blackouts" "List silence/maintenance windows (newest first)." [] Nothing
    , tool "create_blackout" "Create a silence/maintenance window. Scope is optional env/host/service/title values (at least one); env/host/service names containing * or ? are stored as shell globs against the raw alert names instead of being resolved to inventory rows; a title is always stored as a glob (plain text matches exactly). Omit ends_at (or pass null/empty) for an open-ended window that never expires. Two-phase: call with confirmed=false first to show the plan. Requires manage_blackouts." [opt "env" "environment name or glob", opt "host" "host name or glob", opt "service" "service name or glob", opt "title" "alert title glob (e.g. test memory leak*; plain text matches exactly)", req "starts_at" "ISO8601, e.g. 2026-09-24T18:00:00Z", opt "ends_at" "ISO8601; omit/null for open-ended (forever)", opt "reason" "why", optC "confirmed"] (Just "manage_blackouts")
    , tool "delete_blackout" "Delete a blackout by id. Two-phase (confirmed). Requires manage_blackouts." [req "blackout_id" "blackout UUID", optC "confirmed"] (Just "manage_blackouts")
    , tool "list_dashboards" "List the current user's dashboards: name, whether it is the default, and card count." [] Nothing
    , tool "get_dashboard" "Get one of the current user's dashboards by name or id: the full card config JSON (ready to edit and pass to update_dashboard), plus default/position metadata. Use this before modifying an existing dashboard." [opt "name" "dashboard name", opt "id" "dashboard UUID (from the page URL /dashboards/<uuid>)"] Nothing
    , tool "get_dashboard_schema" "Get the dashboard card config schema: card fields, facet references, match operators and an example card." [] Nothing
    , tool "validate_dashboard" "Validate a dashboard config without creating it: checks card JSON, then counts currently matching open alerts per card. Always call this before create_dashboard and show the plan to the user." [req "name" "dashboard name", req "config" "card config as a JSON array string"] Nothing
    , tool "create_dashboard" "Create a dashboard for the current user. Two-phase: call with confirmed=false first and present the returned plan; only after the user explicitly agrees call again with confirmed=true." [req "name" "dashboard name", req "config" "card config as a JSON array string", optC "confirmed"] Nothing
    , tool "update_dashboard" "Replace a dashboard's config (matched by name, must belong to the current user). Two-phase (confirmed)." [req "name" "dashboard name", req "config" "card config as a JSON array string", optC "confirmed"] Nothing
    , tool "delete_dashboard" "Delete a dashboard by name (must belong to the current user). Two-phase (confirmed)." [req "name" "dashboard name", optC "confirmed"] Nothing
    , tool "set_default_dashboard" "Mark a dashboard as the current user's default." [req "name" "dashboard name"] Nothing
    , -- P2: configuration management
      tool "list_teams" "List teams with their members. Requires manage_users." [] (Just "manage_users")
    , tool "create_team" "Create a team. mattermost_team/mattermost_channel set the team's default Mattermost destination (stored in the team defaults; a rule's channel config overrides it). host_groups is a comma-separated list of zabbix host group names. Two-phase (confirmed). Requires manage_users." [req "name" "team name", opt "description" "team description", opt "host_groups" "comma-separated zabbix host group names", opt "mattermost_team" "mattermost team name (default: halemans)", opt "mattermost_channel" "mattermost channel for this team's notifications", optC "confirmed"] (Just "manage_users")
    , tool "update_team" "Update a team by name; omit fields you do not change. mattermost_team/mattermost_channel write the team's default Mattermost destination (an empty string removes it). host_groups replaces the list (comma-separated). Two-phase (confirmed). Provisioned items are read-only. Requires manage_users." [req "name" "team name", opt "description" "team description", opt "host_groups" "comma-separated zabbix host group names", opt "mattermost_team" "mattermost team name (empty removes)", opt "mattermost_channel" "mattermost channel (empty removes)", optC "confirmed"] (Just "manage_users")
    , tool "delete_team" "Delete a team (and its memberships) by name. Two-phase (confirmed). Requires manage_users." [req "name" "team name", optC "confirmed"] (Just "manage_users")
    , tool "add_team_member" "Add a user (by email) to a team, optionally as lead. Requires manage_users." [req "team" "team name", req "email" "user email", opt "role" "member (default) or lead"] (Just "manage_users")
    , tool "remove_team_member" "Remove a user (by email) from a team. Requires manage_users." [req "team" "team name", req "email" "user email"] (Just "manage_users")
    , tool "list_escalation_policies" "List escalation policies with their step counts. Requires manage_rules." [] (Just "manage_rules")
    , tool "get_escalation_policy" "Get one escalation policy's steps (JSON). Requires manage_rules." [req "name" "policy name"] (Just "manage_rules")
    , tool "create_escalation_policy" "Create an escalation policy. steps is a JSON array of {after_seconds, target:{kind:team|user, name}} objects. Two-phase (confirmed). Provisioned items are read-only. Requires manage_rules." [req "name" "policy name", req "steps" "steps as a JSON array string", optC "confirmed"] (Just "manage_rules")
    , tool "update_escalation_policy" "Replace an escalation policy's steps (matched by name). Two-phase (confirmed). Provisioned items are read-only. Requires manage_rules." [req "name" "policy name", req "steps" "steps as a JSON array string", optC "confirmed"] (Just "manage_rules")
    , tool "delete_escalation_policy" "Delete an escalation policy by name. Two-phase (confirmed). Provisioned items are read-only. Requires manage_rules." [req "name" "policy name", optC "confirmed"] (Just "manage_rules")
    , tool "list_notification_rules" "List notification rules. Requires manage_rules." [] (Just "manage_rules")
    , tool "create_notification_rule" "Create a notification rule. match is a JSON object ({fields:{env|host|...:value}, labels:{name:glob}}). Two-phase (confirmed). Provisioned items are read-only. Requires manage_rules." [req "name" "rule name", req "match" "match as a JSON object string", opt "severity_threshold" "critical|high|warning|info (default info)", opt "team" "team name", opt "channel" "notification channel name (see list_notification_channels; default browser_push)", opt "channel_config" "channel config as a JSON object string (mattermost per-rule override: {\"team\",\"channel\"}; the rule team's mattermost default applies when empty)", optC "confirmed"] (Just "manage_rules")
    , tool "list_notification_channels" "List notification channels (name, type, enabled, base URL, token env var). Requires manage_rules." [] (Just "manage_rules")
    , tool "create_notification_channel" "Create a notification channel row (type: browser_push|email|mattermost). base_url is the server address (mattermost); token_env NAMES the environment variable holding the credential — never the secret itself. Two-phase (confirmed). Requires manage_rules." [req "name" "channel name", req "type" "browser_push|email|mattermost", opt "base_url" "server base URL (mattermost)", opt "token_env" "env var name holding the token", opt "enabled" "true (default) or false", optC "confirmed"] (Just "manage_rules")
    , tool "update_notification_channel" "Update a notification channel by name; omit fields you do not change. token_env rewrites the config key (empty string removes it). Two-phase (confirmed). Provisioned items are read-only. Requires manage_rules." [req "name" "channel name", opt "type" "browser_push|email|mattermost", opt "base_url" "server base URL (mattermost)", opt "token_env" "env var name holding the token (empty removes)", opt "enabled" "true or false", optC "confirmed"] (Just "manage_rules")
    , tool "delete_notification_channel" "Delete a notification channel by name. Blocked while notification rules reference it. Two-phase (confirmed). Provisioned items are read-only. Requires manage_rules." [req "name" "channel name", optC "confirmed"] (Just "manage_rules")
    , tool "delete_notification_rule" "Delete a notification rule by name. Two-phase (confirmed). Provisioned items are read-only. Requires manage_rules." [req "name" "rule name", optC "confirmed"] (Just "manage_rules")
    , tool "list_grouping_rules" "List alert grouping rules. Requires manage_rules." [] (Just "manage_rules")
    , tool "create_grouping_rule" "Create an alert grouping rule. match is a JSON object, group_key_template a Go-template-like string. Requires manage_rules." [req "name" "rule name", req "match" "match as a JSON object string", req "group_key_template" "group key template", opt "position" "sort position (default end)"] (Just "manage_rules")
    , tool "delete_grouping_rule" "Delete a grouping rule by name. Requires manage_rules." [req "name" "rule name"] (Just "manage_rules")
    , tool "list_sources" "List alert sources (zabbix/grafana/webhook/alertmanager) with enabled state. Requires manage_sources." [] (Just "manage_sources")
    , tool "enable_source" "Enable a source by name (starts polling/accepts webhooks). Requires manage_sources." [req "name" "source name"] (Just "manage_sources")
    , tool "disable_source" "Disable a source by name. Requires manage_sources." [req "name" "source name"] (Just "manage_sources")
    , -- P3: user + admin reads
      tool "get_profile" "Get the current user's profile settings (theme, timezone, language)." [] Nothing
    , tool "update_profile" "Update the current user's profile settings. Omit fields you do not change." [opt "theme" "theme pack name", opt "timezone" "UTC offset string", opt "language" "language code, e.g. en|ru"] Nothing
    , tool "list_api_tokens" "List the current user's API tokens (prefix, scopes, last used)." [] Nothing
    , tool "create_api_token" "Create an API token for the current user. Returns the plaintext token ONCE — show it to the user and tell them to store it. scopes is a comma-separated list (alerts:read, metrics)." [req "name" "token label", opt "scopes" "comma-separated scopes, default alerts:read"] Nothing
    , tool "revoke_api_token" "Revoke one of the current user's API tokens (matched by label)." [req "name" "token label"] Nothing
    , tool "list_users" "List users (email, display name, locked state). Requires manage_users." [] (Just "manage_users")
    , tool "list_roles" "List roles and their privileges. Requires manage_users." [] (Just "manage_users")
    , tool "list_llm_templates" "List LLM prompt templates (name, version, active). Requires manage_rules." [] (Just "manage_rules")
    , tool "list_mattermost_templates" "List the Mattermost message templates (mattermost_root, mattermost_details, mattermost_status, mattermost_fields, mattermost_color): versions, active flags, and the active bodies (slot syntax like {{alert.title}}). Requires manage_rules." [] (Just "manage_rules")
    , tool "update_mattermost_template" "Update a Mattermost message template by name (mattermost_root|mattermost_details|mattermost_status|mattermost_fields|mattermost_color): appends a new version and activates it transactionally, like the admin editor's save+activate. body is the full template text with {{slots}} — call list_mattermost_templates first to see the current body and slot usage. Two-phase (confirmed). Provisioned items are read-only. Requires manage_rules." [req "name" "mattermost_root|mattermost_details|mattermost_status|mattermost_fields|mattermost_color", req "body" "template body with {{alert.*}} / context slots", opt "notes" "version notes", optC "confirmed"] (Just "manage_rules")
    , tool "list_llm_providers" "List configured LLM providers (name, model, enabled) — never the API key. Requires manage_rules." [] (Just "manage_rules")
    , tool "get_llm_config" "Get the enabled LLM provider name, model and endpoint (never the API key). For agent bootstrap." [] Nothing
    , tool "list_alert_groups" "List recent alert groups with open-alert counts. Requires the view privilege." [opt "limit" "max results, default 20, max 100"] (Just "view")
    , tool "explain_last_turn" "Read this conversation's OWN trace of the most recent turn: per-round durations, LLM token counts, executed tool calls with timings and result excerpts, and errors (e.g. stream stalls). Use it to answer the user asking why the agent was slow, silent, or failed." [] Nothing
    , tool "request_confirmation" "Present a plan and ask the user for explicit approval. REQUIRED whenever a change needs the user's yes/no decision — including plans you composed yourself from read-only tools (validate_*, get_*) or a two-phase tool's confirmed=false plan. The chat UI renders Apply/Discard buttons for the plan you pass. Do NOT ask 'shall I apply?' in plain prose; call this tool instead. After the user Applies, proceed with the change; after Discard, do not." [req "summary" "the exact planned change in one or two sentences"] Nothing
    ]
  where
    tool name description props priv = (name, description, props, priv)
    req name description = (name, stringProp description True)
    opt name description = (name, stringProp description False)
    optC name = (name, boolProp "apply for real; false returns the plan without changing anything" False)
    envP = opt "env" "environment name or glob (*, ?)"
    hostP = opt "host" "host name or glob (*, ?)"
    serviceP = opt "service" "service name or glob (*, ?)"
    sevP = opt "severity" "critical|high|warning|info"
    statusP = opt "status" "firing|ack|resolved|stalled|closed"
    titleP = opt "title" "alert title glob, e.g. VIP IP*"
    limitP = opt "limit" "max results, default 20, max 100"
    cursorP = opt "cursor" "next_cursor from a previous search_alerts call (pagination)"

stringProp :: Text -> Bool -> Value
stringProp description required =
    object ["type" .= ("string" :: Text), "description" .= description, "x-required" .= required]

boolProp :: Text -> Bool -> Value
boolProp description required =
    object ["type" .= ("boolean" :: Text), "description" .= description, "x-required" .= required]

requiredPrivilegeFor :: Text -> Maybe Text
requiredPrivilegeFor name = case [priv | (n, _, _, priv) <- toolCatalog, n == name] of
    (priv : _) -> priv
    [] -> Nothing

agentToolDefinitions :: [Value]
agentToolDefinitions = definitionsFrom toolCatalog

-- Tools the act-as user may actually call: everything without a privilege
-- requirement plus the ones whose privilege the user's roles grant. The
-- model never sees the rest, so it cannot burn rounds (or hallucinate
-- success) on forbidden actions.
agentToolDefinitionsFor :: [Text] -> [Value]
agentToolDefinitionsFor privileges =
    definitionsFrom
        [entry | entry@(_, _, _, priv) <- toolCatalog, maybe True (`elem` privileges) priv]

definitionsFrom :: [(Text, Text, [(Text, Value)], Maybe Text)] -> [Value]
definitionsFrom catalog =
    [ object
        [ "type" .= ("function" :: Text)
        , "function"
            .= object
                [ "name" .= name
                , "description" .= description
                , "parameters"
                    .= object
                        ( [ "type" .= ("object" :: Text)
                          , "properties" .= object [(Key.fromText propName, propSchema) | (propName, propSchema) <- props]
                          ]
                            ++ ["required" .= [propName | (propName, schema) <- props, propRequired schema] | any (propRequired . snd) props]
                        )
                ]
        ]
    | (name, description, props, _) <- catalog
    ]
  where
    propRequired schema = fromMaybe False (parseMaybe (Aeson.withObject "prop" (\o -> o .:? "x-required" .!= False)) schema)

executeAgentTool :: (?modelContext :: ModelContext) => AgentContext -> ToolCall -> IO Text
executeAgentTool context call = do
    let entry = case [meta | meta@(n, _, _, _) <- toolCatalog, n == call.callName] of
            (meta : _) -> Just meta
            [] -> Nothing
    case entry of
        Nothing -> pure ("unknown tool: " <> call.callName)
        Just (_, _, properties, requiredPrivilege) -> do
            -- Hard RBAC gate: the acting user's real privileges decide,
            -- never the model. Enforced before dispatch.
            privileges <- userPrivileges (get #id context.acUser)
            case requiredPrivilege of
                Just privilege
                    | privilege `notElem` privileges ->
                        pure ("forbidden: the acting user lacks the " <> privilege <> " privilege")
                _ -> case decodeArguments call.callArguments of
                    Just arguments -> do
                        result <- try (dispatch context call.callName arguments)
                        pure case result of
                            Right output -> output
                            Left (err :: SomeException) -> invalidArgumentsMessage properties (tshow err)
                    Nothing -> pure (invalidArgumentsMessage properties "arguments are not a JSON object")
  where
    -- Providers emit arguments: "" (empty string) for zero-arg tools —
    -- normalize blank to an empty object instead of failing. When arguments
    -- really are unusable, name the REQUIRED parameters so the model can
    -- self-correct instead of thrashing the same call.
    decodeArguments raw
        | Text.null (Text.strip raw) = Just mempty
        | otherwise = case Aeson.decode (cs raw) of
            Just (Object arguments) -> Just arguments
            _ -> Nothing
    invalidArgumentsMessage properties detail =
        "invalid arguments for "
            <> call.callName
            <> ": "
            <> firstLine detail
            <> ( if null requiredProps
                    then ""
                    else " (required: " <> Text.intercalate ", " requiredProps <> ")"
               )
      where
        -- Handler errors raised via `error` embed a HasCallStack suffix —
        -- never forward it to the model (it gets re-injected on every
        -- repetition-guard retry, pure token noise).
        firstLine = Text.takeWhile (/= '\n')
        requiredProps =
            [ propName
            | (propName, schema) <- properties
            , fromMaybe False (parseMaybe (Aeson.withObject "prop" (\o -> o .:? "x-required" .!= False)) schema)
            ]

-- Handlers: alert actions
dispatch :: (?modelContext :: ModelContext) => AgentContext -> Text -> Aeson.Object -> IO Text
dispatch context name arguments = case name of
    "search_alerts" -> do
        env <- arg "env" ""
        host <- arg "host" ""
        service <- arg "service" ""
        severity <- arg "severity" ""
        status <- arg "status" ""
        title <- arg "title" ""
        limitRaw <- argInt "limit" 20
        let limit = max 1 (min 100 limitRaw)
        cursorArg <- argMaybe "cursor"
        cursor <- case cursorArg of
            Nothing -> pure Nothing
            Just raw -> maybe (error "invalid cursor: pass the next_cursor value verbatim from the previous search_alerts output") (pure . Just) (decodeCursor raw)
        scope <- scopeForUser context.acUser
        page <-
            listAlertsPage
                defaultFilters
                    { afEnvironment = env
                    , afHost = host
                    , afService = service
                    , afSeverity = severity
                    , afStatus = status
                    , afTitle = title
                    , afCursor = cursor
                    , afLimit = limit
                    }
                scope
        pure case page.apAlerts of
            [] -> "no matching alerts"
            _ ->
                Text.intercalate
                    "\n"
                    ( ("total: " <> tshow page.apTotal <> " matching alert(s); showing " <> tshow (length page.apAlerts))
                        : map alertLine page.apAlerts
                        ++ ["next_cursor: " <> nextCursor | Just nextCursor <- [page.apNextCursor]]
                    )
    "get_alert" -> do
        alert <- fetchVisibleAlert context.acUser =<< arg "alert_id" ""
        getAlertDetails alert
    "list_environments" -> listEnvironments =<< scopeForUser context.acUser
    "ack_alert" -> do
        alert <- fetchVisibleAlert context.acUser =<< arg "alert_id" ""
        comment <- argMaybe "comment"
        _ <- ackAlert context.acUser alert comment Nothing
        pure ("acknowledged alert " <> alert.title)
    "unack_alert" -> do
        alert <- fetchVisibleAlert context.acUser =<< arg "alert_id" ""
        _ <- unackAlert (Just context.acUser) alert "agent unack"
        pure ("unacknowledged alert " <> alert.title)
    "close_alert" -> do
        alert <- fetchVisibleAlert context.acUser =<< arg "alert_id" ""
        reason <- argMaybe "reason"
        _ <- closeAlert (Just context.acUser) alert reason
        pure ("closed alert " <> alert.title)
    "ack_alerts" -> do
        alertIds <- bulkAlertIds
        comment <- argMaybe "comment"
        confirmed <- argBool "confirmed" False
        alerts <- mapM (fetchVisibleAlert context.acUser) alertIds
        bulkPlanOrApply confirmed (bulkPlan "acknowledge" "" alerts comment) (applyBulk "acknowledged" "ack" (\alert -> ackAlert context.acUser alert comment Nothing) alerts)
    "close_alerts" -> do
        alertIds <- bulkAlertIds
        reason <- argMaybe "reason"
        confirmed <- argBool "confirmed" False
        alerts <- mapM (fetchVisibleAlert context.acUser) alertIds
        bulkPlanOrApply confirmed (bulkPlan "close" "" alerts reason) (applyBulk "closed" "closed" (\alert -> closeAlert (Just context.acUser) alert reason) alerts)
    "ack_by_match" -> do
        matchText <- argJsonText "match"
        comment <- argMaybe "comment"
        limitRaw <- argInt "limit" 50
        confirmed <- argBool "confirmed" False
        case parseMatchClauses matchText of
            Left err -> pure ("invalid match: " <> err)
            Right clauses -> do
                scope <- scopeForUser context.acUser
                alerts <- alertsMatchingClauses scope clauses (bulkLimit limitRaw)
                bulkPlanOrApply confirmed (bulkPlan "acknowledge" (" matching " <> clauseSummary clauses) alerts comment) (applyBulk "acknowledged" "ack" (\alert -> ackAlert context.acUser alert comment Nothing) alerts)
    "close_by_match" -> do
        matchText <- argJsonText "match"
        reason <- argMaybe "reason"
        limitRaw <- argInt "limit" 50
        confirmed <- argBool "confirmed" False
        case parseMatchClauses matchText of
            Left err -> pure ("invalid match: " <> err)
            Right clauses -> do
                scope <- scopeForUser context.acUser
                alerts <- alertsMatchingClauses scope clauses (bulkLimit limitRaw)
                bulkPlanOrApply confirmed (bulkPlan "close" (" matching " <> clauseSummary clauses) alerts reason) (applyBulk "closed" "closed" (\alert -> closeAlert (Just context.acUser) alert reason) alerts)
    "comment_alert" -> do
        alert <- fetchVisibleAlert context.acUser =<< arg "alert_id" ""
        body <- arg "body" ""
        _ <- addComment context.acUser alert body
        pure ("comment added to alert " <> alert.title)
    "list_blackouts" -> listBlackouts
    "create_blackout" -> do
        envName <- argMaybe "env"
        hostName <- argMaybe "host"
        serviceName <- argMaybe "service"
        titleName <- argMaybe "title"
        startsRaw <- arg "starts_at" ""
        endsProvided <- argMaybe "ends_at"
        reason <- arg "reason" ""
        confirmed <- argBool "confirmed" False
        let openEnded = maybe True (Text.null . Text.strip) endsProvided
            mEndsAt
                | openEnded = Just openEndedBlackoutEndsAt
                | otherwise = iso8601ParseM (cs (fromMaybe "" endsProvided)) :: Maybe UTCTime
        case (iso8601ParseM (cs startsRaw) :: Maybe UTCTime, mEndsAt) of
            (Just startsAt, Just endsAt)
                | endsAt > startsAt -> do
                    let envGlob = globName =<< envName
                        hostGlob = globName =<< hostName
                        serviceGlob = globName =<< serviceName
                        -- Titles have no inventory row: any provided title is
                        -- a glob (plain text matches exactly).
                        titleGlob = titleName
                        anyProvided = any isJust [envName, hostName, serviceName, titleName]
                    knownEnv <- knownInventoryName (\n -> query @Environment |> filterWhere (#name, n) |> fetchOneOrNothing) envName
                    knownHost <- knownInventoryName (\n -> query @Host |> filterWhere (#fqdn, n) |> fetchOneOrNothing) hostName
                    knownService <- knownInventoryName (\n -> query @Service |> filterWhere (#name, n) |> fetchOneOrNothing) serviceName
                    if
                        | not anyProvided -> pure "invalid scope: at least one of env/host/service/title is required"
                        | not (knownEnv && knownHost && knownService) -> pure "invalid scope: unknown env/host/service name"
                        | otherwise -> do
                            envId <- if isJust envGlob then pure Nothing else resolveEnvironmentId (fromMaybe "" envName)
                            hostId <- if isJust hostGlob then pure Nothing else resolveHostId (fromMaybe "" hostName)
                            serviceId <- if isJust serviceGlob then pure Nothing else resolveServiceId (fromMaybe "" serviceName)
                            let scope = blackoutScopeText envName hostName serviceName envGlob hostGlob serviceGlob titleGlob
                                windowText =
                                    if openEnded
                                        then " from " <> tshow startsAt <> " forever (open-ended)"
                                        else " from " <> tshow startsAt <> " to " <> tshow endsAt
                                plan =
                                    "plan: blackout "
                                        <> scope
                                        <> windowText
                                        <> (if Text.null reason then "" else " (" <> reason <> ")")
                            if not confirmed
                                then pure (plan <> "\nconfirmation required: show this plan to the user; call create_blackout again with confirmed=true only after explicit agreement")
                                else do
                                    _ <-
                                        newRecord @Blackout
                                            |> set #environmentId envId
                                            |> set #hostId hostId
                                            |> set #serviceId serviceId
                                            |> set #environmentGlob envGlob
                                            |> set #hostGlob hostGlob
                                            |> set #serviceGlob serviceGlob
                                            |> set #titleGlob titleGlob
                                            |> set #startsAt startsAt
                                            |> set #endsAt endsAt
                                            |> set #reason reason
                                            |> set #createdBy (Just (get #id context.acUser))
                                            |> createRecord
                                    pure ("created blackout " <> scope)
            _ -> pure "invalid time: starts_at/ends_at must be ISO8601 and ends_at must be after starts_at"
    "delete_blackout" -> do
        blackoutId <- arg "blackout_id" ""
        confirmed <- argBool "confirmed" False
        blackout <- fetchBlackout blackoutId
        scope <- blackoutScopeSummary blackout
        if not confirmed
            then pure ("plan: delete blackout " <> tshow (get #id blackout) <> " (" <> scope <> ")\nconfirmation required: call delete_blackout again with confirmed=true only after the user's explicit agreement")
            else do
                deleteRecord blackout
                pure "blackout deleted"
    "list_dashboards" -> listDashboards context
    "get_dashboard" -> do
        name <- argMaybe "name"
        idArg <- argMaybe "id"
        dashboard <- fetchOwnDashboard context name idArg
        pure
            ( Text.intercalate
                "\n"
                [ "name: " <> dashboard.name
                , "id: " <> tshow (get #id dashboard)
                , "is_default: " <> (if dashboard.isDefault then "true" else "false")
                , "config:"
                , prettyJson dashboard.config
                ]
            )
    "get_dashboard_schema" -> pure dashboardSchemaDoc
    "request_confirmation" -> do
        summary <- arg "summary" ""
        if Text.null summary
            then pure "invalid arguments for request_confirmation (required: summary)"
            else pure ("plan: " <> summary <> "\nconfirmation required: present this plan in the chat; proceed with the change only after the user's explicit agreement (Apply), and do not apply if they Discard")
    "validate_dashboard" -> do
        name <- arg "name" ""
        configText <- argJsonText "config"
        validateDashboard name configText
    "create_dashboard" -> do
        name <- arg "name" ""
        configText <- argJsonText "config"
        confirmed <- argBool "confirmed" False
        createDashboardFor context name configText confirmed
    "update_dashboard" -> do
        name <- arg "name" ""
        configText <- argJsonText "config"
        confirmed <- argBool "confirmed" False
        dashboard <- fetchOwnDashboardByName context name
        plan <- validateDashboard name configText
        if not (Text.isPrefixOf "plan:" plan)
            then pure plan
            else
                if not confirmed
                    then pure (plan <> "\nconfirmation required: show this plan to the user; call update_dashboard again with confirmed=true only after explicit agreement")
                    else case decodeConfigText configText of
                        Left err -> pure ("invalid config: " <> err)
                        Right cards -> do
                            _ <- dashboard |> set #config (encodeDashboardConfig cards) |> updateRecord
                            pure ("updated dashboard \"" <> name <> "\"")
    "delete_dashboard" -> do
        name <- arg "name" ""
        confirmed <- argBool "confirmed" False
        dashboard <- fetchOwnDashboardByName context name
        let cardCount = case decodeDashboardConfig dashboard.config of
                Right cards -> length cards
                Left _ -> 0
        if not confirmed
            then pure ("plan: delete dashboard \"" <> name <> "\" (" <> tshow cardCount <> " card(s))\nconfirmation required: call delete_dashboard again with confirmed=true only after the user's explicit agreement")
            else do
                deleteRecord dashboard
                pure ("deleted dashboard \"" <> name <> "\"")
    "set_default_dashboard" -> do
        name <- arg "name" ""
        dashboard <- fetchOwnDashboardByName context name
        defaults <-
            query @Dashboard
                |> filterWhere (#userId, get #id context.acUser)
                |> filterWhere (#isDefault, True)
                |> fetch
        forM_ defaults \other -> void (other |> set #isDefault False |> updateRecord)
        _ <- dashboard |> set #isDefault True |> updateRecord
        pure ("default dashboard set to \"" <> name <> "\"")
    other -> dispatchRest context other arguments
  where
    arg :: Text -> Text -> IO Text
    arg key fallback = do
        value <- argMaybe key
        pure (fromMaybe fallback value)
    argMaybe :: Text -> IO (Maybe Text)
    argMaybe key = case KeyMap.lookup (Key.fromText key) arguments of
        Just (String value) -> pure (Just value)
        _ -> pure Nothing
    argInt :: Text -> Int -> IO Int
    argInt key fallback = case KeyMap.lookup (Key.fromText key) arguments of
        Just (Number value) -> pure (fromMaybe fallback (previewInt value))
        -- Models often quote numbers: accept a numeric string too.
        Just (String value) -> pure (fromMaybe fallback (readMaybe (cs value)))
        _ -> pure fallback
      where
        previewInt value = case floatingOrInteger value of
            Right int -> Just int
            Left _ -> Nothing
    argBool :: Text -> Bool -> IO Bool
    argBool key fallback = case KeyMap.lookup (Key.fromText key) arguments of
        Just (Bool value) -> pure value
        _ -> pure fallback
    -- Like arg, but models often pass structured JSON where a string is
    -- documented: accept any JSON value and re-encode it (a String passes
    -- through verbatim).
    argJsonText :: Text -> IO Text
    argJsonText key = case KeyMap.lookup (Key.fromText key) arguments of
        Just (String value) -> pure value
        Just other -> pure (cs (Aeson.encode other))
        Nothing -> pure ""
    bulkAlertIds :: IO [Text]
    bulkAlertIds = do
        raw <- arg "alert_ids" ""
        case Aeson.decode (cs raw) of
            Just (Array items)
                | let ids = [value | String value <- Vector.toList items]
                , length ids == Vector.length items
                , not (null ids) ->
                    pure ids
            _ -> error "alert_ids must be a non-empty JSON array of alert UUID strings, e.g. [\"id1\",\"id2\"]"

-- Helpers: alerts
alertLine alert =
    "- "
        <> "["
        <> alert.severity
        <> "] ["
        <> alert.status
        <> "] "
        <> alert.title
        <> " (env="
        <> fromMaybe "" alert.env
        <> ", host="
        <> fromMaybe "" alert.host
        <> ", fp="
        <> alert.fingerprint
        <> ", id="
        <> tshow (get #id alert)
        <> ")"

fetchAlert :: (?modelContext :: ModelContext) => Text -> IO Alert
fetchAlert rawId = do
    uuid <- maybe (error "alert_id is not a UUID") pure (readMaybe (cs rawId))
    result <- try (fetch (Id uuid :: Id Alert))
    case result of
        Right alert -> pure alert
        Left (_ :: SomeException) -> error "unknown alert id"

-- | fetchAlert plus the per-user host group visibility (AlertScope): the
-- agent sees exactly what its user sees — an out-of-scope alert id is
-- indistinguishable from a missing one.
fetchVisibleAlert :: (?modelContext :: ModelContext) => User -> Text -> IO Alert
fetchVisibleAlert user rawId = do
    alert <- fetchAlert rawId
    scope <- scopeForUser user
    case scope of
        Nothing -> pure alert
        Just names -> do
            visible <- alertVisibleIO names alert
            if visible
                then pure alert
                else error "unknown alert id (not visible to this user)"

alertVisibleIO :: (?modelContext :: ModelContext) => [Text] -> Alert -> IO Bool
alertVisibleIO names alert = do
    isZabbix <- case alert.sourceId of
        Nothing -> pure False
        Just sourceId -> maybe False (\source -> source.type_ == "zabbix") <$> fetchOneOrNothing sourceId
    pure (alertVisibleWith names isZabbix alert)

-- | Full single-alert record for get_alert: status, effective field values,
-- muted flag, timestamps, ack/close details, group, comments and escalation
-- trackers — everything needed to answer "why isn't that alert acting the
-- way I expect" without opening the web UI.
getAlertDetails :: (?modelContext :: ModelContext) => Alert -> IO Text
getAlertDetails alert = do
    ackUser <- traverse userLabel alert.acknowledgedBy
    closedUser <- traverse userLabel alert.closedBy
    group <- traverse fetch alert.groupId
    source <- traverse fetch alert.sourceId
    comments <- query @Comment |> filterWhere (#alertId, get #id alert) |> orderByAsc #createdAt |> fetch
    commentLines <- forM comments \comment -> do
        author <- userLabel comment.userId
        pure ("- [" <> tshow comment.createdAt <> "] " <> author <> ": " <> comment.body)
    trackers <- query @EscalationTracker |> filterWhere (#alertId, get #id alert) |> fetch
    trackerLines <- forM trackers \tracker -> do
        policyName <- maybe "-" (.name) <$> fetchOneOrNothing tracker.policyId
        pure
            ( "- policy \""
                <> policyName
                <> "\" step "
                <> tshow tracker.currentStep
                <> " ("
                <> tracker.status
                <> ")"
                <> maybe "" (", next: " <>) (tshow <$> tracker.nextDeadline)
            )
    let eff field = fromMaybe "-" (effectiveFieldText field alert)
        mutedText =
            if alert.suppressed
                then "true" <> maybe "" (\by -> " (by " <> by <> ")") alert.suppressedBy
                else "false"
        maybeLine label = maybe [] (\value -> [label <> ": " <> value])
        coreLines =
            [ "id: " <> tshow (get #id alert)
            , "title: " <> alert.title
            , "status: " <> alert.status
            , "severity: " <> alert.severity
            , "env: " <> eff FieldEnv
            , "host: " <> eff FieldHost
            , "service: " <> eff FieldService
            , "check: " <> fromMaybe "-" alert.checkName
            , "muted: " <> mutedText
            , "fingerprint: " <> alert.fingerprint
            , "occurrences: " <> tshow alert.occurrences
            , "first_seen_at: " <> tshow alert.firstSeenAt
            , "last_seen_at: " <> tshow alert.lastSeenAt
            ]
                ++ maybeLine "description: " (alertDescription alert)
                ++ ["started_at: " <> tshow t | Just t <- [alert.startedAt]]
                ++ ["acknowledged_at: " <> tshow t <> " by " <> by | let by = fromMaybe "-" ackUser, Just t <- [alert.acknowledgedAt]]
                ++ ["ack_comment: " <> c | Just c <- [alert.ackComment]]
                ++ ["resolved_at: " <> tshow t | Just t <- [alert.resolvedAt]]
                ++ ["closed_at: " <> tshow t <> " by " <> by | let by = fromMaybe "-" closedUser, Just t <- [alert.closedAt]]
                ++ ["close_reason: " <> r | Just r <- [alert.closeReason]]
                ++ ["group: " <> g.groupKey | Just g <- [group]]
                ++ ["source: " <> s.name <> " (" <> s.type_ <> ")" | Just s <- [source]]
    pure case commentLines ++ trackerLines of
        [] -> Text.intercalate "\n" coreLines
        _ -> Text.intercalate "\n" (coreLines ++ (if null commentLines then [] else "comments:" : commentLines) ++ (if null trackerLines then [] else "escalation:" : trackerLines))
  where
    userLabel uid = maybe "-" (.email) <$> fetchOneOrNothing uid
    alertDescription value = if Text.null value.description then Nothing else Just (Text.take 300 value.description)

-- Bulk ack/close: shared plan/apply machinery for the by-ids and by-match
-- variants. Plans are two-phase confirmed like every other mutating tool;
-- apply reports per-id results and never lies about skipped rows.

bulkLimit :: Int -> Int
bulkLimit raw = max 1 (min 200 raw)

bulkPlan :: Text -> Text -> [Alert] -> Maybe Text -> Text
bulkPlan verb suffix alerts note =
    "plan: "
        <> verb
        <> " "
        <> tshow (length alerts)
        <> " alert(s)"
        <> suffix
        <> ": "
        <> Text.intercalate "; " (take 5 (map (.title) alerts))
        <> (if length alerts > 5 then "; ..." else "")
        <> maybe "" (\value -> " (" <> value <> ")") note

bulkPlanOrApply :: Bool -> Text -> IO Text -> IO Text
bulkPlanOrApply confirmed plan apply =
    if not confirmed
        then pure (plan <> "\nconfirmation required: show this plan to the user; call the tool again with confirmed=true only after explicit agreement")
        else apply

applyBulk :: (?modelContext :: ModelContext) => Text -> Text -> (Alert -> IO Alert) -> [Alert] -> IO Text
applyBulk verb targetStatus action alerts = do
    lines' <- forM alerts \alert ->
        if alert.status == targetStatus
            then pure (bulkLine alert ("skipped: already " <> targetStatus))
            else do
                updated <- action alert
                if updated.status == targetStatus
                    then pure (bulkLine alert "")
                    else pure (bulkLine alert ("skipped: status is " <> updated.status))
    let doneCount = length [() | line <- lines', not ("skipped" `Text.isInfixOf` line)]
    pure (verb <> " " <> tshow doneCount <> " of " <> tshow (length alerts) <> " alert(s)\n" <> Text.intercalate "\n" lines')
  where
    bulkLine alert note = "- " <> tshow (get #id alert) <> ": " <> alert.title <> (if Text.null note then "" else " (" <> note <> ")")

-- | by-match parsing: the dashboard match vocabulary, in two accepted
-- shapes — a full clause array (the "match" member of a dashboard card) or a
-- shorthand object mapping facet references (or plain field names) to glob
-- patterns, e.g. {"host": "web-*", "attr:DC": "dc1"}.
parseMatchClauses :: Text -> Either Text [MatchClause]
parseMatchClauses raw
    | Text.null (Text.strip raw) = Left "match is required, e.g. {\"host\": \"web-*\"}"
    | otherwise = case Aeson.decode (cs raw) of
        Just (Array items) -> parseClauseArray items
        Just (Object object_) -> case KeyMap.lookup "match" object_ of
            Just (Array items) -> parseClauseArray items
            _ -> parseShorthand (KeyMap.toList object_)
        Just _ -> Left "match must be a JSON object of facet->glob or an array of match clauses"
        Nothing -> Left "match is not valid JSON"
  where
    parseClauseArray items = mapM parseClause (Vector.toList items)
    parseClause item = case parseEither Aeson.parseJSON item of
        Left err -> Left (cs err)
        Right clause -> Right clause
    parseShorthand pairs = forM pairs \(key, value) -> case value of
        String glob -> do
            let keyText = Key.toText key
            facet <- maybe (Left ("unknown facet: " <> keyText)) Right (parseFacetRef keyText <|> (FacetField <$> parseAlertField keyText))
            Right (MatchClause facet OpGlob glob [])
        _ -> Left "match values must be strings (glob patterns)"

-- | Open (non-closed) alerts matching every clause, filtered to what the
-- acting user can see, capped at limit. Reuses the dashboard card query so
-- by-match semantics are identical to card matching.
alertsMatchingClauses :: (?modelContext :: ModelContext) => Maybe [Text] -> [MatchClause] -> Int -> IO [Alert]
alertsMatchingClauses scopeNames clauses limit = do
    let card = DashboardCard Nothing clauses Nothing 100 False Nothing Nothing False [] [] Nothing mempty
    alerts <- cardBaseQuery card |> fetch
    visible <- case scopeNames of
        Nothing -> pure alerts
        Just names -> filterM (alertVisibleIO names) alerts
    pure (take limit visible)

clauseSummary :: [MatchClause] -> Text
clauseSummary clauses = "[" <> Text.intercalate ", " (map clauseText clauses) <> "]"

-- Helpers: environments. Open-alert counts respect the per-user host group
-- visibility (AlertScope) so the agent's summary matches what its user sees.
listEnvironments :: (?modelContext :: ModelContext) => Maybe [Text] -> IO Text
listEnvironments scopeNames = do
    let unrestricted = isNothing scopeNames
        scope = fromMaybe [] scopeNames
    rows <-
        sqlQueryTyped
            [typedSql|
        SELECT e.name, COUNT(a.id) FILTER (WHERE ${unrestricted} OR a.fingerprint LIKE 'halemans:%' OR sc.type IS DISTINCT FROM 'zabbix' OR a.host_groups ?| ${scope})::bigint AS n
        FROM environments e
        LEFT JOIN alerts a ON a.environment_id = e.id AND a.status <> 'closed'
        LEFT JOIN sources sc ON sc.id = a.source_id
        GROUP BY e.name
        ORDER BY e.name
    |]
    pure case rows of
        [] -> "no environments defined"
        _ -> Text.intercalate "\n" ["- " <> row.name <> ": " <> tshow (row.n :: Int64) <> " open alerts" | row <- rows]

-- Helpers: blackouts
listBlackouts :: (?modelContext :: ModelContext) => IO Text
listBlackouts = do
    blackouts <- query @Blackout |> orderByDesc #startsAt |> fetch
    lines' <- forM (take 50 blackouts) \blackout -> do
        scope <- blackoutScopeSummary blackout
        pure
            ( tshow (get #id blackout)
                <> ": "
                <> scope
                <> " "
                <> tshow blackout.startsAt
                <> " to "
                <> blackoutEndsLabel blackout
                <> (if Text.null blackout.reason then "" else " — " <> blackout.reason)
            )
    pure case lines' of
        [] -> "no blackouts"
        _ -> Text.intercalate "\n" lines'

blackoutScopeSummary :: (?modelContext :: ModelContext) => Blackout -> IO Text
blackoutScopeSummary blackout = do
    envName <- traverse (fmap (.name) . fetch) blackout.environmentId
    hostName <- traverse (fmap (.fqdn) . fetch) blackout.hostId
    serviceName <- traverse (fmap (.name) . fetch) blackout.serviceId
    pure (blackoutScopeText envName hostName serviceName blackout.environmentGlob blackout.hostGlob blackout.serviceGlob blackout.titleGlob)

blackoutScopeText :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text
blackoutScopeText envName hostName serviceName envGlob hostGlob serviceGlob titleGlob =
    case catMaybes
        [ ("env: " <>) <$> envName
        , ("host: " <>) <$> hostName
        , ("service: " <>) <$> serviceName
        , ("env glob: " <>) <$> envGlob
        , ("host glob: " <>) <$> hostGlob
        , ("service glob: " <>) <$> serviceGlob
        , ("title glob: " <>) <$> titleGlob
        ] of
        [] -> "global"
        parts -> Text.intercalate ", " parts

-- | A name containing glob wildcards is a scope pattern, not an inventory
-- lookup.
globName :: Text -> Maybe Text
globName name = if Text.any (\c -> c == '*' || c == '?') name then Just name else Nothing

-- | Exact-name existence check; glob patterns skip it (they may legitimately
-- match zero current inventory rows).
knownInventoryName :: (?modelContext :: ModelContext) => (Text -> IO (Maybe record)) -> Maybe Text -> IO Bool
knownInventoryName _ Nothing = pure True
knownInventoryName lookupName (Just name)
    | isJust (globName name) = pure True
    | otherwise = isJust <$> lookupName name

fetchBlackout :: (?modelContext :: ModelContext) => Text -> IO Blackout
fetchBlackout rawId = do
    uuid <- maybe (error "blackout_id is not a UUID") pure (readMaybe (cs rawId))
    result <- try (fetch (Id uuid :: Id Blackout))
    case result of
        Right blackout -> pure blackout
        Left (_ :: SomeException) -> error "unknown blackout id"

resolveEnvironmentId :: (?modelContext :: ModelContext) => Text -> IO (Maybe (Id Environment))
resolveEnvironmentId name = fmap (get #id) <$> (query @Environment |> filterWhere (#name, name) |> fetchOneOrNothing)

resolveHostId :: (?modelContext :: ModelContext) => Text -> IO (Maybe (Id Host))
resolveHostId name = fmap (get #id) <$> (query @Host |> filterWhere (#fqdn, name) |> fetchOneOrNothing)

resolveServiceId :: (?modelContext :: ModelContext) => Text -> IO (Maybe (Id Service))
resolveServiceId name = fmap (get #id) <$> (query @Service |> filterWhere (#name, name) |> fetchOneOrNothing)

-- Handlers: P2/P3 dispatch (teams, rules, sources, profile, tokens, reads)
dispatchRest :: (?modelContext :: ModelContext) => AgentContext -> Text -> Aeson.Object -> IO Text
dispatchRest context name arguments = case name of
    "list_teams" -> listTeams
    "create_team" -> do
        name <- arg "name" ""
        description <- arg "description" ""
        hostGroupsText <- arg "host_groups" ""
        mmTeam <- argMaybe "mattermost_team"
        mmChannel <- argMaybe "mattermost_channel"
        confirmed <- argBool "confirmed" False
        existing <- query @Team |> filterWhere (#name, name) |> fetchOneOrNothing
        case existing of
            Just _ -> pure ("invalid: a team named \"" <> name <> "\" already exists")
            Nothing ->
                if not confirmed
                    then pure ("plan: create team \"" <> name <> "\"" <> (if Text.null description then "" else " (" <> description <> ")") <> teamPlanExtras hostGroupsText mmTeam mmChannel <> "\nconfirmation required: call create_team again with confirmed=true only after explicit agreement")
                    else do
                        _ <-
                            newRecord @Team
                                |> set #name name
                                |> set #description description
                                |> set #hostGroups (hostGroupsToJson (splitCsv hostGroupsText))
                                |> set #defaults (applyMattermostArgs mmTeam mmChannel (Aeson.object []))
                                |> createRecord
                        pure ("created team \"" <> name <> "\"")
    "update_team" -> do
        name <- arg "name" ""
        confirmed <- argBool "confirmed" False
        team <- fetchTeamByName name
        if get #protected team
            then pure "forbidden: this team is provisioned-protected"
            else do
                description <- argMaybe "description"
                hostGroupsText <- argMaybe "host_groups"
                mmTeam <- argMaybe "mattermost_team"
                mmChannel <- argMaybe "mattermost_channel"
                let withDescription = maybe team (\value -> team |> set #description value) description
                    withHostGroups = maybe withDescription (\value -> withDescription |> set #hostGroups (hostGroupsToJson (splitCsv value))) hostGroupsText
                    withDefaults = withHostGroups |> set #defaults (applyMattermostArgs mmTeam mmChannel (get #defaults withHostGroups))
                if not confirmed
                    then pure ("plan: update team \"" <> name <> "\"" <> teamPlanExtras (fromMaybe "" hostGroupsText) mmTeam mmChannel <> "\nconfirmation required: call update_team again with confirmed=true only after the user's explicit agreement")
                    else do
                        _ <- updateRecord withDefaults
                        pure ("updated team \"" <> name <> "\"")
    "delete_team" -> do
        name <- arg "name" ""
        confirmed <- argBool "confirmed" False
        team <- fetchTeamByName name
        if get #protected team
            then pure "forbidden: this team is provisioned-protected"
            else
                if not confirmed
                    then pure ("plan: delete team \"" <> name <> "\" and its memberships\nconfirmation required: call delete_team again with confirmed=true only after the user's explicit agreement")
                    else do
                        members <- query @TeamMember |> filterWhere (#teamId, get #id team) |> fetch
                        forM_ members deleteRecord
                        deleteRecord team
                        pure ("deleted team \"" <> name <> "\"")
    "add_team_member" -> do
        teamName <- arg "team" ""
        email <- arg "email" ""
        role <- arg "role" "member"
        team <- fetchTeamByName teamName
        user <- fetchUserByEmail email
        existing <-
            query @TeamMember
                |> filterWhere (#teamId, get #id team)
                |> filterWhere (#userId, get #id user)
                |> fetchOneOrNothing
        case existing of
            Just _ -> pure "invalid: user is already a member of this team"
            Nothing -> do
                _ <-
                    newRecord @TeamMember
                        |> set #teamId (get #id team)
                        |> set #userId (get #id user)
                        |> set #teamRole (if role == "lead" then "lead" else "member")
                        |> createRecord
                pure ("added " <> email <> " to team \"" <> teamName <> "\" as " <> (if role == "lead" then "lead" else "member"))
    "remove_team_member" -> do
        teamName <- arg "team" ""
        email <- arg "email" ""
        team <- fetchTeamByName teamName
        user <- fetchUserByEmail email
        membership <-
            query @TeamMember
                |> filterWhere (#teamId, get #id team)
                |> filterWhere (#userId, get #id user)
                |> fetchOneOrNothing
        case membership of
            Nothing -> pure "invalid: user is not a member of this team"
            Just membership -> do
                deleteRecord membership
                pure ("removed " <> email <> " from team \"" <> teamName <> "\"")
    "list_escalation_policies" -> do
        policies <- query @EscalationPolicy |> orderByAsc #name |> fetch
        pure case policies of
            [] -> "no escalation policies"
            _ -> Text.intercalate "\n" ["- " <> p.name <> " (" <> tshow (length (stepsList p.steps)) <> " step(s))" <> protectedMark p | p <- policies]
    "get_escalation_policy" -> do
        name <- arg "name" ""
        policy <- fetchPolicyByName name
        pure (prettyJson policy.steps)
    "create_escalation_policy" -> do
        name <- arg "name" ""
        stepsText <- arg "steps" ""
        confirmed <- argBool "confirmed" False
        stepsValue <- parseSteps stepsText
        case stepsValue of
            Left err -> pure err
            Right stepsValue -> do
                existing <- query @EscalationPolicy |> filterWhere (#name, name) |> fetchOneOrNothing
                case existing of
                    Just _ -> pure ("invalid: an escalation policy named \"" <> name <> "\" already exists")
                    Nothing ->
                        if not confirmed
                            then pure ("plan: create escalation policy \"" <> name <> "\" with " <> tshow (length (stepsList stepsValue)) <> " step(s)\nconfirmation required: call create_escalation_policy again with confirmed=true only after explicit agreement")
                            else do
                                _ <- newRecord @EscalationPolicy |> set #name name |> set #steps stepsValue |> createRecord
                                pure ("created escalation policy \"" <> name <> "\"")
    "update_escalation_policy" -> do
        name <- arg "name" ""
        stepsText <- arg "steps" ""
        confirmed <- argBool "confirmed" False
        policy <- fetchPolicyByName name
        if get #protected policy
            then pure "forbidden: this escalation policy is provisioned-protected"
            else do
                stepsValue <- parseSteps stepsText
                case stepsValue of
                    Left err -> pure err
                    Right stepsValue ->
                        if not confirmed
                            then pure ("plan: replace steps of escalation policy \"" <> name <> "\" with " <> tshow (length (stepsList stepsValue)) <> " step(s)\nconfirmation required: call update_escalation_policy again with confirmed=true only after explicit agreement")
                            else do
                                _ <- policy |> set #steps stepsValue |> updateRecord
                                pure ("updated escalation policy \"" <> name <> "\"")
    "delete_escalation_policy" -> do
        name <- arg "name" ""
        confirmed <- argBool "confirmed" False
        policy <- fetchPolicyByName name
        if get #protected policy
            then pure "forbidden: this escalation policy is provisioned-protected"
            else
                if not confirmed
                    then pure ("plan: delete escalation policy \"" <> name <> "\"\nconfirmation required: call delete_escalation_policy again with confirmed=true only after the user's explicit agreement")
                    else do
                        deleteRecord policy
                        pure ("deleted escalation policy \"" <> name <> "\"")
    "list_notification_rules" -> listNotificationRules
    "create_notification_rule" -> do
        name <- arg "name" ""
        matchText <- arg "match" ""
        severityThreshold <- arg "severity_threshold" "info"
        teamName <- argMaybe "team"
        channel <- arg "channel" "browser_push"
        channelConfigText <- arg "channel_config" ""
        confirmed <- argBool "confirmed" False
        matchValue <- parseJsonObject matchText
        channelConfig <-
            if Text.null (Text.strip channelConfigText)
                then pure (Right (Aeson.object []))
                else parseJsonObject channelConfigText
        channelRow <- query @NotificationChannel |> filterWhere (#name, channel) |> fetchOneOrNothing
        case (matchValue, channelConfig, isJust channelRow) of
            (Left err, _, _) -> pure err
            (_, Left err, _) -> pure err
            (_, _, False) ->
                pure ("invalid: unknown notification channel \"" <> channel <> "\" (see list_notification_channels or create it first)")
            (Right matchValue, Right channelConfig, True) -> do
                teamId <- traverse (fmap (get #id) . fetchTeamByName) teamName
                existing <- query @NotificationRule |> filterWhere (#name, name) |> fetchOneOrNothing
                case existing of
                    Just _ -> pure ("invalid: a notification rule named \"" <> name <> "\" already exists")
                    Nothing ->
                        if not confirmed
                            then pure ("plan: create notification rule \"" <> name <> "\" (severity >= " <> severityThreshold <> ", team=" <> fromMaybe "-" teamName <> ", channel=" <> channel <> ")\nconfirmation required: call create_notification_rule again with confirmed=true only after explicit agreement")
                            else do
                                _ <-
                                    newRecord @NotificationRule
                                        |> set #name name
                                        |> set #match matchValue
                                        |> set #severityThreshold severityThreshold
                                        |> set #teamId teamId
                                        |> set #channel channel
                                        |> set #channelConfig channelConfig
                                        |> createRecord
                                pure ("created notification rule \"" <> name <> "\"")
    "delete_notification_rule" -> do
        name <- arg "name" ""
        confirmed <- argBool "confirmed" False
        rule <- fetchNotificationRuleByName name
        if get #protected rule
            then pure "forbidden: this notification rule is provisioned-protected"
            else
                if not confirmed
                    then pure ("plan: delete notification rule \"" <> name <> "\"\nconfirmation required: call delete_notification_rule again with confirmed=true only after the user's explicit agreement")
                    else do
                        deleteRecord rule
                        pure ("deleted notification rule \"" <> name <> "\"")
    "list_notification_channels" -> do
        channels <- query @NotificationChannel |> orderByAsc #name |> fetch
        pure case channels of
            [] -> "no notification channels"
            _ -> Text.intercalate "\n" (map channelLine channels)
    "create_notification_channel" -> do
        name <- arg "name" ""
        channelType <- arg "type" ""
        baseUrl <- arg "base_url" ""
        tokenEnv <- arg "token_env" ""
        enabledText <- arg "enabled" "true"
        confirmed <- argBool "confirmed" False
        if channelType `notElem` channelTypes
            then pure ("invalid: type must be one of " <> Text.intercalate "|" channelTypes)
            else do
                existing <- query @NotificationChannel |> filterWhere (#name, name) |> fetchOneOrNothing
                case existing of
                    Just _ -> pure ("invalid: a notification channel named \"" <> name <> "\" already exists")
                    Nothing ->
                        if not confirmed
                            then pure ("plan: create notification channel \"" <> name <> "\" (type=" <> channelType <> ", base_url=" <> (if Text.null baseUrl then "-" else baseUrl) <> ", token_env=" <> (if Text.null tokenEnv then "-" else tokenEnv) <> ", enabled=" <> enabledText <> ")\nconfirmation required: call create_notification_channel again with confirmed=true only after explicit agreement")
                            else do
                                _ <-
                                    newRecord @NotificationChannel
                                        |> set #name name
                                        |> set #type_ channelType
                                        |> set #baseUrl baseUrl
                                        |> set #config (tokenEnvConfig tokenEnv (Aeson.object []))
                                        |> set #enabled (enabledText /= "false")
                                        |> createRecord
                                pure ("created notification channel \"" <> name <> "\"")
    "update_notification_channel" -> do
        name <- arg "name" ""
        confirmed <- argBool "confirmed" False
        channel <- fetchNotificationChannelByName name
        if get #protected channel
            then pure "forbidden: this notification channel is provisioned-protected"
            else do
                channelType <- argMaybe "type"
                baseUrl <- argMaybe "base_url"
                tokenEnv <- argMaybe "token_env"
                enabledText <- argMaybe "enabled"
                if maybe False (`notElem` channelTypes) channelType
                    then pure ("invalid: type must be one of " <> Text.intercalate "|" channelTypes)
                    else
                        if not confirmed
                            then pure ("plan: update notification channel \"" <> name <> "\"\nconfirmation required: call update_notification_channel again with confirmed=true only after the user's explicit agreement")
                            else do
                                let withType = maybe channel (\value -> channel |> set #type_ value) channelType
                                    withBaseUrl = maybe withType (\value -> withType |> set #baseUrl value) baseUrl
                                    withTokenEnv = maybe withBaseUrl (\value -> withBaseUrl |> set #config (tokenEnvConfig value (get #config withBaseUrl))) tokenEnv
                                    withEnabled = maybe withTokenEnv (\value -> withTokenEnv |> set #enabled (value /= "false")) enabledText
                                _ <- updateRecord withEnabled
                                pure ("updated notification channel \"" <> name <> "\"")
    "delete_notification_channel" -> do
        name <- arg "name" ""
        confirmed <- argBool "confirmed" False
        channel <- fetchNotificationChannelByName name
        if get #protected channel
            then pure "forbidden: this notification channel is provisioned-protected"
            else do
                referenced <- referencedChannelNames
                if name `elem` referenced
                    then pure ("invalid: notification rules still reference channel \"" <> name <> "\"")
                    else
                        if not confirmed
                            then pure ("plan: delete notification channel \"" <> name <> "\"\nconfirmation required: call delete_notification_channel again with confirmed=true only after the user's explicit agreement")
                            else do
                                deleteRecord channel
                                pure ("deleted notification channel \"" <> name <> "\"")
    "list_grouping_rules" -> do
        rules <- query @GroupingRule |> orderByAsc #position |> fetch
        pure case rules of
            [] -> "no grouping rules"
            _ -> Text.intercalate "\n" ["- " <> r.name <> (if r.enabled then "" else " (disabled)") <> protectedMark r | r <- rules]
    "create_grouping_rule" -> do
        name <- arg "name" ""
        matchText <- arg "match" ""
        groupKeyTemplate <- arg "group_key_template" ""
        position <- argInt "position" 0
        matchValue <- parseJsonObject matchText
        case matchValue of
            Left err -> pure err
            Right matchValue -> do
                existing <- query @GroupingRule |> filterWhere (#name, name) |> fetchOneOrNothing
                case existing of
                    Just _ -> pure ("invalid: a grouping rule named \"" <> name <> "\" already exists")
                    Nothing -> do
                        allRules <- query @GroupingRule |> fetch
                        let maxPosition = maximum (0 : map (.position) allRules)
                        _ <-
                            newRecord @GroupingRule
                                |> set #name name
                                |> set #match matchValue
                                |> set #groupKeyTemplate groupKeyTemplate
                                |> set #position (if position > 0 then position else maxPosition + 1)
                                |> set #createdBy (Just (get #id context.acUser))
                                |> createRecord
                        pure ("created grouping rule \"" <> name <> "\"")
    "delete_grouping_rule" -> do
        name <- arg "name" ""
        rule <- fetchGroupingRuleByName name
        if get #protected rule
            then pure "forbidden: this grouping rule is provisioned-protected"
            else do
                deleteRecord rule
                pure ("deleted grouping rule \"" <> name <> "\"")
    "list_sources" -> do
        sources <- query @Source |> orderByAsc #name |> fetch
        pure case sources of
            [] -> "no sources configured"
            _ -> Text.intercalate "\n" ["- " <> s.name <> " (" <> s.type_ <> ", env=" <> s.env <> ")" <> (if s.enabled then ", enabled" else ", disabled") <> protectedMark s | s <- sources]
    "enable_source" -> setSourceEnabled True =<< arg "name" ""
    "disable_source" -> setSourceEnabled False =<< arg "name" ""
    "get_profile" -> do
        let user = context.acUser
        pure
            ( Text.intercalate
                "\n"
                [ "email: " <> user.email
                , "displayName: " <> user.displayName
                , "settings: " <> cs (Aeson.encode user.settings)
                ]
            )
    "update_profile" -> do
        let user = context.acUser
        theme <- argMaybe "theme"
        timezone <- argMaybe "timezone"
        language <- argMaybe "language"
        let settings = case user.settings of
                Object o -> o
                _ -> mempty
            updated =
                foldr
                    (\(key, value) acc -> KeyMap.insert (Key.fromText key) (String value) acc)
                    settings
                    ([("theme", value) | Just value <- [theme]] ++ [("timezone", value) | Just value <- [timezone]] ++ [("language", value) | Just value <- [language]])
        if null [() | Just _ <- [theme, timezone, language]]
            then pure "nothing to update: pass at least one of theme/timezone/language"
            else do
                _ <- user |> set #settings (Object updated) |> updateRecord
                pure "profile updated"
    "list_api_tokens" -> do
        tokens <- query @ApiToken |> filterWhere (#userId, get #id context.acUser) |> orderByDesc #createdAt |> fetch
        pure case tokens of
            [] -> "no API tokens"
            _ -> Text.intercalate "\n" ["- " <> t.name <> " (prefix=" <> t.prefix <> ", scopes=" <> Text.intercalate "," t.scopes <> ", last used: " <> fromMaybe "never" (tshow <$> t.lastUsedAt) <> ")" | t <- tokens]
    "create_api_token" -> do
        name <- arg "name" ""
        scopesRaw <- arg "scopes" "alerts:read"
        let scopes = map Text.strip (Text.splitOn "," scopesRaw)
        (token, plaintext) <- newApiToken (get #id context.acUser) name scopes Nothing
        pure ("created token \"" <> token.name <> "\" — plaintext (show once, store it now): " <> plaintext)
    "revoke_api_token" -> do
        name <- arg "name" ""
        token <-
            query @ApiToken
                |> filterWhere (#userId, get #id context.acUser)
                |> filterWhere (#name, name)
                |> fetchOneOrNothing
                >>= maybe (error "no API token with that label") pure
        now <- getCurrentTime
        _ <- token |> set #revokedAt (Just now) |> updateRecord
        pure ("revoked token \"" <> name <> "\"")
    "list_users" -> do
        users <- query @User |> orderByAsc #email |> fetch
        pure (Text.intercalate "\n" ["- " <> u.email <> " (" <> u.displayName <> ")" <> (if isJust u.lockedAt then " [locked]" else "") | u <- users])
    "list_roles" -> do
        roles <- query @Role |> orderByAsc #name |> fetch
        pure (Text.intercalate "\n" ["- " <> r.name <> ": " <> Text.intercalate ", " r.privileges | r <- roles])
    "list_llm_templates" -> do
        templates <- query @LlmPromptTemplate |> orderByAsc #name |> orderByDesc #version |> fetch
        pure case templates of
            [] -> "no prompt templates"
            _ -> Text.intercalate "\n" ["- " <> t.name <> " v" <> tshow t.version <> (if t.active then " (active)" else "") | t <- templates]
    "list_mattermost_templates" -> do
        templates <- query @LlmPromptTemplate |> orderByAsc #name |> orderByDesc #version |> fetch
        let mattermost = [t | t <- templates, t.name `elem` mattermostTemplateNames]
        pure case mattermost of
            [] -> "no mattermost templates"
            _ -> Text.intercalate "\n" (map mattermostTemplateLine mattermost)
    "update_mattermost_template" -> do
        name <- arg "name" ""
        body <- arg "body" ""
        notes <- argMaybe "notes"
        confirmed <- argBool "confirmed" False
        if name `notElem` mattermostTemplateNames
            then pure ("invalid: name must be one of " <> Text.intercalate "|" mattermostTemplateNames)
            else do
                versions <- query @LlmPromptTemplate |> filterWhere (#name, name) |> orderByDesc #version |> fetch
                case versions of
                    (latest : _) | get #protected latest -> pure "forbidden: this template is provisioned-protected"
                    _ ->
                        -- An EMPTY body is allowed (and meaningful): an empty
                        -- mattermost_root renders no header line at all.
                        let nextVersion = maybe 1 (\latest -> latest.version + 1) (listToMaybe versions)
                         in if not confirmed
                                then pure ("plan: update mattermost template \"" <> name <> "\" (creates v" <> tshow nextVersion <> " and activates it)\nconfirmation required: call update_mattermost_template again with confirmed=true only after the user's explicit agreement")
                                else do
                                    void do
                                        newRecord @LlmPromptTemplate
                                            |> set #name name
                                            |> set #version nextVersion
                                            |> set #body body
                                            |> set #active False
                                            |> set #notes notes
                                            |> createRecord
                                    withTransaction do
                                        void do
                                            sqlExecTyped
                                                [typedSql|
                                                        UPDATE llm_prompt_templates SET active = false, updated_at = NOW()
                                                        WHERE name = ${name}
                                                    |]
                                        void do
                                            sqlExecTyped
                                                [typedSql|
                                                        UPDATE llm_prompt_templates SET active = true, updated_at = NOW()
                                                        WHERE name = ${name} AND version = ${nextVersion}
                                                    |]
                                    pure ("updated mattermost template \"" <> name <> "\" (v" <> tshow nextVersion <> ", active)")
    "list_llm_providers" -> do
        providers <- query @LlmConfig |> orderByAsc #providerName |> fetch
        pure case providers of
            [] -> "no LLM providers configured"
            _ -> Text.intercalate "\n" ["- " <> p.providerName <> " (" <> p.model <> ", endpoint=" <> p.endpoint <> ")" <> (if p.enabled then ", enabled" else ", disabled") | p <- providers]
    "get_llm_config" -> getLlmConfig
    "list_alert_groups" -> do
        limitRaw <- argInt "limit" 20
        let limit = max 1 (min 100 limitRaw)
        scope <- scopeForUser context.acUser
        groups <- query @AlertGroup |> orderByDesc #createdAt |> fetch
        let limited = take limit groups
            ids = map (get #id) limited
            unrestricted = isNothing scope
            scopeNames = fromMaybe [] scope
        rows <-
            sqlQueryTyped
                [typedSql|
            SELECT g.id, COUNT(a.id) FILTER (WHERE ${unrestricted} OR a.fingerprint LIKE 'halemans:%' OR sc.type IS DISTINCT FROM 'zabbix' OR a.host_groups ?| ${scopeNames})::bigint AS n
            FROM alert_groups g
            LEFT JOIN alerts a ON a.group_id = g.id AND a.status <> 'closed'
            LEFT JOIN sources sc ON sc.id = a.source_id
            WHERE g.id = ANY(${ids})
            GROUP BY g.id
        |]
        let countsById = Map.fromList [(get #id row, get #n row) | row <- rows]
            groupsById = Map.fromList [(get #id g, g) | g <- limited]
            -- The agent sees what its user sees: a group whose open alerts
            -- are all out of scope disappears entirely.
            visibleGroups = [g | g <- limited, Map.lookup (get #id g) countsById /= Just 0]
        pure case visibleGroups of
            [] -> "no alert groups visible to you"
            _ ->
                Text.intercalate
                    "\n"
                    [ "- "
                        <> fromMaybe "-" (fmap (.groupKey) (Map.lookup gid groupsById))
                        <> ": "
                        <> tshow (fromMaybe 0 (Map.lookup gid countsById))
                        <> " open alert(s) (id="
                        <> tshow gid
                        <> ")"
                    | g <- visibleGroups
                    , let gid = get #id g
                    ]
    "explain_last_turn" -> explainLastTurn context
    other -> pure ("unknown tool: " <> other)
  where
    arg :: Text -> Text -> IO Text
    arg key fallback = do
        value <- argMaybe key
        pure (fromMaybe fallback value)
    argMaybe :: Text -> IO (Maybe Text)
    argMaybe key = case KeyMap.lookup (Key.fromText key) arguments of
        Just (String value) -> pure (Just value)
        _ -> pure Nothing
    argInt :: Text -> Int -> IO Int
    argInt key fallback = case KeyMap.lookup (Key.fromText key) arguments of
        Just (Number value) -> pure (fromMaybe fallback (previewInt value))
        Just (String value) -> pure (fromMaybe fallback (readMaybe (cs value)))
        _ -> pure fallback
      where
        previewInt value = case floatingOrInteger value of
            Right int -> Just int
            Left _ -> Nothing
    argBool :: Text -> Bool -> IO Bool
    argBool key fallback = case KeyMap.lookup (Key.fromText key) arguments of
        Just (Bool value) -> pure value
        _ -> pure fallback

-- Helpers: dashboards
dashboardSchemaDoc :: Text
dashboardSchemaDoc =
    Text.intercalate
        "\n"
        [ "Dashboard config is a JSON array of card objects. Card fields:"
        , "- match: array of clauses {facet, op, value|values} (conjunction)"
        , "- title, groupBy, forEach: facet references; limit (default 100); sortBy; alertSortBy; size {width,height}; hideWhen {match, maxCount}; summary (bool)"
        , "Facet references: field:env|host|service|check|severity|status|muted|title, label:<name>, attr:<facet name>"
        , "  field:muted matches \"true\"/\"false\" (suppressed alerts); field:title supports glob matching"
        , "Match operators: \"=\" (exact), \"!=\" (not equal), \"~\" (glob: * and ?), \"in\" (list), \"not-in\" (exclude list)"
        , "forEach cards: the template expands into one card per distinct facet value. Title templating: {value} or {{value}} in the title is replaced with the pinned value; {{field:name}}, {{attr:name}}, {{label:name}} resolve to the pinned value when the forEach facet is the same reference, else \"-\". Omit title to auto-title cards by the pinned value."
        , "Example:"
        , "[{\"title\":\"Critical alerts\",\"match\":[{\"facet\":\"field:severity\",\"op\":\"in\",\"values\":[\"critical\",\"high\"]},{\"facet\":\"field:muted\",\"op\":\"=\",\"value\":\"false\"}],\"limit\":50}]"
        ]

listDashboards :: (?modelContext :: ModelContext) => AgentContext -> IO Text
listDashboards context = do
    dashboards <- ownDashboards context
    pure case dashboards of
        [] -> "no dashboards yet"
        _ ->
            Text.intercalate
                "\n"
                [ "- "
                    <> dashboard.name
                    <> " (cards="
                    <> tshow (cardCount dashboard)
                    <> (if dashboard.isDefault then ", default" else "")
                    <> ")"
                | dashboard <- dashboards
                ]
  where
    cardCount dashboard = case decodeDashboardConfig dashboard.config of
        Right cards -> length cards
        Left _ -> 0

ownDashboards :: (?modelContext :: ModelContext) => AgentContext -> IO [Dashboard]
ownDashboards context =
    query @Dashboard
        |> filterWhere (#userId, get #id context.acUser)
        |> orderByAsc #position
        |> fetch

fetchOwnDashboardByName :: (?modelContext :: ModelContext) => AgentContext -> Text -> IO Dashboard
fetchOwnDashboardByName context name = fetchOwnDashboard context (Just name) Nothing

-- | Resolve a dashboard by name or UUID (both optional, at least one
-- required), scoped to the acting user — this is what lets the agent act
-- on "this dashboard" from a /dashboards/<uuid> page URL.
fetchOwnDashboard :: (?modelContext :: ModelContext) => AgentContext -> Maybe Text -> Maybe Text -> IO Dashboard
fetchOwnDashboard context mName mIdArg = do
    byId <- case mIdArg of
        Just idArg -> do
            uuid <- maybe (error "id is not a UUID") pure (readMaybe (cs idArg))
            let dashboardId = Id uuid :: Id Dashboard
            found <- fetchOneOrNothing dashboardId
            case found of
                Just dashboard | dashboard.userId == get #id context.acUser -> pure (Just dashboard)
                _ -> error "no dashboard with that id owned by the acting user"
        Nothing -> pure Nothing
    case byId of
        Just dashboard -> checkProtected dashboard
        Nothing -> case mName of
            Just name -> do
                dashboard <-
                    query @Dashboard
                        |> filterWhere (#userId, get #id context.acUser)
                        |> filterWhere (#name, name)
                        |> fetchOneOrNothing
                case dashboard of
                    Just dashboard -> checkProtected dashboard
                    Nothing -> error ("no dashboard named \"" <> name <> "\" owned by the acting user")
            Nothing -> error "pass name or id"
  where
    checkProtected dashboard = do
        when (get #protected dashboard) (error "this dashboard is provisioned-protected")
        pure dashboard

validateDashboard :: (?modelContext :: ModelContext) => Text -> Text -> IO Text
validateDashboard name configText = do
    if Text.null (Text.strip name)
        then pure "invalid: name must be non-empty"
        else case decodeConfigText configText of
            Left err -> pure ("invalid config: " <> err)
            Right cards -> do
                counts <- forM (zip [1 ..] cards) \(index, card) -> do
                    count <- cardBaseQuery card |> fetchCount
                    pure (index, card, count)
                let total = sum [count | (_, _, count) <- counts]
                    lines' =
                        [ "card "
                            <> tshow (index :: Int)
                            <> ": "
                            <> cardTitleText card index
                            <> " ["
                            <> Text.intercalate ", " (map clauseText card.cardMatch)
                            <> "] -> "
                            <> tshow count
                            <> " open alerts"
                        | (index, card, count) <- counts
                        ]
                pure
                    ( "plan: dashboard \""
                        <> name
                        <> "\" with "
                        <> tshow (length cards)
                        <> " card(s), "
                        <> tshow (total :: Int)
                        <> " total open-alert matches\n"
                        <> Text.intercalate "\n" lines'
                    )

createDashboardFor :: (?modelContext :: ModelContext) => AgentContext -> Text -> Text -> Bool -> IO Text
createDashboardFor context name configText confirmed = do
    plan <- validateDashboard name configText
    if not (Text.isPrefixOf "plan:" plan)
        then pure plan
        else
            if not confirmed
                then pure (plan <> "\nconfirmation required: show this plan to the user; call create_dashboard again with confirmed=true only after explicit agreement")
                else case decodeConfigText configText of
                    Left err -> pure ("invalid config: " <> err)
                    Right cards -> do
                        position <- nextPosition context
                        dashboard <-
                            newRecord @Dashboard
                                |> set #userId (get #id context.acUser)
                                |> set #name name
                                |> set #config (encodeDashboardConfig cards)
                                |> set #position position
                                |> set #isDefault False
                                |> createRecord
                        pure ("created dashboard \"" <> name <> "\" (id=" <> tshow (get #id dashboard) <> ")")

nextPosition :: (?modelContext :: ModelContext) => AgentContext -> IO Int
nextPosition context = do
    dashboards <- ownDashboards context
    pure (1 + maximum (0 : map (.position) dashboards))

-- | Models sometimes pass the config as a JSON array/object instead of the
-- documented string; accept both (a String decodes as-is, anything else is
-- re-encoded) so the error below means genuinely unparseable text.
decodeConfigText :: Text -> Either Text [DashboardCard]
decodeConfigText raw =
    case Aeson.decode (cs raw) of
        Just (String value) -> decodeDashboardConfig (String value)
        Just other -> decodeDashboardConfig other
        Nothing -> Left "config is not valid JSON"

cardTitleText :: DashboardCard -> Int -> Text
cardTitleText card index = fromMaybe ("card " <> tshow index) card.cardTitle

clauseText :: MatchClause -> Text
clauseText clause =
    facetRefText clause.mcFacet
        <> " "
        <> opLabel clause.mcOp
        <> " "
        <> case clause.mcOp of
            OpIn -> "[" <> Text.intercalate ", " clause.mcValues <> "]"
            OpNotIn -> "[" <> Text.intercalate ", " clause.mcValues <> "]"
            _ -> "\"" <> clause.mcValue <> "\""
  where
    opLabel op = case op of
        OpEq -> "="
        OpNe -> "!="
        OpGlob -> "~"
        OpIn -> "in"
        OpNotIn -> "not-in"

-- Helpers: teams
splitCsv :: Text -> [Text]
splitCsv = filter (not . Text.null) . map Text.strip . Text.splitOn ","

teamPlanExtras :: Text -> Maybe Text -> Maybe Text -> Text
teamPlanExtras hostGroupsText mmTeam mmChannel =
    (if Text.null hostGroupsText then "" else "; host_groups=" <> hostGroupsText)
        <> ( if isJust mmChannel
                then "; mattermost=" <> fromMaybe "halemans" mmTeam <> "/" <> fromMaybe "" mmChannel
                else ""
           )

-- | Merge the mattermost destination args into a team defaults JSON value
-- ({"mattermost":{"team","channel"}}): Nothing leaves the key unchanged,
-- Just "" removes it.
applyMattermostArgs :: Maybe Text -> Maybe Text -> Aeson.Value -> Aeson.Value
applyMattermostArgs mmTeam mmChannel defaults =
    let base = case defaults of
            Aeson.Object object_ -> object_
            _ -> KeyMap.empty
        existingMm = case KeyMap.lookup "mattermost" base of
            Just (Aeson.Object object_) -> object_
            _ -> KeyMap.empty
        setField key value object_ = case value of
            Nothing -> object_
            Just text
                | Text.null text -> KeyMap.delete (Key.fromText key) object_
                | otherwise -> KeyMap.insert (Key.fromText key) (Aeson.String text) object_
        mm' = setField "team" mmTeam (setField "channel" mmChannel existingMm)
     in if KeyMap.null mm'
            then Aeson.Object (KeyMap.delete "mattermost" base)
            else Aeson.Object (KeyMap.insert "mattermost" (Aeson.Object mm') base)

listTeams :: (?modelContext :: ModelContext) => IO Text
listTeams = do
    teams <- query @Team |> orderByAsc #name |> fetch
    case teams of
        [] -> pure "no teams"
        _ -> do
            lines' <- forM teams \team -> do
                let teamId = get #id team
                members <-
                    sqlQueryTyped
                        [typedSql|
                    SELECT u.email, m.team_role
                    FROM team_members m JOIN users u ON u.id = m.user_id
                    WHERE m.team_id = ${teamId}
                    ORDER BY u.email
                |]
                pure
                    ( "- "
                        <> team.name
                        <> ": "
                        <> ( if null members
                                then "no members"
                                else Text.intercalate ", " [row.email <> " (" <> row.team_role <> ")" | row <- members]
                           )
                    )
            pure (Text.intercalate "\n" lines')

fetchTeamByName :: (?modelContext :: ModelContext) => Text -> IO Team
fetchTeamByName name =
    query @Team
        |> filterWhere (#name, name)
        |> fetchOneOrNothing
        >>= maybe (error ("unknown team: " <> name)) pure

fetchUserByEmail :: (?modelContext :: ModelContext) => Text -> IO User
fetchUserByEmail email =
    query @User
        |> filterWhere (#email, email)
        |> fetchOneOrNothing
        >>= maybe (error ("unknown user: " <> email)) pure

-- Helpers: rules
stepsList :: Aeson.Value -> [Aeson.Value]
stepsList value = case value of
    Array items -> Vector.toList items
    _ -> []

parseSteps :: Text -> IO (Either Text Aeson.Value)
parseSteps raw = pure case Aeson.decode (cs raw) of
    Just value@(Array items) | not (null items) -> Right value
    Just (Array _) -> Left "invalid: steps must be a non-empty JSON array"
    _ -> Left "invalid: steps is not a JSON array"

parseJsonObject :: Text -> IO (Either Text Aeson.Value)
parseJsonObject raw = pure case Aeson.decode (cs raw) of
    Just value@(Object _) -> Right value
    _ -> Left "invalid: expected a JSON object"

prettyJson :: Aeson.Value -> Text
prettyJson = cs . Pretty.encodePretty

protectedMark :: (HasField "protected" record Bool) => record -> Text
protectedMark record = if get #protected record then " [provisioned-protected]" else ""

fetchPolicyByName :: (?modelContext :: ModelContext) => Text -> IO EscalationPolicy
fetchPolicyByName name =
    query @EscalationPolicy
        |> filterWhere (#name, name)
        |> fetchOneOrNothing
        >>= maybe (error ("unknown escalation policy: " <> name)) pure

listNotificationRules :: (?modelContext :: ModelContext) => IO Text
listNotificationRules = do
    rules <- query @NotificationRule |> orderByAsc #position |> fetch
    case rules of
        [] -> pure "no notification rules"
        _ -> do
            lines' <- forM rules \rule -> do
                teamName <- traverse (fmap (.name) . fetch) rule.teamId
                userMail <- traverse (fmap (.email) . fetch) rule.userId
                pure
                    ( "- "
                        <> rule.name
                        <> (if rule.enabled then "" else " (disabled)")
                        <> ": severity>="
                        <> rule.severityThreshold
                        <> ", team="
                        <> fromMaybe "-" teamName
                        <> ", user="
                        <> fromMaybe "-" userMail
                        <> ", channel="
                        <> rule.channel
                        <> protectedMark rule
                    )
            pure (Text.intercalate "\n" lines')

-- Helpers: notification channels
channelTypes :: [Text]
channelTypes = ["browser_push", "email", "mattermost"]

mattermostTemplateLine :: LlmPromptTemplate -> Text
mattermostTemplateLine template =
    "- "
        <> template.name
        <> " v"
        <> tshow template.version
        <> (if template.active then " (active)" else "")
        <> (if template.active then "\n" <> template.body else "")

channelLine :: NotificationChannel -> Text
channelLine channel =
    "- "
        <> channel.name
        <> " ("
        <> channel.type_
        <> ")"
        <> (if channel.enabled then "" else " (disabled)")
        <> (if Text.null channel.baseUrl then "" else ", base_url=" <> channel.baseUrl)
        <> (let tokenEnv = channelTokenEnv channel in if Text.null tokenEnv then "" else ", token_env=" <> tokenEnv)
        <> protectedMark channel

channelTokenEnv :: NotificationChannel -> Text
channelTokenEnv channel = case get #config channel of
    Aeson.Object object_ -> case KeyMap.lookup "tokenEnv" object_ of
        Just (Aeson.String value) -> value
        _ -> ""
    _ -> ""

-- | Overlay the managed tokenEnv key onto a channel config JSON (the
-- sourceConfig/channelFromForm pattern): empty value removes the key, other
-- config keys survive.
tokenEnvConfig :: Text -> Aeson.Value -> Aeson.Value
tokenEnvConfig tokenEnv config =
    Aeson.Object
        ( if Text.null tokenEnv
            then KeyMap.delete "tokenEnv" base
            else KeyMap.insert "tokenEnv" (Aeson.String tokenEnv) base
        )
  where
    base = case config of
        Aeson.Object object_ -> object_
        _ -> KeyMap.empty

fetchNotificationChannelByName :: (?modelContext :: ModelContext) => Text -> IO NotificationChannel
fetchNotificationChannelByName name =
    query @NotificationChannel
        |> filterWhere (#name, name)
        |> fetchOneOrNothing
        >>= maybe (error ("unknown notification channel: " <> name)) pure

referencedChannelNames :: (?modelContext :: ModelContext) => IO [Text]
referencedChannelNames = do
    rules <- query @NotificationRule |> fetch
    pure (nub [rule.channel | rule <- rules, rule.channel /= ""])

fetchNotificationRuleByName :: (?modelContext :: ModelContext) => Text -> IO NotificationRule
fetchNotificationRuleByName name =
    query @NotificationRule
        |> filterWhere (#name, name)
        |> fetchOneOrNothing
        >>= maybe (error ("unknown notification rule: " <> name)) pure

fetchGroupingRuleByName :: (?modelContext :: ModelContext) => Text -> IO GroupingRule
fetchGroupingRuleByName name =
    query @GroupingRule
        |> filterWhere (#name, name)
        |> fetchOneOrNothing
        >>= maybe (error ("unknown grouping rule: " <> name)) pure

-- Helpers: sources
setSourceEnabled :: (?modelContext :: ModelContext) => Bool -> Text -> IO Text
setSourceEnabled enabled name = do
    source <-
        query @Source
            |> filterWhere (#name, name)
            |> fetchOneOrNothing
            >>= maybe (error ("unknown source: " <> name)) pure
    when (get #protected source) (error "this source is provisioned-protected")
    _ <- source |> set #enabled enabled |> updateRecord
    pure (source.name <> " " <> (if enabled then "enabled" else "disabled"))

getLlmConfig :: (?modelContext :: ModelContext) => IO Text
getLlmConfig = do
    config <- currentLlmConfig
    pure case config of
        Nothing -> "no LLM provider configured"
        Just config ->
            Text.intercalate
                "\n"
                [ "provider: " <> config.providerName
                , "model: " <> config.model
                , "endpoint: " <> config.endpoint
                , "tools enabled: " <> (if config.toolsEnabled then "yes" else "no")
                ]

-- The agent reading its own traces: renders the most recent turn of the
-- current session from the persisted per-round trace JSON (with a fallback
-- to the tool_calls replay data for rows written before traces existed).
explainLastTurn :: (?modelContext :: ModelContext) => AgentContext -> IO Text
explainLastTurn context = case context.acSessionId of
    Nothing -> pure "explain_last_turn needs a chat session context (not available via the internal HTTP API or MCP server)"
    Just sessionId -> do
        userRows <-
            query @AgentMessage
                |> filterWhere (#sessionId, sessionId)
                |> filterWhere (#role_, "user" :: Text)
                |> orderByDesc #createdAt
                |> limit 1
                |> fetch
        case userRows of
            [] -> pure "no turns yet in this conversation"
            (userRow : _) -> do
                rows <-
                    query @AgentMessage
                        |> filterWhere (#sessionId, sessionId)
                        |> orderByAsc #createdAt
                        |> fetch
                let turnRows = case dropWhile (\row -> get #id row /= get #id userRow) rows of
                        (_ : rest) -> [row | row <- rest, row.role_ == "assistant"]
                        [] -> []
                if null turnRows
                    then pure "the previous turn produced no assistant rows (check the session history)"
                    else do
                        let roundLines = zipWith (renderRound sessionId) [1 ..] turnRows
                        pure ("turn trace for session " <> tshow sessionId <> ":\n" <> Text.intercalate "\n" roundLines)
  where
    renderRound _sessionId index row =
        case row.trace of
            Just traceValue -> renderTracedRound index traceValue
            Nothing -> renderLegacyRound index row

    renderTracedRound index traceValue =
        case traceValue of
            Object _ ->
                let duration = traceTextField "duration_ms" traceValue
                    tokensIn = traceTextField "tokens_in" traceValue
                    tokensOut = traceTextField "tokens_out" traceValue
                    err = traceMaybeField "error" traceValue
                    calls = traceCalls traceValue
                 in "round "
                        <> tshow (index :: Int)
                        <> ": "
                        <> duration
                        <> "ms"
                        <> (if Text.null tokensIn then "" else ", tokens " <> tokensIn <> "/" <> tokensOut)
                        <> ( case err of
                                Just errText -> " — ERROR: " <> errText
                                Nothing -> ""
                           )
                        <> (if null calls then "" else "\n" <> Text.intercalate "\n" calls)
            _ -> "round " <> tshow (index :: Int) <> ": (unreadable trace)"

    renderLegacyRound index row =
        let calls = case row.toolCalls of
                Just callsValue -> legacyCalls callsValue
                Nothing -> []
         in "round "
                <> tshow (index :: Int)
                <> ": (no trace recorded)"
                <> (if null calls then "" else "\n" <> Text.intercalate "\n" calls)

    legacyCalls callsValue = case callsValue of
        Aeson.Array items ->
            [ "  - "
                <> fromMaybe "?" (textOf "name" item)
                <> " "
                <> fromMaybe "{}" (textOf "arguments" item)
                <> " — ok"
            | item <- Vector.toList items
            ]
        _ -> []

    traceCalls traceValue = case lookupIn "tool_calls" traceValue of
        Just (Aeson.Array items) ->
            [ "  - "
                <> fromMaybe "?" (textOf "name" item)
                <> " "
                <> fromMaybe "{}" (textOf "arguments" item)
                <> " — "
                <> fromMaybe "?" (textOf "duration_ms" item)
                <> "ms — "
                <> Text.take 200 (fromMaybe "" (textOf "result" item))
            | item <- Vector.toList items
            ]
        _ -> []

    traceTextField key value = fromMaybe "" (traceMaybeField key value)

    traceMaybeField key value = textOf key value

    textOf key value = case lookupIn key value of
        Just (Aeson.String text) -> Just text
        Just (Aeson.Number number) -> Just (renderNumber number)
        _ -> Nothing

    renderNumber number = case floatingOrInteger number of
        Right int -> tshow (int :: Int64)
        Left _ -> cs (show number)

    lookupIn key value = case value of
        Aeson.Object obj -> KeyMap.lookup (Key.fromText key) obj
        _ -> Nothing
