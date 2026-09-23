module Application.Service.Agent.Tools (
    AgentContext (..),
    agentToolDefinitions,
    executeAgentTool,
) where

import Application.Helper.Controller (userPrivileges)
import Application.Helper.DashboardConfig
import Application.Service.Api.Alerts (AlertFilters (..), defaultFilters, listAlertsPage)
import Application.Service.DashboardCards (cardBaseQuery)
import Application.Service.Llm (LlmProviderConfig (..), ToolCall (..))
import Application.Service.Llm.DbConfig (currentLlmConfig)
import Data.Aeson (Value (..), object, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Int (Int64)
import qualified Data.Text as Text
import Generated.Types hiding (createDashboard)
import IHP.Fetch (fetch, fetchCount)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, orderByAsc, query)
import IHP.TypedSql (sqlQueryTyped, typedSql)

-- Agent tool registry (internal API milestone): the single implementation of
-- every tool the Halemans agent (web chat, MCP server, internal HTTP API) can
-- call. Executors are request-independent — they take an explicit act-as
-- AgentContext instead of leaning on ?request/currentUser — so the same code
-- runs in the web process, the worker and the stdio MCP server. Tools return
-- in-band text results (soft-fail) exactly like
-- Application.Service.Llm.Tools; mutating tools use the validate -> confirm
-- -> apply pattern with a `confirmed` argument instead of session state.

data AgentContext = AgentContext
    { acUser :: User
    -- ^ The user the agent acts on behalf of (act-as). Tool-level privilege
    -- checks run against this user's roles.
    , acLanguage :: Text
    -- ^ Display language name (users.settings.language) for hint text.
    }

agentToolDefinitions :: [Value]
agentToolDefinitions =
    [ function "list_environments" "List monitoring environments with the count of currently open (non-closed) alerts in each. Requires the view privilege." []
    , function "list_dashboards" "List the current user's dashboards: id, name, whether it is the default, and card count." []
    , function "get_dashboard_schema" "Get the dashboard card config schema: card fields, facet references, match operators and an example card." []
    , function
        "validate_dashboard"
        "Validate a dashboard config without creating it: checks card JSON, then counts currently matching open alerts per card. Always call this before create_dashboard and show the plan to the user."
        [ ("name", stringProp "dashboard name" True)
        , ("config", stringProp "card config as a JSON array string (see get_dashboard_schema)" True)
        ]
    , function
        "create_dashboard"
        "Create a dashboard for the current user. Two-phase: call with confirmed=false first and present the returned plan to the user; only after the user explicitly agrees call again with confirmed=true."
        [ ("name", stringProp "dashboard name" True)
        , ("config", stringProp "card config as a JSON array string" True)
        , ("confirmed", boolProp "apply for real; false returns the plan without creating anything" False)
        ]
    , function
        "search_alerts"
        "Search open alerts (newest first). All filters optional, exact match on effective env/host/service values. Requires the view privilege."
        [ ("env", stringProp "effective environment name" False)
        , ("host", stringProp "effective host name" False)
        , ("service", stringProp "effective service name" False)
        , ("severity", stringProp "critical|high|warning|info" False)
        , ("status", stringProp "firing|ack|resolved|stalled|closed" False)
        , ("limit", intProp "max results, default 20, max 100" False)
        ]
    , function "get_llm_config" "Get the configured LLM provider name, model and endpoint (never the API key). For agent bootstrap." []
    ]

function :: Text -> Text -> [(Text, Value)] -> Value
function name description props =
    object
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
  where
    propRequired schema = fromMaybe False (parseMaybe (Aeson.withObject "prop" (\o -> o .:? "x-required" .!= False)) schema)

stringProp :: Text -> Bool -> Value
stringProp description required =
    object ["type" .= ("string" :: Text), "description" .= description, "x-required" .= required]

boolProp :: Text -> Bool -> Value
boolProp description required =
    object ["type" .= ("boolean" :: Text), "description" .= description, "x-required" .= required]

intProp :: Text -> Bool -> Value
intProp description required =
    object ["type" .= ("integer" :: Text), "description" .= description, "x-required" .= required]

executeAgentTool :: (?modelContext :: ModelContext) => AgentContext -> ToolCall -> IO Text
executeAgentTool context call = case call.callName of
    "list_environments" -> withViewPrivilege listEnvironments
    "list_dashboards" -> listDashboards context
    "get_dashboard_schema" -> pure dashboardSchemaDoc
    "validate_dashboard" ->
        runArgs
            ( objectArgs \o -> do
                name <- o .: "name"
                configText <- o .: "config"
                pure (validateDashboard context name configText)
            )
    "create_dashboard" ->
        runArgs
            ( objectArgs \o -> do
                name <- o .: "name"
                configText <- o .: "config"
                confirmed <- o .:? "confirmed" .!= False
                pure (createDashboard context name configText confirmed)
            )
    "search_alerts" -> withViewPrivilege (\_ -> runArgs (searchAlerts context))
    "get_llm_config" -> getLlmConfig
    other -> pure ("unknown tool: " <> other)
  where
    withViewPrivilege run = do
        privileges <- userPrivileges (get #id context.acUser)
        if "view" `elem` privileges
            then run context
            else pure "forbidden: the acting user lacks the view privilege"
    -- Args parsers run over the decoded JSON arguments and produce the IO
    -- action; a parse failure is reported in-band (soft-fail, like
    -- Application.Service.Llm.Tools).
    runArgs :: (Value -> Parser (IO Text)) -> IO Text
    runArgs parser = case decodeArgs parser of
        Just action -> action
        Nothing -> pure ("invalid arguments for " <> call.callName)
    decodeArgs parser = do
        decoded <- Aeson.decode (cs call.callArguments) :: Maybe Value
        parseMaybe parser decoded
    objectArgs :: (Aeson.Object -> Parser (IO Text)) -> Value -> Parser (IO Text)
    objectArgs = Aeson.withObject (cs call.callName)

-- | Environments with open-alert counts (LEFT JOIN keeps empty envs).
listEnvironments :: (?modelContext :: ModelContext) => AgentContext -> IO Text
listEnvironments _context = do
    rows <-
        sqlQueryTyped
            [typedSql|
        SELECT e.name, COUNT(a.id) AS n
        FROM environments e
        LEFT JOIN alerts a ON a.environment_id = e.id AND a.status <> 'closed'
        GROUP BY e.name
        ORDER BY e.name
    |]
    pure case rows of
        [] -> "no environments defined"
        _ -> Text.intercalate "\n" ["- " <> row.name <> ": " <> tshow (row.n :: Int64) <> " open alerts" | row <- rows]

listDashboards :: (?modelContext :: ModelContext) => AgentContext -> IO Text
listDashboards context = do
    dashboards <-
        query @Dashboard
            |> filterWhere (#userId, get #id context.acUser)
            |> orderByAsc #position
            |> fetch
    pure case dashboards of
        [] -> "no dashboards yet"
        _ ->
            Text.intercalate
                "\n"
                [ "- "
                    <> dashboard.name
                    <> " (id="
                    <> tshow (get #id dashboard)
                    <> ", cards="
                    <> tshow (cardCount dashboard)
                    <> (if dashboard.isDefault then ", default" else "")
                    <> ")"
                | dashboard <- dashboards
                ]
  where
    cardCount dashboard = case decodeDashboardConfig dashboard.config of
        Right cards -> length cards
        Left _ -> 0

-- | Static description of the card config format the dashboard edit form
-- accepts (design_docs/milestone_9.md §4) plus the new not-in operator.
dashboardSchemaDoc :: Text
dashboardSchemaDoc =
    Text.intercalate
        "\n"
        [ "Dashboard config is a JSON array of card objects. Card fields:"
        , "- match: array of clauses {facet, op, value|values} (conjunction)"
        , "- title, groupBy, forEach: facet references; limit (default 100); sortBy; alertSortBy; size {width,height}; hideWhen {match, maxCount}; summary (bool)"
        , "Facet references: field:env|host|service|check|severity|status, label:<name>, attr:<facet name>"
        , "Match operators: \"=\" (exact), \"!=\" (not equal), \"~\" (glob: * and ?), \"in\" (list), \"not-in\" (exclude list)"
        , "Example:"
        , "[{\"title\":\"Critical alerts\",\"match\":[{\"facet\":\"field:severity\",\"op\":\"in\",\"values\":[\"critical\",\"high\"]},{\"facet\":\"field:env\",\"op\":\"not-in\",\"values\":[\"prod-eu\"]}],\"limit\":50}]"
        ]

validateDashboard :: (?modelContext :: ModelContext) => AgentContext -> Text -> Text -> IO Text
validateDashboard _context name configText = do
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

createDashboard :: (?modelContext :: ModelContext) => AgentContext -> Text -> Text -> Bool -> IO Text
createDashboard context name configText confirmed = do
    plan <- validateDashboard context name configText
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
    dashboards <-
        query @Dashboard
            |> filterWhere (#userId, get #id context.acUser)
            |> fetch
    pure (1 + maximum (0 : map (.position) dashboards))

searchAlerts :: (?modelContext :: ModelContext) => AgentContext -> Value -> Parser (IO Text)
searchAlerts _context = Aeson.withObject "search_alerts" \o -> do
    env <- o .:? "env" .!= ""
    host <- o .:? "host" .!= ""
    service <- o .:? "service" .!= ""
    severity <- o .:? "severity" .!= ""
    status <- o .:? "status" .!= ""
    limitRaw <- o .:? "limit" .!= (20 :: Int)
    let limit = max 1 (min 100 limitRaw)
    pure do
        (alerts, _cursor) <-
            listAlertsPage
                defaultFilters
                    { afEnvironment = env
                    , afHost = host
                    , afService = service
                    , afSeverity = severity
                    , afStatus = status
                    , afLimit = limit
                    }
        pure case alerts of
            [] -> "no matching alerts"
            _ -> Text.intercalate "\n" (map alertLine alerts)
  where
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

decodeConfigText :: Text -> Either Text [DashboardCard]
decodeConfigText raw =
    maybe (Left "config is not valid JSON") decodeDashboardConfig (Aeson.decode (cs raw))

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
