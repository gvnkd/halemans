module Web.Controller.InternalApi where

import Application.Service.Agent.Tools (AgentContext (..), executeAgentTool)
import Application.Service.Api.InternalAuth (withInternalToken)
import Application.Service.I18n (agentLanguageName)
import qualified Application.Service.Llm as Llm
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseMaybe)
import Generated.Types
import IHP.Prelude
import Web.Controller.Prelude

-- Internal JSON API (internal API milestone). Unversioned and unstable —
-- for the local agent and tests; endpoints are promoted to /api/v1 as they
-- stabilize. M1 wraps the agent tool registry directly and returns the
-- in-band text result under {"result": ...}; a promoted public endpoint
-- replaces this with a real schema when it graduates.

instance Controller InternalApiController where
    action InternalEnvironmentsAction = runTool "list_environments" "{}"
    action InternalDashboardsAction = runTool "list_dashboards" "{}"
    action InternalDashboardSchemaAction = runTool "get_dashboard_schema" "{}"
    action InternalValidateDashboardAction = do
        let name = param @Text "name"
            config = param @Text "config"
        runTool "validate_dashboard" (encodeArgs [("name", Aeson.toJSON name), ("config", Aeson.toJSON config)])
    action InternalCreateDashboardAction = do
        let name = param @Text "name"
            config = param @Text "config"
            confirmed = fromMaybe False (paramOrNothing @Bool "confirmed")
        runTool
            "create_dashboard"
            (encodeArgs [("name", Aeson.toJSON name), ("config", Aeson.toJSON config), ("confirmed", Aeson.toJSON confirmed)])
    action InternalSearchAlertsAction = do
        let filters =
                [ (key, value)
                | key <- ["env", "host", "service", "severity", "status", "limit"]
                , Just value <- [paramOrNothing @Text (cs key)]
                ]
        runTool "search_alerts" (encodeArgs [(key, Aeson.String value) | (key, value) <- filters])
    action InternalLlmConfigAction = runTool "get_llm_config" "{}"

-- One tool implementation behind both surfaces: the internal HTTP API passes
-- the act-as user from InternalAuth into the same executor the LLM loop and
-- the MCP server use.
runTool :: (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext) => Text -> Text -> IO ResponseReceived
runTool name arguments = withInternalToken \user -> do
    language <- agentLanguageName (userLanguageCode user)
    let context = AgentContext{acUser = user, acLanguage = language, acSessionId = Nothing}
    output <- executeAgentTool context (Llm.ToolCall name name arguments)
    renderJson (object ["result" .= output])

userLanguageCode :: User -> Maybe Text
userLanguageCode user =
    fromMaybe
        Nothing
        (parseMaybe (Aeson.withObject "settings" (\o -> o Aeson..:? "language")) user.settings)

encodeArgs :: [(Text, Value)] -> Text
encodeArgs pairs = cs (Aeson.encode (Aeson.object [(Key.fromText key, value) | (key, value) <- pairs]))
