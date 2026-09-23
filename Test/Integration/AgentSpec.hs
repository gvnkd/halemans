module Test.Integration.AgentSpec (spec) where

import Application.Helper.Controller (userPrivileges)
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest)
import Application.Service.Agent.Mcp (McpConfig (..), defaultMcpEmail, defaultMcpRoleName, handleMessage, resolveMcpUser)
import Application.Service.Agent.Tools (AgentContext (..), executeAgentTool)
import qualified Application.Service.Llm as Llm
import Control.Exception (finally)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import Data.UUID.V4 (nextRandom)
import qualified Data.Vector as Vector
import Generated.Types
import IHP.Fetch (fetch)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, query)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec
import Test.Integration.Setup (freshFingerprint, m6User, restoreEnv, testEventIn, testSource)

-- Agent tool registry + MCP protocol (internal API milestone). The internal
-- HTTP layer is a thin wrapper over the same executor; auth/audit HTTP
-- behavior is exercised manually until an HTTP harness exists.
spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = describe "agent tools (internal API milestone)" do
    describe "executeAgentTool" do
        it "lists environments with open alert counts" do
            source <- testSource
            envName <- ("agent-env-" <>) . tshow <$> nextRandom
            fingerprint <- freshFingerprint
            _ <- ingest source (testEventIn envName fingerprint Firing)
            user <- m6User ["view"]
            output <- runTool user "list_environments" "{}"
            output `shouldSatisfy` (envName `Text.isInfixOf`)

        it "validates a config and counts matching open alerts" do
            source <- testSource
            envName <- ("agent-env-" <>) . tshow <$> nextRandom
            fingerprint <- freshFingerprint
            _ <- ingest source (testEventIn envName fingerprint Firing)
            user <- m6User ["view"]
            let onlyConfig = "[{\"title\":\"Only env\",\"match\":[{\"facet\":\"field:env\",\"op\":\"in\",\"values\":[\"" <> envName <> "\"]}]}]"
            onlyPlan <- runTool user "validate_dashboard" (args [("name", "t"), ("config", onlyConfig)])
            onlyPlan `shouldSatisfy` ("plan:" `Text.isInfixOf`)
            onlyPlan `shouldSatisfy` ("1 open alerts" `Text.isInfixOf`)
            let excludeConfig = "[{\"title\":\"All but one\",\"match\":[{\"facet\":\"field:env\",\"op\":\"not-in\",\"values\":[\"" <> envName <> "\"]}]}]"
            excludePlan <- runTool user "validate_dashboard" (args [("name", "t"), ("config", excludeConfig)])
            excludePlan `shouldSatisfy` ("All but one" `Text.isInfixOf`)
            excludePlan `shouldSatisfy` ("not-in" `Text.isInfixOf`)
            bad <- runTool user "validate_dashboard" (args [("name", "t"), ("config", "[{]")])
            bad `shouldSatisfy` ("invalid config" `Text.isInfixOf`)

        it "requires confirmation before creating a dashboard" do
            user <- m6User ["view"]
            email <- ("agent-dash-" <>) . tshow <$> nextRandom
            let name = "agent-test " <> email
                config = "[{\"title\":\"x\",\"match\":[]}]"
            plan <- runTool user "create_dashboard" (args [("name", name), ("config", config)])
            plan `shouldSatisfy` ("confirmation required" `Text.isInfixOf`)
            created <- query @Dashboard |> filterWhere (#name, name) |> fetch
            length created `shouldBe` 0
            done <- runTool user "create_dashboard" (argsV [("name", String name), ("config", String config), ("confirmed", Bool True)])
            done `shouldSatisfy` ("created dashboard" `Text.isInfixOf`)
            rows <- query @Dashboard |> filterWhere (#name, name) |> fetch
            map (.userId) rows `shouldBe` [get #id user]

        it "searches alerts for the acting user with the view privilege" do
            source <- testSource
            envName <- ("agent-env-" <>) . tshow <$> nextRandom
            fingerprint <- freshFingerprint
            _ <- ingest source (testEventIn envName fingerprint Firing)
            user <- m6User ["view"]
            found <- runTool user "search_alerts" (args [("env", envName)])
            found `shouldSatisfy` (fingerprint `Text.isInfixOf`)
            none <- runTool user "search_alerts" (args [("env", "no-such-env")])
            none `shouldBe` "no matching alerts"
            demoted <- m6User ["ack"]
            denied <- runTool demoted "search_alerts" "{}"
            denied `shouldSatisfy` ("forbidden" `Text.isInfixOf`)

        it "soft-fails unknown tools and invalid arguments" do
            user <- m6User ["view"]
            runTool user "nope" "{}" `shouldReturn` "unknown tool: nope"
            runTool user "search_alerts" "{]" `shouldReturn` "invalid arguments for search_alerts"

    describe "resolveMcpUser (default service account)" do
        it "auto-creates mcp@localhost with a single dedicated role, idempotently" do
            oldUser <- lookupEnv "HALEMANS_MCP_USER"
            oldPrivs <- lookupEnv "HALEMANS_MCP_PRIVILEGES"
            flip finally (restoreEnv "HALEMANS_MCP_USER" oldUser >> restoreEnv "HALEMANS_MCP_PRIVILEGES" oldPrivs) do
                unsetEnv "HALEMANS_MCP_USER"
                setEnv "HALEMANS_MCP_PRIVILEGES" "view"
                user1 <- resolveMcpUser
                user1.email `shouldBe` defaultMcpEmail
                user1.passwordHash `shouldBe` "!"
                userPrivileges (get #id user1) `shouldReturn` ["view"]
                user2 <- resolveMcpUser
                get #id user2 `shouldBe` get #id user1
                roles <- query @Role |> filterWhere (#name, defaultMcpRoleName) |> fetch
                length roles `shouldBe` 1
        it "syncs HALEMANS_MCP_PRIVILEGES onto the role on every boot" do
            oldUser <- lookupEnv "HALEMANS_MCP_USER"
            oldPrivs <- lookupEnv "HALEMANS_MCP_PRIVILEGES"
            flip finally (restoreEnv "HALEMANS_MCP_USER" oldUser >> restoreEnv "HALEMANS_MCP_PRIVILEGES" oldPrivs) do
                unsetEnv "HALEMANS_MCP_USER"
                setEnv "HALEMANS_MCP_PRIVILEGES" "view,manage_rules"
                user <- resolveMcpUser
                sort <$> userPrivileges (get #id user) `shouldReturn` sort ["view", "manage_rules"]
        it "resolves HALEMANS_MCP_USER when set and fails on unknown" do
            user <- m6User ["view"]
            old <- lookupEnv "HALEMANS_MCP_USER"
            flip finally (restoreEnv "HALEMANS_MCP_USER" old) do
                setEnv "HALEMANS_MCP_USER" (cs user.email)
                resolved <- resolveMcpUser
                get #id resolved `shouldBe` get #id user
                setEnv "HALEMANS_MCP_USER" "nobody@dev"
                resolveMcpUser `shouldThrow` anyException

    describe "MCP protocol (stdio JSON-RPC)" do
        it "answers initialize and lists tools" do
            user <- m6User ["view"]
            let config = testMcpConfig user
            Just initResult <- handleMessage config (rpcRequest 1 "initialize")
            lookupString "result.protocolVersion" initResult `shouldBe` Just "2025-03-26"
            Just listResult <- handleMessage config (rpcRequest 2 "tools/list")
            let toolNames = do
                    result <- lookupKey "result" listResult
                    tools <- lookupKey "tools" result
                    case tools of
                        Array items -> mapM (lookupKeyAsText "name") (Vector.toList items)
                        _ -> Nothing
            fmap length toolNames `shouldBe` Just 7

        it "executes tools/call and flags errors" do
            user <- m6User ["view"]
            let config = testMcpConfig user
                callParams :: Text -> Value -> Value
                callParams name argsValue = object ["name" .= name, "arguments" .= argsValue]
            Just okResult <- handleMessage config (rpcRequestWithParams 3 "tools/call" (callParams "list_dashboards" (object [])))
            lookupBool "result.isError" okResult `shouldBe` Just False
            Just errResult <- handleMessage config (rpcRequestWithParams 4 "tools/call" (callParams "nope" (object [])))
            lookupBool "result.isError" errResult `shouldBe` Just True

        it "ignores notifications and rejects unknown methods" do
            user <- m6User ["view"]
            let config = testMcpConfig user
            handleMessage config (object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("notifications/initialized" :: Text)]) `shouldReturn` Nothing
            unknown <- handleMessage config (rpcRequest 5 "bogus/method")
            (lookupNumber "error.code" =<< unknown) `shouldBe` Just (-32601)
  where
    runTool user name arguments = do
        let context = AgentContext{acUser = user, acLanguage = "English"}
        executeAgentTool context (Llm.ToolCall name name arguments)
    args pairs = argsV [(key, String value) | (key, value) <- pairs]
    argsV pairs = cs (Aeson.encode (Aeson.object [(Key.fromText key, value) | (key, value) <- pairs]))
    testMcpConfig user = McpConfig{mcpUser = user, mcpLanguage = "English"}
    rpcRequest :: Int -> Text -> Value
    rpcRequest id' method = rpcRequestWithParams id' method (object [])
    rpcRequestWithParams :: Int -> Text -> Value -> Value
    rpcRequestWithParams id' method params =
        object ["jsonrpc" .= ("2.0" :: Text), "id" .= id', "method" .= method, "params" .= params]
    lookupKey :: KeyMap.Key -> Value -> Maybe Value
    lookupKey key value = case value of
        Object obj -> KeyMap.lookup key obj
        _ -> Nothing
    lookupKeyAsText :: Text -> Value -> Maybe Text
    lookupKeyAsText key value = case lookupKey (Key.fromText key) value of
        Just (String text) -> Just text
        _ -> Nothing
    lookupPath keys value = foldl (\acc key -> acc >>= \v -> lookupKey key v) (Just value) keys
    lookupString dotted value = case lookupPath (map Key.fromText (Text.splitOn "." dotted)) value of
        Just (String text) -> Just text
        _ -> Nothing
    lookupBool dotted value = case lookupPath (Key.fromText <$> Text.splitOn "." dotted) value of
        Just (Bool b) -> Just b
        _ -> Nothing
    lookupNumber dotted value = case lookupPath (Key.fromText <$> Text.splitOn "." dotted) value of
        Just (Number n) -> Just (floor n)
        _ -> Nothing
