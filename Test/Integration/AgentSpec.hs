module Test.Integration.AgentSpec (spec) where

import Application.Helper.Controller (userPrivileges)
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest)
import Application.Pipeline.Blackouts (openEndedBlackoutEndsAt)
import Application.Service.Agent.Core (agentTurnGate, buildSystemMessage, defaultAgentTemplateBody, internalAgentTemplateName, maxToolRounds, runAgentTurnWith)
import Application.Service.Agent.Mcp (McpConfig (..), defaultMcpEmail, defaultMcpRoleName, handleMessage, resolveMcpUser)
import Application.Service.Agent.Tools (AgentContext (..), agentToolDefinitionsFor, channelTokenEnv, executeAgentTool, requiredPrivilegeFor)
import Application.Service.Llm (Completion (..), Prompt (..))
import qualified Application.Service.Llm as Llm
import Application.Service.Llm.AgentConfig (AgentBudgetConfig (..), agentBudgetConfig, defaultAgentBudgetConfig, saveAgentBudgetConfig)
import Application.Service.Llm.GlobalConfig (GlobalBudgetConfig (..), globalBudgetConfig, saveGlobalBudgetConfig)
import Control.Exception (finally)
import Control.Monad (void)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.List
import qualified Data.Text as Text
import Data.UUID.V4 (nextRandom)
import qualified Data.Vector as Vector
import Generated.Types
import IHP.Fetch (fetch, fetchOne, fetchOneOrNothing)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, orderByAsc, query)
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec
import Test.Integration.Setup (freshFingerprint, m6User, restoreEnv, testEventIn, testSource)
import Web.View.Teams.New (mattermostFieldValues)

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

        it "validate_dashboard accepts config passed as raw JSON, not only a string" do
            user <- m6User ["view"]
            let configArray = Aeson.Array (Vector.fromList [object ["title" .= ("{{value}} card" :: Text), "match" .= ([] :: [Int])]])
            rawValue <- runTool user "validate_dashboard" (argsKV [("name", String "t"), ("config", configArray)])
            rawValue `shouldSatisfy` ("plan:" `Text.isPrefixOf`)
            garbage <- runTool user "validate_dashboard" (args [("name", "t"), ("config", "{]")])
            garbage `shouldSatisfy` ("invalid config" `Text.isPrefixOf`)

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

        it "search_alerts supports globs, title filter, pagination and totals" do
            source <- testSource
            token <- ("g" <>) . tshow <$> nextRandom
            let envName = "agent-glob-" <> token <> "-env"
                envGlob = "agent-glob-" <> token <> "-*"
                hostName = "agent-glob-" <> token <> "-web-01"
            let fire = do
                    fp <- freshFingerprint
                    ingest source ((testEventIn envName fp Firing){host = Just hostName, title = "VIP IP failover"})
            Just _ <- fire
            Just _ <- fire
            Just _ <- fire
            user <- m6User ["view"]
            globbed <- runTool user "search_alerts" (args [("env", envGlob)])
            globbed `shouldSatisfy` ("total: 3 matching" `Text.isPrefixOf`)
            page1 <- runTool user "search_alerts" (args [("env", envName), ("limit", "2")])
            page1 `shouldSatisfy` ("total: 3 matching alert(s); showing 2" `Text.isPrefixOf`)
            page1 `shouldSatisfy` ("next_cursor:" `Text.isInfixOf`)
            -- IHP.Prelude's last is Maybe (safe list accessors)
            Just cursor <- pure (last (Text.lines page1) >>= Text.stripPrefix "next_cursor: ")
            page2 <- runTool user "search_alerts" (args [("env", envName), ("limit", "2"), ("cursor", cursor)])
            page2 `shouldSatisfy` ("showing 1" `Text.isInfixOf`)
            page2 `shouldSatisfy` (not . ("next_cursor:" `Text.isInfixOf`))
            byTitle <- runTool user "search_alerts" (args [("env", envName), ("title", "VIP IP*")])
            byTitle `shouldSatisfy` ("total: 3" `Text.isInfixOf`)
            none <- runTool user "search_alerts" (args [("env", envName), ("title", "vip ip*")])
            none `shouldBe` "no matching alerts"
            badCursor <- runTool user "search_alerts" (args [("cursor", "bogus")])
            badCursor `shouldSatisfy` ("invalid arguments" `Text.isPrefixOf`)

        it "get_alert returns the full record for any status with comments and escalation" do
            source <- testSource
            envName <- ("agent-get-" <>) . tshow <$> nextRandom
            fp <- freshFingerprint
            Just alertId <- ingest source (testEventIn envName fp Firing)
            user <- m6User ["view", "ack", "close"]
            let aid = tshow alertId
            out <- runTool user "get_alert" (args [("alert_id", aid)])
            out `shouldSatisfy` ("status: firing" `Text.isInfixOf`)
            out `shouldSatisfy` ("title: integration test alert" `Text.isInfixOf`)
            out `shouldSatisfy` (("env: " <> envName) `Text.isInfixOf`)
            out `shouldSatisfy` ("muted: false" `Text.isInfixOf`)
            out `shouldSatisfy` (("id: " <> aid) `Text.isInfixOf`)
            _ <- runTool user "comment_alert" (args [("alert_id", aid), ("body", "agent probe comment")])
            policy <- newRecord @EscalationPolicy |> set #name ("agent-get-pol-" <> envName) |> createRecord
            _ <-
                newRecord @EscalationTracker
                    |> set #alertId alertId
                    |> set #policyId (get #id policy)
                    |> set #currentStep 2
                    |> set #status "active"
                    |> createRecord
            -- close requires acked/stalled (state machine): ack first
            _ <- runTool user "ack_alert" (args [("alert_id", aid), ("comment", "acked before close")])
            ackedOut <- runTool user "get_alert" (args [("alert_id", aid)])
            ackedOut `shouldSatisfy` ("status: ack" `Text.isInfixOf`)
            ackedOut `shouldSatisfy` ("acknowledged_at:" `Text.isInfixOf`)
            _ <- runTool user "close_alert" (args [("alert_id", aid), ("reason", "agent done")])
            closed <- runTool user "get_alert" (args [("alert_id", aid)])
            closed `shouldSatisfy` ("status: closed" `Text.isInfixOf`)
            closed `shouldSatisfy` ("close_reason: agent done" `Text.isInfixOf`)
            closed `shouldSatisfy` ("closed_at:" `Text.isInfixOf`)
            closed `shouldSatisfy` ("agent probe comment" `Text.isInfixOf`)
            closed `shouldSatisfy` ("policy" `Text.isInfixOf`)
            closed `shouldSatisfy` ("step 2" `Text.isInfixOf`)
            unknown <- runTool user "get_alert" (args [("alert_id", "00000000-0000-0000-0000-000000000000")])
            unknown `shouldSatisfy` ("invalid arguments" `Text.isPrefixOf`)

        it "close_alerts and ack_alerts run two-phase in bulk, all or nothing" do
            source <- testSource
            envName <- ("agent-bulk-" <>) . tshow <$> nextRandom
            fp1 <- freshFingerprint
            fp2 <- freshFingerprint
            Just a1 <- ingest source (testEventIn envName fp1 Firing)
            Just a2 <- ingest source (testEventIn envName fp2 Firing)
            user <- m6User ["view", "close", "ack"]
            let idsJson = cs (Aeson.encode [tshow a1, tshow a2])
            -- close requires acked/stalled: firing alerts are reported as skipped
            skipped <- runTool user "close_alerts" (argsV [("alert_ids", String idsJson), ("confirmed", Bool True)])
            skipped `shouldSatisfy` ("closed 0 of 2" `Text.isPrefixOf`)
            skipped `shouldSatisfy` ("skipped: status is firing" `Text.isInfixOf`)
            _ <- runTool user "ack_alerts" (argsV [("alert_ids", String idsJson), ("comment", String "bulk ack"), ("confirmed", Bool True)])
            plan <- runTool user "close_alerts" (args [("alert_ids", idsJson)])
            plan `shouldSatisfy` ("confirmation required" `Text.isInfixOf`)
            plan `shouldSatisfy` ("close 2 alert(s)" `Text.isInfixOf`)
            stillAcked <- fetch a1
            stillAcked.status `shouldBe` "ack"
            done <- runTool user "close_alerts" (argsV [("alert_ids", String idsJson), ("reason", String "cleanup"), ("confirmed", Bool True)])
            done `shouldSatisfy` ("closed 2 of 2 alert(s)" `Text.isPrefixOf`)
            done `shouldSatisfy` (tshow a1 `Text.isInfixOf`)
            closed1 <- fetch a1
            closed1.status `shouldBe` "closed"
            closed1.closeReason `shouldBe` Just "cleanup"
            -- idempotent-ish: already-closed rows are reported as skipped
            again <- runTool user "close_alerts" (argsV [("alert_ids", String idsJson), ("confirmed", Bool True)])
            again `shouldSatisfy` ("closed 0 of 2" `Text.isPrefixOf`)
            again `shouldSatisfy` ("skipped: already closed" `Text.isInfixOf`)
            -- all or nothing: an unknown id aborts before anything changes
            fp3 <- freshFingerprint
            Just a3 <- ingest source (testEventIn envName fp3 Firing)
            let mixedJson = cs (Aeson.encode [tshow a3, ("00000000-0000-0000-0000-000000000000" :: Text)])
            aborted <- runTool user "close_alerts" (argsV [("alert_ids", String mixedJson), ("confirmed", Bool True)])
            aborted `shouldSatisfy` ("invalid arguments" `Text.isPrefixOf`)
            untouched <- fetch a3
            untouched.status `shouldBe` "firing"
            -- ack_alerts
            fp4 <- freshFingerprint
            fp5 <- freshFingerprint
            Just a4 <- ingest source (testEventIn envName fp4 Firing)
            Just a5 <- ingest source (testEventIn envName fp5 Firing)
            acked <- runTool user "ack_alerts" (argsV [("alert_ids", String (cs (Aeson.encode [tshow a4, tshow a5]))), ("comment", String "bulk ack"), ("confirmed", Bool True)])
            acked `shouldSatisfy` ("acknowledged 2 of 2" `Text.isPrefixOf`)
            acked4 <- fetch a4
            acked4.status `shouldBe` "ack"
            acked4.ackComment `shouldBe` Just "bulk ack"
            badShape <- runTool user "close_alerts" (args [("alert_ids", "not json")])
            badShape `shouldSatisfy` ("invalid arguments" `Text.isPrefixOf`)
            emptyIds <- runTool user "close_alerts" (args [("alert_ids", "[]")])
            emptyIds `shouldSatisfy` ("invalid arguments" `Text.isPrefixOf`)

        it "close_by_match and ack_by_match accept glob shorthand and clause arrays" do
            source <- testSource
            token <- ("m" <>) . tshow <$> nextRandom
            let envName = "agent-match-" <> token
                web1 = "agent-match-" <> token <> "-web-01"
                web2 = "agent-match-" <> token <> "-web-02"
                db1 = "agent-match-" <> token <> "-db-01"
            let fire host = do
                    fp <- freshFingerprint
                    ingest source ((testEventIn envName fp Firing){host = Just host})
            Just m1 <- fire web1
            Just m2 <- fire web2
            Just m3 <- fire db1
            user <- m6User ["view", "close", "ack"]
            let shorthand = "{\"host\": \"agent-match-" <> token <> "-web-*\"}"
            -- close requires acked/stalled: ack the web hosts first
            ackPlan <- runTool user "ack_by_match" (args [("match", shorthand)])
            ackPlan `shouldSatisfy` ("plan: acknowledge 2 alert(s)" `Text.isPrefixOf`)
            ackDone <- runTool user "ack_by_match" (argsV [("match", String shorthand), ("comment", String "web rebuild"), ("confirmed", Bool True)])
            ackDone `shouldSatisfy` ("acknowledged 2 of 2" `Text.isPrefixOf`)
            acked1 <- fetch m1
            acked1.status `shouldBe` "ack"
            plan <- runTool user "close_by_match" (args [("match", shorthand)])
            plan `shouldSatisfy` ("plan: close 2 alert(s)" `Text.isPrefixOf`)
            plan `shouldSatisfy` (("field:host ~ \"agent-match-" <> token <> "-web-*\"") `Text.isInfixOf`)
            done <- runTool user "close_by_match" (argsV [("match", String shorthand), ("reason", String "web rebuild"), ("confirmed", Bool True)])
            done `shouldSatisfy` ("closed 2 of 2" `Text.isInfixOf`)
            closed1 <- fetch m1
            closed1.status `shouldBe` "closed"
            other <- fetch m3
            other.status `shouldBe` "firing"
            -- clause array form, passed as a raw JSON value (not a string)
            let clauseArray = Aeson.Array (Vector.fromList [object ["facet" .= ("field:host" :: Text), "op" .= ("~" :: Text), "value" .= ("agent-match-" <> token <> "-db-*" :: Text)]])
            ackDb <- runTool user "ack_by_match" (argsKV [("match", clauseArray), ("confirmed", Bool True)])
            ackDb `shouldSatisfy` ("acknowledged 1 of 1" `Text.isPrefixOf`)
            done2 <- runTool user "close_by_match" (argsKV [("match", clauseArray), ("confirmed", Bool True)])
            done2 `shouldSatisfy` ("closed 1 of 1" `Text.isInfixOf`)
            closed3 <- fetch m3
            closed3.status `shouldBe` "closed"
            -- raw JSON object value form works for the shorthand too
            let shorthandValue = object ["host" .= ("agent-match-" <> token <> "-web-*" :: Text)]
            acked <- runTool user "ack_by_match" (argsKV [("match", shorthandValue), ("confirmed", Bool True)])
            acked `shouldSatisfy` ("acknowledged 0 of 0" `Text.isPrefixOf`)
            badMatch <- runTool user "close_by_match" (args [("match", "{\"bogus facet\": 42}")])
            badMatch `shouldSatisfy` ("invalid match" `Text.isPrefixOf`)
            badFacet <- runTool user "close_by_match" (args [("match", "{\"nosuchfield\": \"x*\"}")])
            badFacet `shouldSatisfy` ("invalid match" `Text.isPrefixOf`)
            closed2 <- fetch m2
            closed2.status `shouldBe` "closed"

            user <- m6User ["view"]
            runTool user "nope" "{}" `shouldReturn` "unknown tool: nope"
            bad <- runTool user "search_alerts" "{]"
            bad `shouldSatisfy` ("invalid arguments for search_alerts" `Text.isPrefixOf`)
        it "normalizes empty arguments to an empty object (providers emit \"\")" do
            user <- m6User ["view"]
            out <- runTool user "list_dashboards" ""
            out `shouldSatisfy` (not . ("invalid arguments" `Text.isInfixOf`))
            out2 <- runTool user "get_dashboard_schema" "   "
            out2 `shouldSatisfy` ("Dashboard config is a JSON array" `Text.isPrefixOf`)
        it "names the required parameters in invalid-arguments errors" do
            user <- m6User ["view"]
            out <- runTool user "validate_dashboard" "{bad json"
            out `shouldSatisfy` ("invalid arguments for validate_dashboard" `Text.isPrefixOf`)
            out `shouldSatisfy` ("required: name, config" `Text.isInfixOf`)
        it "get_dashboard resolves by name and by id" do
            user <- m6User ["view"]
            suffix <- tshow <$> nextRandom
            let dashName = "probe-dash-" <> suffix
                config = "[{\"title\":\"x\",\"match\":[]}]"
            _ <- runTool user "create_dashboard" (argsV [("name", String dashName), ("config", String config), ("confirmed", Bool True)])
            byName <- runTool user "get_dashboard" (args [("name", dashName)])
            byName `shouldSatisfy` ("\"title\": \"x\"" `Text.isInfixOf`)
            byName `shouldSatisfy` ("is_default: false" `Text.isInfixOf`)
            dashId <- do
                rows <- query @Dashboard |> filterWhere (#name, dashName) |> fetch
                case rows of
                    (row : _) -> pure (tshow (get #id row))
                    [] -> error "probe dashboard missing"
            byId <- runTool user "get_dashboard" (args [("id", dashId)])
            byId `shouldSatisfy` (dashName `Text.isInfixOf`)
            missing <- runTool user "get_dashboard" "{}"
            missing `shouldSatisfy` ("invalid arguments" `Text.isPrefixOf`)

    describe "agent turn loop (regression: tool-call rounds)" do
        it "executes tool calls and answers instead of exhausting immediately" do
            user <- m6User ["view"]
            session <-
                newRecord @AgentSession
                    |> set #userId (get #id user)
                    |> createRecord
            _ <-
                newRecord @AgentMessage
                    |> set #sessionId (get #id session)
                    |> set #role_ ("user" :: Text)
                    |> set #content "hi"
                    |> createRecord
            counter <- newIORef (0 :: Int)
            let fakeComplete _ = do
                    step <- readIORef counter
                    modifyIORef' counter (+ 1)
                    pure $ Right case step of
                        0 ->
                            Llm.Completion
                                { content = ""
                                , tokensIn = Nothing
                                , tokensOut = Nothing
                                , toolCalls = [Llm.ToolCall "c1" "list_environments" "{}"]
                                }
                        _ ->
                            Llm.Completion
                                { content = "Here are the environments."
                                , tokensIn = Just 1
                                , tokensOut = Just 1
                                , toolCalls = []
                                }
            result <- runAgentTurnWith "fake" fakeComplete (get #id session)
            result `shouldBe` Right ()
            rows <-
                query @AgentMessage
                    |> filterWhere (#sessionId, get #id session)
                    |> orderByAsc #createdAt
                    |> fetch
            let contents = map (.content) rows
            contents `shouldSatisfy` any ("Here are the environments." `Text.isInfixOf`)
            contents `shouldSatisfy` (not . any ("tool-call budget" `Text.isInfixOf`))
            -- the executed round is persisted for the UI
            map (.toolCalls) rows `shouldSatisfy` any isJust
            -- and carries a per-round trace with duration + token counts
            let traces = [trace | Just trace <- map (.trace) rows]
            length traces `shouldSatisfy` (>= 2)
            let firstTrace = traces !! 0
            firstTrace `shouldSatisfy` (("duration_ms" `Text.isInfixOf`) . cs . Aeson.encode)
            firstTrace `shouldSatisfy` (("list_environments" `Text.isInfixOf`) . cs . Aeson.encode)

        it "spends the budget then forces a final tool-less answer" do
            user <- m6User ["view"]
            session <-
                newRecord @AgentSession
                    |> set #userId (get #id user)
                    |> createRecord
            _ <-
                newRecord @AgentMessage
                    |> set #sessionId (get #id session)
                    |> set #role_ ("user" :: Text)
                    |> set #content "hi"
                    |> createRecord
            calls <- newIORef (0 :: Int)
            let fakeComplete prompt = do
                    modifyIORef' calls (+ 1)
                    -- the exhaustion rescue passes an empty tool list; a sane
                    -- model answers then, an obstinate one keeps calling
                    pure $
                        Right
                            Llm.Completion
                                { content = ""
                                , tokensIn = Nothing
                                , tokensOut = Nothing
                                , toolCalls =
                                    [ Llm.ToolCall "c1" "list_environments" "{}"
                                    | not (null prompt.tools)
                                    ]
                                }
            result <- runAgentTurnWith "fake" fakeComplete (get #id session)
            result `shouldBe` Right ()
            rows <-
                query @AgentMessage
                    |> filterWhere (#sessionId, get #id session)
                    |> orderByAsc #createdAt
                    |> fetch
            let contents = map (.content) rows
            contents `shouldSatisfy` (not . any ("tool-call budget" `Text.isInfixOf`))
            -- maxToolRounds executed rounds, one boundary request whose calls
            -- are dropped, plus the rescue completion
            readIORef calls `shouldReturn` (maxToolRounds + 2)

    describe "agent budget (dedicated, admin-configured)" do
        it "defaults without a row and round-trips via saveAgentBudgetConfig" do
            void do
                sqlExecTyped [typedSql| DELETE FROM llm_agent_configs |]
            initial <- agentBudgetConfig
            initial `shouldBe` defaultAgentBudgetConfig
            saveAgentBudgetConfig AgentBudgetConfig{abcDailyTokenBudget = 12345, abcRatePerMinute = 3}
            saved <- agentBudgetConfig
            saved `shouldBe` AgentBudgetConfig{abcDailyTokenBudget = 12345, abcRatePerMinute = 3}
            rows <- sqlQueryTyped [typedSql| SELECT COUNT(*)::int AS n FROM llm_agent_configs |]
            rows `shouldBe` [1 :: Int]
        it "records agent usage under scope 'agent', leaving analysis counters alone" do
            void do
                sqlExecTyped [typedSql| DELETE FROM llm_budget_counters WHERE provider = 'fake-budget' |]
            user <- m6User ["view"]
            session <-
                newRecord @AgentSession
                    |> set #userId (get #id user)
                    |> createRecord
            _ <-
                newRecord @AgentMessage
                    |> set #sessionId (get #id session)
                    |> set #role_ ("user" :: Text)
                    |> set #content "hi"
                    |> createRecord
            counter <- newIORef (0 :: Int)
            let fakeComplete _ = do
                    step <- readIORef counter
                    modifyIORef' counter (+ 1)
                    pure $ Right case step of
                        0 -> Completion "" Nothing Nothing [Llm.ToolCall "c1" "list_environments" "{}"]
                        _ -> Completion "done." (Just 100) (Just 50) []
            result <- runAgentTurnWith "fake-budget" fakeComplete (get #id session)
            result `shouldBe` Right ()
            rows <-
                sqlQueryTyped
                    [typedSql|
                SELECT scope, tokens_in, tokens_out, requests
                FROM llm_budget_counters
                WHERE provider = 'fake-budget' AND day = CURRENT_DATE
            |]
            map (get #scope) rows `shouldBe` ["agent" :: Text]
            let (row : _) = rows
            (get #tokens_in row, get #tokens_out row, get #requests row)
                `shouldBe` (100 :: Int64, 50 :: Int64, 2 :: Int)

    describe "agent system prompt (internal_agent template)" do
        it "falls back to the built-in default when no template is seeded" do
            user <- m6User ["view"]
            resetAgentTemplates
            msg <-
                buildSystemMessage
                    AgentContext{acUser = user, acLanguage = "English", acSessionId = Nothing}
                    (Just (Aeson.object ["url" .= ("/dashboards/abc?x=1" :: Text), "title" .= ("Dash" :: Text)]))
            msg.content `shouldSatisfy` ("Halemans agent" `Text.isInfixOf`)
            msg.content `shouldSatisfy` (user.email `Text.isInfixOf`)
            -- regression: the fallback binds ALL slots — no literal
            -- {{current_page_*}} placeholders may reach the model
            msg.content `shouldSatisfy` (not . ("{{" `Text.isInfixOf`))
            msg.content `shouldSatisfy` ("/dashboards/abc?x=1" `Text.isInfixOf`)
            msg.content `shouldSatisfy` ("Dash" `Text.isInfixOf`)
        it "renders the active template with the per-turn bindings" do
            user <- m6User ["view"]
            resetAgentTemplates
            _ <-
                newRecord @LlmPromptTemplate
                    |> set #name internalAgentTemplateName
                    |> set #version (1 :: Int)
                    |> set #body ("Custom agent prompt for {{user_email}} in {{language}}. Page: {{page_context}}" :: Text)
                    |> set #active True
                    |> createRecord
            msg <-
                buildSystemMessage
                    AgentContext{acUser = user, acLanguage = "Russian", acSessionId = Nothing}
                    (Just (Aeson.object ["path" .= ("/alerts" :: Text)]))
            msg.content
                `shouldBe` "Custom agent prompt for "
                    <> user.email
                    <> " in Russian. Page: The user is currently looking at this page: "
                    <> cs (Aeson.encode (Aeson.object ["path" .= ("/alerts" :: Text)]))
        it "the seeded default body covers the structured page slots" do
            let slots = ["{{user_name}}", "{{user_email}}", "{{language}}", "{{current_page_url}}", "{{current_page_title}}"]
            [slot | slot <- slots, slot `Text.isInfixOf` defaultAgentTemplateBody] `shouldBe` slots
        it "renders current_page_url and current_page_title from the widget payload" do
            user <- m6User ["view"]
            resetAgentTemplates
            _ <-
                newRecord @LlmPromptTemplate
                    |> set #name internalAgentTemplateName
                    |> set #version (2 :: Int)
                    |> set #body ("Page: {{current_page_title}} at {{current_page_url}}" :: Text)
                    |> set #active True
                    |> createRecord
            msg <-
                buildSystemMessage
                    AgentContext{acUser = user, acLanguage = "English", acSessionId = Nothing}
                    (Just (Aeson.object ["url" .= ("/alerts?sort=title" :: Text), "title" .= ("Alerts" :: Text)]))
            msg.content `shouldBe` "Page: Alerts at /alerts?sort=title"
        it "falls back to the legacy path key for pre-widget-fix payloads" do
            user <- m6User ["view"]
            resetAgentTemplates
            _ <-
                newRecord @LlmPromptTemplate
                    |> set #name internalAgentTemplateName
                    |> set #version (2 :: Int)
                    |> set #body ("Page: {{current_page_title}} at {{current_page_url}}" :: Text)
                    |> set #active True
                    |> createRecord
            msg <-
                buildSystemMessage
                    AgentContext{acUser = user, acLanguage = "English", acSessionId = Nothing}
                    (Just (Aeson.object ["path" .= ("/dashboards" :: Text), "title" .= ("Dashboards" :: Text)]))
            msg.content `shouldBe` "Page: Dashboards at /dashboards"
        it "never leaks literal placeholders into the system message" do
            user <- m6User ["view"]
            resetAgentTemplates
            _ <-
                newRecord @LlmPromptTemplate
                    |> set #name internalAgentTemplateName
                    |> set #version (3 :: Int)
                    |> set #body defaultAgentTemplateBody
                    |> set #active True
                    |> createRecord
            msg <-
                buildSystemMessage
                    AgentContext{acUser = user, acLanguage = "English", acSessionId = Nothing}
                    (Just (Aeson.object ["url" .= ("/alerts?x=1" :: Text), "title" .= ("Alerts" :: Text)]))
            msg.content `shouldSatisfy` (not . ("{{" `Text.isInfixOf`))
            msg.content `shouldSatisfy` ("/alerts?x=1" `Text.isInfixOf`)

    describe "agent budget gate (agent cap + global cap)" do
        it "passes with fresh counters, blocks on the agent cap then the global cap" do
            -- Clear ALL counters: the global gate sums every scope/provider,
            -- so token spend from earlier examples (agent turns against the
            -- mock LLM) would otherwise leak into this example.
            void do
                sqlExecTyped [typedSql| DELETE FROM llm_budget_counters |]
            let agentConfig = AgentBudgetConfig{abcDailyTokenBudget = 100, abcRatePerMinute = 12}
                globalConfig = GlobalBudgetConfig{gbcDailyTokenBudget = 500, gbcRatePerMinute = 20}
            clear <- agentTurnGate "fake-gate" agentConfig globalConfig
            clear `shouldBe` Nothing
            -- spend agent tokens (scope 'agent'): 60 in + 60 out > cap 100
            void do
                sqlExecTyped
                    [typedSql|
                INSERT INTO llm_budget_counters (scope, provider, day, tokens_in, tokens_out, requests)
                VALUES ('agent', 'fake-gate', CURRENT_DATE, 60, 60, 1)
            |]
            agentBlocked <- agentTurnGate "fake-gate" agentConfig globalConfig
            agentBlocked `shouldBe` Just "agent"
            -- a different provider's agent usage does not block this provider
            otherProviderClear <- agentTurnGate "fake-gate-2" agentConfig globalConfig
            otherProviderClear `shouldBe` Nothing
            -- push the global total over 500 (add analysis scope tokens)
            void do
                sqlExecTyped
                    [typedSql|
                INSERT INTO llm_budget_counters (scope, provider, day, tokens_in, tokens_out, requests)
                VALUES ('analysis', 'fake-gate-2', CURRENT_DATE, 400, 100, 1)
            |]
            globalBlocked <- agentTurnGate "fake-gate-2" agentConfig GlobalBudgetConfig{gbcDailyTokenBudget = 500, gbcRatePerMinute = 20}
            globalBlocked `shouldBe` Just "global"

    describe "global budget config" do
        it "falls back to env defaults without a row and round-trips saves" do
            void do
                sqlExecTyped [typedSql| DELETE FROM llm_global_configs |]
            config0 <- globalBudgetConfig
            config0.gbcDailyTokenBudget `shouldSatisfy` (> 0)
            config0.gbcRatePerMinute `shouldSatisfy` (> 0)
            saveGlobalBudgetConfig GlobalBudgetConfig{gbcDailyTokenBudget = 777777, gbcRatePerMinute = 9}
            saved <- globalBudgetConfig
            saved `shouldBe` GlobalBudgetConfig{gbcDailyTokenBudget = 777777, gbcRatePerMinute = 9}

    describe "tool registry RBAC (real roles, not model discretion)" do
        it "executor hard-fails tools the acting user lacks the privilege for" do
            viewer <- m6User ["view"]
            out <- runTool viewer "ack_alert" (args [("alert_id", "00000000-0000-0000-0000-000000000000")])
            out `shouldBe` "forbidden: the acting user lacks the ack privilege"
            out2 <- runTool viewer "create_blackout" (args [("starts_at", "x"), ("ends_at", "y")])
            out2 `shouldBe` "forbidden: the acting user lacks the manage_blackouts privilege"
            sre <- m6User ["view", "ack", "close", "manage_blackouts"]
            bad <- runTool sre "ack_alert" (args [("alert_id", "not-a-uuid")])
            bad `shouldSatisfy` ("invalid arguments" `Text.isPrefixOf`)
        it "tool definitions are filtered to the caller's privileges" do
            let names privs = [name | Just (String name) <- map (functionField "name") (agentToolDefinitionsFor privs)]
            names ["view"] `shouldSatisfy` ("search_alerts" `elem`)
            names ["view"] `shouldSatisfy` (\xs -> "create_blackout" `Data.List.notElem` xs)
            names ["view"] `shouldSatisfy` (\xs -> "list_teams" `Data.List.notElem` xs)
            names ["view"] `shouldSatisfy` (\xs -> "list_notification_channels" `Data.List.notElem` xs)
            names ["manage_rules"] `shouldSatisfy` (\xs -> "list_notification_channels" `elem` xs)
            ["list_notification_channels", "create_notification_channel", "update_notification_channel", "delete_notification_channel"]
                `shouldSatisfy` all (`elem` names ["manage_rules"])
            requiredPrivilegeFor "create_notification_channel" `shouldBe` Just "manage_rules"
            requiredPrivilegeFor "update_team" `shouldBe` Just "manage_users"
            names ["view"] `shouldSatisfy` (\xs -> "list_mattermost_templates" `Data.List.notElem` xs)
            ["list_mattermost_templates", "update_mattermost_template"]
                `shouldSatisfy` all (`elem` names ["manage_rules"])
            requiredPrivilegeFor "update_mattermost_template" `shouldBe` Just "manage_rules"
            ["create_blackout", "list_teams", "ack_alert", "list_sources", "list_escalation_policies"]
                `shouldSatisfy` all (`elem` names ["view", "ack", "manage_blackouts", "manage_users", "manage_rules", "manage_sources", "close"])
            names ["admin"] `shouldSatisfy` (\xs -> "create_blackout" `Data.List.notElem` xs) -- raw "admin" is expanded by userPrivileges, not here
            requiredPrivilegeFor "create_blackout" `shouldBe` Just "manage_blackouts"
            requiredPrivilegeFor "get_profile" `shouldBe` Nothing
        it "admin role implies every tool (userPrivileges expands admin)" do
            adminUser <- m6User ["admin"]
            out <- runTool adminUser "list_roles" "{}"
            out `shouldSatisfy` ("admin" `Text.isInfixOf`)
        it "blackout two-phase flow with manage_blackouts" do
            source <- testSource
            envName <- ("rbac-env-" <>) . tshow <$> nextRandom
            fingerprint <- freshFingerprint
            _ <- ingest source (testEventIn envName fingerprint Firing)
            user <- m6User ["view", "manage_blackouts"]
            let args' confirmed =
                    argsV
                        [ ("env", String envName)
                        , ("starts_at", String "2026-09-24T18:00:00Z")
                        , ("ends_at", String "2026-09-24T20:00:00Z")
                        , ("reason", String "agent test")
                        , ("confirmed", Bool confirmed)
                        ]
            plan <- runTool user "create_blackout" (args' False)
            plan `shouldSatisfy` ("confirmation required" `Text.isInfixOf`)
            done <- runTool user "create_blackout" (args' True)
            done `shouldSatisfy` ("created blackout" `Text.isInfixOf`)
            listed <- runTool user "list_blackouts" "{}"
            listed `shouldSatisfy` (envName `Text.isInfixOf`)
        it "create_blackout accepts globs and still rejects unknown exact names" do
            user <- m6User ["view", "manage_blackouts"]
            let globArgs confirmed =
                    argsV
                        [ ("host", String "rbac-glob-host-*")
                        , ("starts_at", String "2026-09-24T18:00:00Z")
                        , ("ends_at", String "2026-09-24T20:00:00Z")
                        , ("confirmed", Bool confirmed)
                        ]
            globPlan <- runTool user "create_blackout" (globArgs False)
            globPlan `shouldSatisfy` ("confirmation required" `Text.isInfixOf`)
            globDone <- runTool user "create_blackout" (globArgs True)
            globDone `shouldSatisfy` ("created blackout" `Text.isInfixOf`)
            globRow <- query @Blackout |> filterWhere (#hostGlob, Just "rbac-glob-host-*") |> fetchOneOrNothing
            globRow `shouldSatisfy` isJust
            let Just created = globRow
            created.hostId `shouldBe` Nothing
            listed <- runTool user "list_blackouts" "{}"
            listed `shouldSatisfy` ("rbac-glob-host-*" `Text.isInfixOf`)
            bad <-
                runTool
                    user
                    "create_blackout"
                    ( args
                        [ ("host", "rbac-no-such-host-exact")
                        , ("starts_at", "2026-09-24T18:00:00Z")
                        , ("ends_at", "2026-09-24T20:00:00Z")
                        ]
                    )
            bad `shouldSatisfy` ("invalid scope" `Text.isPrefixOf`)
        it "create_blackout accepts a title glob (single plan-and-confirm)" do
            user <- m6User ["view", "manage_blackouts"]
            let titleArgs confirmed =
                    argsV
                        [ ("title", String "rbac-title-glob-*")
                        , ("starts_at", String "2026-09-24T18:00:00Z")
                        , ("ends_at", String "2026-09-24T20:00:00Z")
                        , ("confirmed", Bool confirmed)
                        ]
            plan <- runTool user "create_blackout" (titleArgs False)
            plan `shouldSatisfy` ("confirmation required" `Text.isInfixOf`)
            plan `shouldSatisfy` ("title glob: rbac-title-glob-*" `Text.isInfixOf`)
            done <- runTool user "create_blackout" (titleArgs True)
            done `shouldSatisfy` ("created blackout" `Text.isInfixOf`)
            titleRow <- query @Blackout |> filterWhere (#titleGlob, Just "rbac-title-glob-*") |> fetchOneOrNothing
            titleRow `shouldSatisfy` isJust
            noScope <- runTool user "create_blackout" (args [("starts_at", "2026-09-24T18:00:00Z"), ("ends_at", "2026-09-24T20:00:00Z")])
            noScope `shouldSatisfy` ("invalid scope" `Text.isPrefixOf`)
        it "create_blackout treats an omitted ends_at as open-ended" do
            user <- m6User ["view", "manage_blackouts"]
            let openArgs confirmed =
                    argsV
                        [ ("title", String "rbac-open-ended-*")
                        , ("starts_at", String "2026-09-24T18:00:00Z")
                        , ("confirmed", Bool confirmed)
                        ]
            plan <- runTool user "create_blackout" (openArgs False)
            plan `shouldSatisfy` ("confirmation required" `Text.isInfixOf`)
            plan `shouldSatisfy` ("forever (open-ended)" `Text.isInfixOf`)
            done <- runTool user "create_blackout" (openArgs True)
            done `shouldSatisfy` ("created blackout" `Text.isInfixOf`)
            Just row <- query @Blackout |> filterWhere (#titleGlob, Just "rbac-open-ended-*") |> fetchOneOrNothing
            row.endsAt `shouldBe` openEndedBlackoutEndsAt
            listed <- runTool user "list_blackouts" "{}"
            listed `shouldSatisfy` ("rbac-open-ended-*" `Text.isInfixOf`)
            listed `shouldSatisfy` (" to forever" `Text.isInfixOf`)
            -- an explicit ends_at still wins
            let boundedArgs =
                    argsV
                        [ ("title", String "rbac-bounded-*")
                        , ("starts_at", String "2026-09-24T18:00:00Z")
                        , ("ends_at", String "2026-09-24T20:00:00Z")
                        , ("confirmed", Bool True)
                        ]
            _ <- runTool user "create_blackout" boundedArgs
            Just bounded <- query @Blackout |> filterWhere (#titleGlob, Just "rbac-bounded-*") |> fetchOneOrNothing
            bounded.endsAt `shouldSatisfy` (/= openEndedBlackoutEndsAt)
            bad <- runTool user "create_blackout" (argsV [("title", String "rbac-bad-time-*"), ("starts_at", String "2026-09-24T18:00:00Z"), ("ends_at", String "not-a-time"), ("confirmed", Bool True)])
            bad `shouldSatisfy` ("invalid time" `Text.isPrefixOf`)

    describe "notification channel and team mattermost tools" do
        it "notification channels: create, list, update token_env, delete blocked by rule reference" do
            suffix <- tshow <$> nextRandom
            user <- m6User ["view", "manage_rules"]
            let name = "agent-chan-" <> suffix
                tokenEnv = "AGENT_MM_TOKEN_" <> Text.map (\c -> if c == '-' then '_' else c) suffix
                createArgs confirmed =
                    argsV
                        [ ("name", String name)
                        , ("type", String "mattermost")
                        , ("base_url", String "http://mm-agent.example")
                        , ("token_env", String tokenEnv)
                        , ("confirmed", Bool confirmed)
                        ]
            badType <- runTool user "create_notification_channel" (args [("name", name), ("type", "pagerduty")])
            badType `shouldSatisfy` ("invalid: type" `Text.isPrefixOf`)
            plan <- runTool user "create_notification_channel" (createArgs False)
            plan `shouldSatisfy` ("confirmation required" `Text.isInfixOf`)
            created <- runTool user "create_notification_channel" (createArgs True)
            created `shouldSatisfy` ("created notification channel" `Text.isPrefixOf`)
            listed <- runTool user "list_notification_channels" "{}"
            listed `shouldSatisfy` (name `Text.isInfixOf`)
            listed `shouldSatisfy` (tokenEnv `Text.isInfixOf`)
            -- update: rewrite the token env, then remove it with ""
            updateResult <- runTool user "update_notification_channel" (argsV [("name", String name), ("token_env", String (tokenEnv <> "_ROTATED")), ("confirmed", Bool True)])
            updateResult `shouldSatisfy` ("updated notification channel" `Text.isPrefixOf`)
            rotated <- query @NotificationChannel |> filterWhere (#name, name) |> fetchOne
            channelTokenEnv rotated `shouldBe` tokenEnv <> "_ROTATED"
            _ <- runTool user "update_notification_channel" (argsV [("name", String name), ("token_env", String ""), ("confirmed", Bool True)])
            cleared <- query @NotificationChannel |> filterWhere (#name, name) |> fetchOne
            channelTokenEnv cleared `shouldBe` ""
            -- delete is blocked while a rule references the channel
            _ <-
                runTool
                    user
                    "create_notification_rule"
                    ( argsV
                        [ ("name", String ("agent-chan-rule-" <> suffix))
                        , ("match", String ("{\"fields\":{\"env\":\"agent-chan-env-" <> suffix <> "\"}}"))
                        , ("channel", String name)
                        , ("confirmed", Bool True)
                        ]
                    )
            blocked <- runTool user "delete_notification_channel" (argsV [("name", String name), ("confirmed", Bool True)])
            blocked `shouldSatisfy` ("still reference" `Text.isInfixOf`)
            rule <- query @NotificationRule |> filterWhere (#name, "agent-chan-rule-" <> suffix) |> fetchOne
            deleteRecord rule
            deletePlan <- runTool user "delete_notification_channel" (argsV [("name", String name), ("confirmed", Bool False)])
            deletePlan `shouldSatisfy` ("confirmation required" `Text.isInfixOf`)
            deleted <- runTool user "delete_notification_channel" (argsV [("name", String name), ("confirmed", Bool True)])
            deleted `shouldBe` "deleted notification channel \"" <> name <> "\""
        it "create_team and update_team manage the mattermost destination in team defaults" do
            suffix <- tshow <$> nextRandom
            user <- m6User ["view", "manage_users"]
            let name = "agent-team-" <> suffix
                createArgs confirmed =
                    argsV
                        [ ("name", String name)
                        , ("description", String "agent test team")
                        , ("mattermost_team", String "sre")
                        , ("mattermost_channel", String "oncall")
                        , ("confirmed", Bool confirmed)
                        ]
            plan <- runTool user "create_team" (createArgs False)
            plan `shouldSatisfy` ("mattermost=sre/oncall" `Text.isInfixOf`)
            _ <- runTool user "create_team" (createArgs True)
            team <- query @Team |> filterWhere (#name, name) |> fetchOne
            mattermostFieldValues (get #defaults team) `shouldBe` ("sre", "oncall")
            -- update: change the channel, then remove the whole destination
            _ <- runTool user "update_team" (argsV [("name", String name), ("mattermost_channel", String "alerts"), ("confirmed", Bool True)])
            updated <- query @Team |> filterWhere (#name, name) |> fetchOne
            mattermostFieldValues (get #defaults updated) `shouldBe` ("sre", "alerts")
            _ <- runTool user "update_team" (argsV [("name", String name), ("mattermost_team", String ""), ("mattermost_channel", String ""), ("confirmed", Bool True)])
            cleared <- query @Team |> filterWhere (#name, name) |> fetchOne
            mattermostFieldValues (get #defaults cleared) `shouldBe` ("", "")
            get #defaults cleared `shouldBe` Aeson.object []

    describe "mattermost message template tools" do
        it "list shows the active body; update appends a version and activates it" do
            user <- m6User ["view", "manage_rules"]
            -- Clean slate (idempotent reruns) and cleanup afterwards: a
            -- customized mattermost_root row would leak into the Mattermost
            -- delivery specs sharing this database.
            deleteMattermostRootTemplates
            flip finally deleteMattermostRootTemplates do
                _ <-
                    newRecord @LlmPromptTemplate
                        |> set #name "mattermost_root"
                        |> set #version (1 :: Int)
                        |> set #body "[{{alert.state}}] {{alert.title}}"
                        |> set #active True
                        |> createRecord
                listed <- runTool user "list_mattermost_templates" "{}"
                listed `shouldSatisfy` ("mattermost_root v1 (active)" `Text.isInfixOf`)
                listed `shouldSatisfy` ("[{{alert.state}}]" `Text.isInfixOf`)
                let newBody = "ALERT {{alert.severity}}: {{alert.title}}"
                    updateArgs confirmed =
                        argsV
                            [ ("name", String "mattermost_root")
                            , ("body", String newBody)
                            , ("confirmed", Bool confirmed)
                            ]
                invalid <- runTool user "update_mattermost_template" (args [("name", "alert_enrichment"), ("body", "x")])
                invalid `shouldSatisfy` ("invalid: name must be one of" `Text.isPrefixOf`)
                -- An EMPTY body is allowed on purpose: empty mattermost_root =
                -- no header line on the card. It goes through the same
                -- two-phase plan/apply flow.
                emptyPlan <- runTool user "update_mattermost_template" (args [("name", "mattermost_root"), ("body", "")])
                emptyPlan `shouldSatisfy` ("confirmation required" `Text.isInfixOf`)
                plan <- runTool user "update_mattermost_template" (updateArgs False)
                plan `shouldSatisfy` ("confirmation required" `Text.isInfixOf`)
                applied <- runTool user "update_mattermost_template" (updateArgs True)
                applied `shouldSatisfy` ("updated mattermost template" `Text.isPrefixOf`)
                activeRow <- query @LlmPromptTemplate |> filterWhere (#name, "mattermost_root") |> filterWhere (#active, True) |> fetchOne
                get #body activeRow `shouldBe` newBody
                previous <- query @LlmPromptTemplate |> filterWhere (#name, "mattermost_root") |> filterWhere (#body, "[{{alert.state}}] {{alert.title}}") |> fetchOne
                get #active previous `shouldBe` False

    describe "turn traces (agent observability)" do
        it "explain_last_turn renders the current session's per-round trace" do
            user <- m6User ["view"]
            session <-
                newRecord @AgentSession
                    |> set #userId (get #id user)
                    |> createRecord
            _ <-
                newRecord @AgentMessage
                    |> set #sessionId (get #id session)
                    |> set #role_ ("user" :: Text)
                    |> set #content "hi"
                    |> createRecord
            counter <- newIORef (0 :: Int)
            let fakeComplete _ = do
                    step <- readIORef counter
                    modifyIORef' counter (+ 1)
                    pure $ Right case step of
                        0 -> Completion "" Nothing Nothing [Llm.ToolCall "c1" "list_environments" "{}"]
                        _ -> Completion "done." (Just 10) (Just 5) []
            result <- runAgentTurnWith "fake-trace" fakeComplete (get #id session)
            result `shouldBe` Right ()
            let context =
                    AgentContext
                        { acUser = user
                        , acLanguage = "English"
                        , acSessionId = Just (get #id session)
                        }
            out <- executeAgentTool context (Llm.ToolCall "x" "explain_last_turn" "{}")
            out `shouldSatisfy` ("turn trace for session" `Text.isInfixOf`)
            out `shouldSatisfy` ("list_environments" `Text.isInfixOf`)
            out `shouldSatisfy` ("round 1:" `Text.isInfixOf`)
            out `shouldSatisfy` ("round 2:" `Text.isInfixOf`)
            out `shouldSatisfy` ("tokens" `Text.isInfixOf`)
        it "explain_last_turn without a session context says so" do
            user <- m6User ["view"]
            let context = AgentContext{acUser = user, acLanguage = "English", acSessionId = Nothing}
            out <- executeAgentTool context (Llm.ToolCall "x" "explain_last_turn" "{}")
            out `shouldSatisfy` ("needs a chat session context" `Text.isInfixOf`)

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
            fmap length toolNames `shouldBe` Just 22
            -- privilege filter, not just a count: view tools present,
            -- manage_blackouts/close/ack-only tools hidden
            fmap ("search_alerts" `elem`) toolNames `shouldBe` Just True
            fmap ("get_alert" `elem`) toolNames `shouldBe` Just True
            fmap ("create_blackout" `elem`) toolNames `shouldBe` Just False
            fmap ("close_alerts" `elem`) toolNames `shouldBe` Just False
            fmap ("close_by_match" `elem`) toolNames `shouldBe` Just False
            fmap ("ack_alerts" `elem`) toolNames `shouldBe` Just False
            fmap ("ack_by_match" `elem`) toolNames `shouldBe` Just False

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
        let context = AgentContext{acUser = user, acLanguage = "English", acSessionId = Nothing}
        executeAgentTool context (Llm.ToolCall name name arguments)
    resetAgentTemplates = void do
        sqlExecTyped
            [typedSql| DELETE FROM llm_prompt_templates WHERE name = 'internal_agent' |]
    deleteMattermostRootTemplates = void do
        sqlExecTyped
            [typedSql| DELETE FROM llm_prompt_templates WHERE name = 'mattermost_root' |]
    args pairs = argsV [(key, String value) | (key, value) <- pairs]
    argsV pairs = cs (Aeson.encode (Aeson.object [(Key.fromText key, value) | (key, value) <- pairs]))
    argsKV pairs = cs (Aeson.encode (Aeson.object pairs))
    testMcpConfig user = McpConfig{mcpUser = user, mcpLanguage = "English", mcpPrivileges = ["view"]}
    functionField key tool = field "function" tool >>= field key
    field key (Object obj) = KeyMap.lookup key obj
    field _ _ = Nothing
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
