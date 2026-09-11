module Test.LlmSpec where

import Test.Hspec
import IHP.Prelude
import Text.Read (readMaybe)
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as Key
import Application.Service.Llm
import Application.Service.Llm.Output
import Application.Service.Llm.Prompt
import Application.Service.Llm.Budget
import Application.Service.Llm.AutoAnalyze
import Application.Service.Llm.ToolCache (isFailureText, freshEnough)

atTime :: Text -> UTCTime
atTime raw = fromMaybe (error "bad utc literal") (readMaybe (cs raw))

spec :: Spec
spec = describe "Milestone 4 LLM services" do
    describe "Llm.apiUrl" do
        it "strips a trailing slash from the endpoint" do
            apiUrl (LlmProviderConfig "default" "https://llm.example.com/" "m" Nothing False) "/v1/models"
                `shouldBe` "https://llm.example.com/v1/models"
        it "works when the endpoint has no trailing slash" do
            apiUrl (LlmProviderConfig "default" "https://llm.example.com" "m" Nothing False) "/v1/chat/completions"
                `shouldBe` "https://llm.example.com/v1/chat/completions"

    describe "Llm.chatCompletionPayload" do
        it "omits null optional request fields" do
            let encoded = cs (Aeson.encode (chatCompletionPayload
                    (LlmProviderConfig "default" "http://llm.example" "m" Nothing False)
                    (Prompt [userMessage "hi"] []))) :: Text
            "messages" `Text.isInfixOf` encoded `shouldBe` True
            "tool_call_id" `Text.isInfixOf` encoded `shouldBe` False
            "tool_calls" `Text.isInfixOf` encoded `shouldBe` False
            "\"tools\"" `Text.isInfixOf` encoded `shouldBe` False

    describe "Output.parseCompletionOutput" do
        let fenced = Text.intercalate "\n"
                [ "The disk is nearly full."
                , ""
                , "```json"
                , "{\"probable_cause\": \"disk full\", \"confidence\": 0.9, \"suggested_actions\": [\"free space\"], \"references\": [\"runbook-1\"]}"
                , "```"
                ]
        it "splits markdown from a well-formed fenced json block" do
            let parsed = parseCompletionOutput fenced
            parsed.markdown `shouldBe` "The disk is nearly full."
            fieldOf "probable_cause" parsed `shouldBe` Just "disk full"
        it "returns markdown-only when the fence is missing" do
            let parsed = parseCompletionOutput "just prose, no fence"
            parsed.markdown `shouldBe` "just prose, no fence"
            parsed.structured `shouldBe` Nothing
        it "returns markdown-only when the json is malformed" do
            let broken = "Analysis text.\n\n```json\n{not json\n```"
                parsed = parseCompletionOutput broken
            parsed.structured `shouldBe` Nothing
            parsed.markdown `shouldSatisfy` \md -> "Analysis text" `Text.isInfixOf` md
        it "returns markdown-only when the fence holds a non-object" do
            let parsed = parseCompletionOutput "text\n```json\n[1,2]\n```"
            parsed.structured `shouldBe` Nothing
        it "ignores prose after the fence" do
            let parsed = parseCompletionOutput (fenced <> "\nTrailing prose.")
            parsed.markdown `shouldBe` "The disk is nearly full."

    describe "Prompt.renderTemplate" do
        it "replaces named placeholders" do
            renderTemplate "Alert: {{alert.title}} on {{alert.host}}"
                [("alert.title", "CPU hot"), ("alert.host", "web-1")]
                `shouldBe` "Alert: CPU hot on web-1"
        it "leaves unknown placeholders untouched" do
            renderTemplate "{{alert.title}} {{unknown}}" [("alert.title", "x")]
                `shouldBe` "x {{unknown}}"
        it "is stable across repeated placeholders" do
            renderTemplate "{{events}}|{{events}}" [("events", "e")]
                `shouldBe` "e|e"

    describe "Prompt.fitPrompt" do
        let template = Text.intercalate "\n"
                [ "Title: {{alert.title}}"
                , "Desc: {{alert.description}}"
                , "Events: {{events}}"
                , "Similar: {{similar_alerts}}"
                , "CMDB: {{cmdb_excerpt}}"
                , "Jira: {{jira_links}}"
                ]
            inputs = emptyInputs
                { piTitle = "t"
                , piDescription = Text.replicate 400 "d"
                , piEvents = Text.replicate 400 "e"
                , piSimilarAlerts = Text.replicate 400 "s"
                , piCmdbExcerpt = Text.replicate 400 "c"
                , piJiraLinks = Text.replicate 400 "j"
                }
        it "leaves prompts within budget untouched" do
            let small = emptyInputs { piTitle = "t", piDescription = "short" }
            fitPrompt 100 template small `shouldBe` renderTemplate template (bindingsFor small)
        it "truncates in the documented order (similar_alerts first)" do
            let rendered = fitPrompt 120 template inputs
            Text.length rendered `shouldSatisfy` (<= charBudgetForTokens 120)
            "Title: t" `Text.isInfixOf` rendered `shouldBe` True
        it "never exceeds the hard cap even when everything must go" do
            let huge = inputs { piTitle = Text.replicate 5000 "t" }
            Text.length (fitPrompt 10 template huge) `shouldSatisfy` (<= charBudgetForTokens 10)
        it "marks truncation points and keeps the title" do
            let rendered = fitPrompt 120 template inputs
            truncationMarker `Text.isInfixOf` rendered `shouldBe` True
            "Title: t" `Text.isInfixOf` rendered `shouldBe` True
        it "truncates similar_alerts before events" do
            let narrow = inputs
                    { piDescription = "short", piCmdbExcerpt = "short", piJiraLinks = "short"
                    , piSimilarAlerts = Text.replicate 200 "s"
                    , piEvents = Text.replicate 200 "e"
                    }
                rendered = fitPrompt 60 template narrow
            -- the budget forces similar_alerts out entirely while events survive
            "ss" `Text.isInfixOf` rendered `shouldBe` False
            "ee" `Text.isInfixOf` rendered `shouldBe` True

    describe "Prompt.sha256Hex" do
        it "is stable for identical inputs" do
            sha256Hex "same prompt" `shouldBe` sha256Hex "same prompt"
        it "differs for different inputs" do
            sha256Hex "a" `shouldNotBe` sha256Hex "b"
        it "produces 64 hex chars" do
            Text.length (sha256Hex "x") `shouldBe` 64

    describe "Budget.budgetExceeded" do
        it "is over budget at the cap" do
            budgetExceeded 100 60 40 `shouldBe` True
        it "is under budget below the cap" do
            budgetExceeded 100 60 39 `shouldBe` False

    describe "Budget.rateLimitDelaySeconds" do
        let now = atTime "2026-09-05 12:00:00 UTC"
        it "allows requests under the limit" do
            rateLimitDelaySeconds 2 now [addUTCTime (-10) now] `shouldBe` Nothing
        it "delays until the oldest in-window request leaves the window" do
            let recent = [addUTCTime (-10) now, addUTCTime (-30) now]
            rateLimitDelaySeconds 2 now recent `shouldBe` Just 30
        it "ignores requests older than the window" do
            let old = [addUTCTime (-120) now, addUTCTime (-90) now]
            rateLimitDelaySeconds 2 now old `shouldBe` Nothing

    describe "Budget.withinDedupeWindow" do
        let now = atTime "2026-09-05 12:00:00 UTC"
        it "is inside the window" do
            withinDedupeWindow 3600 now (addUTCTime (-3599) now) `shouldBe` True
        it "is outside the window" do
            withinDedupeWindow 3600 now (addUTCTime (-3601) now) `shouldBe` False

    describe "AutoAnalyze.allowedByRules (milestone 10 §5)" do
        it "defaults allow firing/ack of any severity" do
            allowedByRules defaultRules "firing" "info" `shouldBe` True
            allowedByRules defaultRules "ack" "critical" `shouldBe` True
        it "defaults exclude stalled, resolved and closed" do
            allowedByRules defaultRules "stalled" "critical" `shouldBe` False
            allowedByRules defaultRules "resolved" "critical" `shouldBe` False
            allowedByRules defaultRules "closed" "critical" `shouldBe` False
        it "a disabled gate allows nothing" do
            allowedByRules defaultRules { aaEnabled = False } "firing" "critical" `shouldBe` False
        it "custom severity/status lists are honoured" do
            let rules = AutoAnalyzeRules True ["firing", "resolved"] ["critical", "high"]
            allowedByRules rules "resolved" "high" `shouldBe` True
            allowedByRules rules "resolved" "info" `shouldBe` False
            allowedByRules rules "ack" "critical" `shouldBe` False

    describe "ToolCache (milestone 10 §6)" do
        let now = atTime "2026-09-05 12:00:00 UTC"
        it "freshEnough within and past the ttl" do
            freshEnough 300 now (addUTCTime (-299) now) `shouldBe` True
            freshEnough 300 now (addUTCTime (-301) now) `shouldBe` False
        it "failure texts are not cacheable" do
            isFailureText "jira search failed: timeout" `shouldBe` True
            isFailureText "cmdb not configured" `shouldBe` True
            isFailureText "assets lookup failed: boom" `shouldBe` True
        it "legitimate results and empty negatives are cacheable" do
            isFailureText "- DEV-1 something [Open]" `shouldBe` False
            isFailureText "no jira tickets found" `shouldBe` False
            isFailureText "no cmdb pages found" `shouldBe` False

fieldOf :: Text -> ParsedOutput -> Maybe Text
fieldOf key parsed = do
    structured <- parsed.structured
    parseMaybe (Aeson.withObject "result" (\o -> o .: Key.fromText key)) structured
