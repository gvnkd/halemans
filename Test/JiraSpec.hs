module Test.JiraSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport (newRecord)
import Generated.Types hiding (JiraConfig)
import Application.Service.Jira
import Application.Service.Jira.Related (parseRelevantKeys)

spec :: Spec
spec = describe "Application.Service.Jira" do
    describe "apiUrl" do
        it "joins the rest path on the configured api version" do
            apiUrl (JiraConfig "https://jira.example.com" "t" "DEV" ["DEV"] "3") "/search"
                `shouldBe` "https://jira.example.com/rest/api/3/search"
        it "strips a trailing slash from the base url" do
            apiUrl (JiraConfig "https://jira.example.com/" "t" "DEV" ["DEV"] "2") "/myself"
                `shouldBe` "https://jira.example.com/rest/api/2/myself"

    describe "jqlForAlert" do
        it "matches host labels and check text, open tickets only" do
            let alert = newRecord @Alert
                    |> set #host (Just "dev-host-01")
                    |> set #checkName (Just "halemans test trigger")
            jqlForAlert ["DEV"] alert `shouldBe`
                "project = DEV AND statusCategory != Done AND (labels ~ dev-host-01 OR text ~ \"halemans test trigger\")"
        it "falls back to a title text search without a subject" do
            let alert = newRecord @Alert
                    |> set #title "disk full"
            jqlForAlert ["OPS"] alert `shouldBe`
                "project = OPS AND statusCategory != Done AND (text ~ \"disk full\")"
        it "OR-es several configured projects" do
            let alert = newRecord @Alert
                    |> set #host (Just "dev-host-01")
            jqlForAlert ["DEV", "OPS"] alert `shouldBe`
                "project in (DEV, OPS) AND statusCategory != Done AND (labels ~ dev-host-01)"
        it "drops the project clause when no projects are configured" do
            let alert = newRecord @Alert
                    |> set #host (Just "dev-host-01")
            jqlForAlert [] alert `shouldBe`
                "statusCategory != Done AND (labels ~ dev-host-01)"

    describe "parseRelevantKeys" do
        it "reads the fenced json verdict and keeps candidate keys only" do
            let raw = "Some prose\n\n```json\n{\"relevant\": [\"DEV-1\", \"BOGUS-9\", \"OPS-2\"]}\n```\n"
            parseRelevantKeys ["DEV-1", "OPS-2", "OPS-3"] raw `shouldBe` Just ["DEV-1", "OPS-2"]
        it "accepts an empty verdict (nothing relevant)" do
            parseRelevantKeys ["DEV-1"] "```json\n{\"relevant\": []}\n```" `shouldBe` Just []
        it "returns Nothing without a parseable verdict" do
            parseRelevantKeys ["DEV-1"] "no json here" `shouldBe` Nothing
