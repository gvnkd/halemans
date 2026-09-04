module Test.JiraSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport (newRecord)
import Generated.Types
import Application.Service.Jira

spec :: Spec
spec = describe "Application.Service.Jira" do
    describe "jqlForAlert" do
        it "matches host labels and check text, open tickets only" do
            let alert = newRecord @Alert
                    |> set #host (Just "dev-host-01")
                    |> set #checkName (Just "halemans test trigger")
            jqlForAlert "DEV" alert `shouldBe`
                "project = DEV AND statusCategory != Done AND (labels ~ dev-host-01 OR text ~ \"halemans test trigger\")"
        it "falls back to a title text search without a subject" do
            let alert = newRecord @Alert
                    |> set #title "disk full"
            jqlForAlert "OPS" alert `shouldBe`
                "project = OPS AND statusCategory != Done AND (text ~ \"disk full\")"
