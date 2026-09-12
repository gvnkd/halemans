module Test.MarkdownSpec where

import Application.Helper.View (renderMarkdownText)
import qualified Data.Text as Text
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Markdown rendering (LLM analysis card)" do
    it "renders headings, emphasis and lists" do
        let html = renderMarkdownText "## Cause\n\n**disk full**\n\n- one\n- two\n"
        "<h2>Cause</h2>" `Text.isInfixOf` html `shouldBe` True
        "<strong>disk full</strong>" `Text.isInfixOf` html `shouldBe` True
        "<li>one</li>" `Text.isInfixOf` html `shouldBe` True

    it "renders fenced code blocks" do
        let html = renderMarkdownText "```\ndf -h\n```\n"
        "<code>" `Text.isInfixOf` html `shouldBe` True
        "df -h" `Text.isInfixOf` html `shouldBe` True

    it "suppresses raw html from model output" do
        let html = renderMarkdownText "hello <script>alert(1)</script>"
        "<script>" `Text.isInfixOf` html `shouldBe` False

    it "suppresses dangerous link urls" do
        let html = renderMarkdownText "[click](javascript:alert(1))"
        "javascript:" `Text.isInfixOf` html `shouldBe` False
