module Test.CmdbSpec where

import Test.Hspec
import IHP.Prelude
import Text.Read (readMaybe)
import qualified Data.Text as Text
import Application.Service.Cmdb

atTime :: Text -> UTCTime
atTime raw = fromMaybe (error "bad utc literal") (readMaybe (cs raw))

spec :: Spec
spec = describe "Application.Service.Cmdb" do
    describe "apiUrl" do
        it "strips a trailing slash from the base url" do
            apiUrl (CmdbConfig "https://confluence.example.com/confluence/" "t" "DEV" ["DEV"]) "/rest/api/content/search"
                `shouldBe` "https://confluence.example.com/confluence/rest/api/content/search"

    describe "cqlForSubject" do
        it "scopes to space and page type with a text match" do
            cqlForSubject ["DEV"] "dev-host-01" `shouldBe` "space = \"DEV\" AND text ~ \"dev-host-01\" AND type = page"
        it "OR-es several configured spaces" do
            cqlForSubject ["DEV", "OPS"] "dev-host-01" `shouldBe` "space in (\"DEV\", \"OPS\") AND text ~ \"dev-host-01\" AND type = page"
        it "drops the space clause when no spaces are configured" do
            cqlForSubject [] "dev-host-01" `shouldBe` "text ~ \"dev-host-01\" AND type = page"

    describe "pickBestPage" do
        let pageA = ConfPage "1" "dev-host-01 notes" "" ""
            pageB = ConfPage "2" "dev-host-01" "" ""
        it "prefers an exact title match over the first hit" do
            pickBestPage "dev-host-01" [pageA, pageB] `shouldBe` Just pageB
        it "is case-insensitive on titles" do
            pickBestPage "DEV-HOST-01" [pageB] `shouldBe` Just pageB
        it "falls back to the first hit" do
            pickBestPage "other" [pageA, pageB] `shouldBe` Just pageA
        it "returns Nothing on no hits" do
            pickBestPage "other" [] `shouldBe` Nothing

    describe "excerptFromHtml" do
        it "strips tags and collapses whitespace" do
            excerptFromHtml 2000 "<p>Owner: <b>team-sre</b>.</p>\n<p>Runbook soon.</p>"
                `shouldBe` "Owner: team-sre . Runbook soon."
        it "truncates to the budget" do
            let long = mconcat (replicate 100 "lorem ipsum ")
            excerptFromHtml 50 long `shouldSatisfy` \excerpt -> Text.length excerpt <= 51

    describe "isFresh" do
        let now = atTime "2026-09-04 12:00:00 UTC"
        it "is fresh within the ttl" do
            isFresh now (atTime "2026-09-04 06:00:01 UTC") (6 * 3600) `shouldBe` True
        it "is stale past the ttl" do
            isFresh now (atTime "2026-09-04 05:59:59 UTC") (6 * 3600) `shouldBe` False
