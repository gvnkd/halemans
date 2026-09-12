module Test.ReportsSpec where

import Application.Service.Reports
import qualified Data.Text as Text
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Application.Service.Reports" do
    describe "chart rendering" do
        it "renders the severity chart as inline SVG" do
            let svg = severityChartSvg [("critical", 5), ("warning", 12)]
            svg `shouldSatisfy` Text.isInfixOf "<svg"
            svg `shouldSatisfy` Text.isInfixOf "critical"
            svg `shouldSatisfy` Text.isInfixOf "12"

        it "renders the env chart as inline SVG" do
            let svg = envChartSvg [("prod", 7)]
            svg `shouldSatisfy` Text.isInfixOf "<svg"
            svg `shouldSatisfy` Text.isInfixOf "prod"

        it "renders the volume chart as inline SVG" do
            let svg = volumeChartSvg [("09-10", 3), ("09-11", 9)]
            svg `shouldSatisfy` Text.isInfixOf "<svg"
            svg `shouldSatisfy` Text.isInfixOf "09-11"

        it "renders the mttr chart as inline SVG" do
            let svg = mttrChartSvg [("critical", 300)]
            svg `shouldSatisfy` Text.isInfixOf "<svg"

        it "renders a placeholder for empty data" do
            let svg = severityChartSvg []
            svg `shouldSatisfy` Text.isInfixOf "<svg"
            svg `shouldSatisfy` Text.isInfixOf "no data"

    describe "formatDuration" do
        it "formats seconds below 90 as seconds" do
            formatDuration 42 `shouldBe` "42s"

        it "formats minutes" do
            formatDuration 600 `shouldBe` "10m"

        it "formats hours" do
            formatDuration 7200 `shouldBe` "2h"

    describe "severityCssClass" do
        it "maps known severities to their theme classes" do
            severityCssClass "critical" `shouldBe` "chart-sev-critical"
            severityCssClass "Warning" `shouldBe` "chart-sev-warning"
            severityCssClass "disaster" `shouldBe` "chart-sev-critical"
            severityCssClass "average" `shouldBe` "chart-sev-high"
            severityCssClass "information" `shouldBe` "chart-sev-info"

        it "falls back to the neutral class for unknown severities" do
            severityCssClass "notice" `shouldBe` "chart-sev-other"

    describe "truncateLabel" do
        it "keeps short labels" do
            truncateLabel 10 "prod" `shouldBe` "prod"

        it "truncates long labels with an ellipsis" do
            truncateLabel 10 "production-eu-west" `shouldBe` "productio…"
