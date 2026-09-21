module Test.ReportsSpec where

import Application.Service.Reports
import qualified Data.Text as Text
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Application.Service.Reports" do
    describe "volumeChartSvg" do
        it "renders a stacked bar per bucket with the total above it" do
            let svg = volumeChartSvg [("09-10", [("critical", 3)]), ("09-11", [("critical", 4), ("warning", 5)])]
            svg `shouldSatisfy` Text.isInfixOf "<svg"
            -- fluid width: fills the card at any viewport, never clipped
            svg `shouldSatisfy` Text.isInfixOf "width=\"100%\""
            (Text.isInfixOf "width=\"1140" svg) `shouldBe` False
            -- shapes are visible: diagrams' default fill is transparent
            -- (fill-opacity=0); the widget must pin fill-opacity=1
            svg `shouldSatisfy` Text.isInfixOf "fill-opacity=\"1.0\""
            svg `shouldSatisfy` Text.isInfixOf "09-11"
            svg `shouldSatisfy` Text.isInfixOf "chart-sev-critical"
            svg `shouldSatisfy` Text.isInfixOf "chart-sev-warning-dim"
            -- value label above a stacked bar shows the bucket total
            svg `shouldSatisfy` Text.isInfixOf ">9<"
            -- hover tooltip lists every severity in the bar plus the total
            svg `shouldSatisfy` Text.isInfixOf "<title>critical: 4\nwarning: 5\ntotal: 9</title>"
            -- z-order: grid lines paint BEHIND the bars (diagrams-svg would
            -- otherwise paint stroked segments over filled shapes)
            let (beforeBars, _) = Text.breakOn "class=\"chart-sev-critical" svg
            Text.isInfixOf "class=\"chart-grid" beforeBars `shouldBe` True

        it "renders empty buckets as baseline ticks" do
            let svg = volumeChartSvg [("09-10", []), ("09-11", [("info", 2)])]
            svg `shouldSatisfy` Text.isInfixOf "chart-tick"

        it "renders a placeholder when there is no data" do
            let svg = volumeChartSvg []
            svg `shouldSatisfy` Text.isInfixOf "<svg"
            svg `shouldSatisfy` Text.isInfixOf "no data"

    describe "formatPct" do
        it "formats a percentage with one decimal" do
            formatPct 66.6667 `shouldBe` "66.7%"
            formatPct 100 `shouldBe` "100.0%"

    describe "formatDuration" do
        it "formats seconds below 90 as seconds" do
            formatDuration 42 `shouldBe` "42s"

        it "formats minutes" do
            formatDuration 600 `shouldBe` "10m"

        it "formats hours" do
            formatDuration 7200 `shouldBe` "2h"

    describe "formatDurationHm" do
        it "formats seconds below 90 as seconds" do
            formatDurationHm 42 `shouldBe` "42s"

        it "formats sub-hour values as minutes" do
            formatDurationHm 3480 `shouldBe` "58m"

        it "formats hours with a zero-padded minutes remainder" do
            formatDurationHm 29040 `shouldBe` "8h 04m"

        it "rounds to the nearest minute" do
            formatDurationHm 4980 `shouldBe` "1h 23m"

    describe "formatHours" do
        it "formats seconds as decimal hours" do
            formatHours 15120 `shouldBe` "4.2"

    describe "severityCssClass" do
        it "maps known severities to their theme classes" do
            severityCssClass "critical" `shouldBe` "chart-sev-critical"
            severityCssClass "Warning" `shouldBe` "chart-sev-warning-dim"
            severityCssClass "disaster" `shouldBe` "chart-sev-critical"
            severityCssClass "average" `shouldBe` "chart-sev-high"
            severityCssClass "information" `shouldBe` "chart-sev-info"

        it "falls back to the neutral class for unknown severities" do
            severityCssClass "notice" `shouldBe` "chart-sev-other"

    describe "severityFillClass" do
        it "maps known severities to their bar fill classes" do
            severityFillClass "critical" `shouldBe` "sev-critical"
            severityFillClass "warning" `shouldBe` "sev-warning"
            severityFillClass "info" `shouldBe` "sev-info"

        it "falls back to the neutral fill for unknown severities" do
            severityFillClass "notice" `shouldBe` "sev-other"

    describe "truncateLabel" do
        it "keeps short labels" do
            truncateLabel 10 "prod" `shouldBe` "prod"

        it "truncates long labels with an ellipsis" do
            truncateLabel 10 "production-eu-west" `shouldBe` "productio…"
