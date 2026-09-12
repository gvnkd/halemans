module Test.TimelineSpec where

import Application.Service.Timeline
import Generated.Types
import IHP.ModelSupport (newRecord)
import IHP.Prelude
import Test.Hspec

event :: Text -> AlertEvent
event kind = newRecord @AlertEvent |> set #kind kind

summarize :: [TimelineGroup] -> [(Text, Int)]
summarize = map (\group -> (get #kind group.tgLatest, group.tgCount))

spec :: Spec
spec = describe "Application.Service.Timeline" do
    describe "timelineHiddenKind" do
        it "hides internal error kinds" do
            forEach ["enrichment_failed", "writeback_failed", "llm_failed", "llm_skipped"] \kind ->
                timelineHiddenKind kind `shouldBe` True
        it "keeps state-change kinds" do
            forEach ["created", "repeated", "resolved", "ack", "unack", "closed", "stalled", "severity_upgraded", "notified", "external", "comment"] \kind ->
                timelineHiddenKind kind `shouldBe` False

    describe "groupTimeline (ascending)" do
        it "aggregates consecutive same-kind runs" do
            summarize (groupTimeline (map event ["created", "repeated", "repeated", "repeated", "resolved"]))
                `shouldBe` [("created", 1), ("repeated", 3), ("resolved", 1)]
        it "hidden kinds never reach the timeline but do not merge runs" do
            summarize (groupTimeline (map event ["repeated", "enrichment_failed", "repeated", "llm_failed", "resolved"]))
                `shouldBe` [("repeated", 2), ("resolved", 1)]
        it "anchor is the oldest event of the run, latest the newest" do
            let t1 = UTCTime (fromGregorian 2026 1 1) 0
                t2 = UTCTime (fromGregorian 2026 1 1) 60
                older = event "repeated" |> set #createdAt t1
                newer = event "repeated" |> set #createdAt t2
            case groupTimeline [older, newer] of
                [group] -> do
                    get #createdAt group.tgAnchor `shouldBe` t1
                    get #createdAt group.tgLatest `shouldBe` t2
                    group.tgCount `shouldBe` 2
                _ -> expectationFailure "expected one group"
        it "empty input" do
            length (groupTimeline []) `shouldBe` 0

    describe "headTimelineGroup (descending)" do
        it "aggregates only the leading run" do
            let eventsDesc = map event ["repeated", "repeated", "resolved", "repeated"]
            fmap (\group -> (get #kind group.tgLatest, group.tgCount)) (headTimelineGroup eventsDesc)
                `shouldBe` Just ("repeated", 2)
        it "skips leading hidden events" do
            let eventsDesc = map event ["enrichment_failed", "resolved", "repeated"]
            fmap (\group -> (get #kind group.tgLatest, group.tgCount)) (headTimelineGroup eventsDesc)
                `shouldBe` Just ("resolved", 1)
        it "Nothing when everything is hidden" do
            isNothing (headTimelineGroup (map event ["llm_failed", "llm_skipped"])) `shouldBe` True
