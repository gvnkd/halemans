module Test.PollZabbixSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport (newRecord)
import Generated.Types
import Application.Job.PollZabbix (initialCursor, initialHistoryDays)
import Data.Aeson (object, (.=))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

spec :: Spec
spec = describe "Application.Job.PollZabbix" do
    describe "initialHistoryDays" do
        it "defaults to 1 when the key is absent" do
            initialHistoryDays (newRecord @Source) `shouldBe` 1
        it "reads initialHistoryDays from source config" do
            let source = newRecord @Source |> set #config (object ["initialHistoryDays" .= (7 :: Int)])
            initialHistoryDays source `shouldBe` 7

    describe "initialCursor" do
        it "uses lastSyncCursor when present" do
            let cursorTime = addUTCTime (-3600) now
                source = newRecord @Source |> set #lastSyncCursor (Just cursorTime)
            initialCursor now source `shouldBe` floor (utcTimeToPOSIXSeconds cursorTime)
        it "bounds the first poll to one day back by default" do
            initialCursor now (newRecord @Source) `shouldBe` nowPosix - 86400
        it "honors initialHistoryDays on the first poll" do
            let source = newRecord @Source |> set #config (object ["initialHistoryDays" .= (30 :: Int)])
            initialCursor now source `shouldBe` nowPosix - 30 * 86400
        it "ignores initialHistoryDays once a cursor exists" do
            let cursorTime = addUTCTime (-3600) now
                source = newRecord @Source
                    |> set #lastSyncCursor (Just cursorTime)
                    |> set #config (object ["initialHistoryDays" .= (30 :: Int)])
            initialCursor now source `shouldBe` floor (utcTimeToPOSIXSeconds cursorTime)
  where
    now = UTCTime (fromGregorian 2026 6 1) 43200
    nowPosix = floor (utcTimeToPOSIXSeconds now) :: Integer
