module Test.SourceHealthSpec where

import Application.Service.SourceHealth
import Data.Aeson (object, (.=))
import Generated.Types
import IHP.ModelSupport (newRecord)
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Application.Service.SourceHealth" do
    describe "backoffSeconds" do
        it "returns the base interval for zero or negative failures" do
            backoffSeconds 30 0 `shouldBe` 30
            backoffSeconds 30 (-1) `shouldBe` 30
        it "doubles with each consecutive failure" do
            backoffSeconds 30 1 `shouldBe` 60
            backoffSeconds 30 2 `shouldBe` 120
            backoffSeconds 30 3 `shouldBe` 240
        it "caps at 30 minutes" do
            backoffSeconds 30 10 `shouldBe` 30 * 60
            backoffSeconds 1800 5 `shouldBe` 30 * 60

    describe "jitteredBackoff" do
        it "stays within ±10% of the base backoff" do
            forM_ ["seed-a", "seed-b", "seed-c", "seed-d"] \seed -> do
                let base = backoffSeconds 30 3
                    jittered = jitteredBackoff seed 30 3
                jittered `shouldSatisfy` (\v -> v >= base - base `div` 10 && v <= base + base `div` 10)
        it "never drops below one second" do
            forM_ ["x", "y", "z"] \seed ->
                jitteredBackoff seed 1 0 `shouldSatisfy` (>= 1)
        it "is deterministic per seed" do
            jitteredBackoff "same" 30 2 `shouldBe` jitteredBackoff "same" 30 2

    describe "pollDue" do
        it "is due when next_poll_at is unset" do
            let source = newRecord @Source
            pollDue epoch source `shouldBe` True
        it "is due when next_poll_at is in the past" do
            let source = newRecord @Source |> set #nextPollAt (Just (addUTCTime (-60) epoch))
            pollDue epoch source `shouldBe` True
        it "is not due when next_poll_at is in the future" do
            let source = newRecord @Source |> set #nextPollAt (Just (addUTCTime 60 epoch))
            pollDue epoch source `shouldBe` False

    describe "halemansHostName" do
        it "returns the non-empty local hostname" do
            name <- halemansHostName
            name `shouldSatisfy` (/= "")

    describe "expectedIntervalSeconds" do
        it "is unset by default (silence detection disabled)" do
            expectedIntervalSeconds (newRecord @Source) `shouldBe` Nothing
        it "reads expectedIntervalSeconds from source config" do
            let source = newRecord @Source |> set #config (object ["expectedIntervalSeconds" .= (60 :: Int)])
            expectedIntervalSeconds source `shouldBe` Just 60
  where
    epoch = UTCTime (fromGregorian 2026 1 1) 0
