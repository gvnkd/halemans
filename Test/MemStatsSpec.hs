module Test.MemStatsSpec (spec) where

import Application.Service.MemStats
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Application.Service.MemStats" do
    describe "parseMemStatsFlag" do
        it "defaults to enabled" do
            parseMemStatsFlag Nothing `shouldBe` True
            parseMemStatsFlag (Just "1") `shouldBe` True
            parseMemStatsFlag (Just "") `shouldBe` True
        it "accepts the disable spellings case-insensitively" do
            parseMemStatsFlag (Just "0") `shouldBe` False
            parseMemStatsFlag (Just "FALSE") `shouldBe` False
            parseMemStatsFlag (Just " off ") `shouldBe` False
            parseMemStatsFlag (Just "No") `shouldBe` False
    describe "formatMemStats" do
        it "renders the snapshot counters with units" do
            let snapshot =
                    MemSnapshot
                        { msLiveBytes = 3 * 1024 * 1024
                        , msMaxLiveBytes = 5 * 1024 * 1024
                        , msInUseBytes = 8 * 1024 * 1024
                        , msAllocatedBytes = 2 * 1024 * 1024 * 1024
                        , msGcs = 12
                        , msMajorGcs = 3
                        , msGcElapsedNs = 2_500_000_000
                        }
            formatMemStats "poll-zabbix" snapshot
                `shouldBe` "memstats poll-zabbix live=3MiB maxLive=5MiB inUse=8MiB allocTotal=2GiB gcs=12/3 gcWall=2500ms"
    describe "formatBytes" do
        it "picks the unit by magnitude" do
            formatBytes 1024 `shouldBe` "1KiB"
            formatBytes 2048 `shouldBe` "2KiB"
            formatBytes (3 * 1024 * 1024) `shouldBe` "3MiB"
            formatBytes (2 * 1024 * 1024 * 1024) `shouldBe` "2GiB"
