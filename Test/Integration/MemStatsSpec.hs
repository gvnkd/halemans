module Test.Integration.MemStatsSpec (spec) where

import Application.Service.Chart (LineSeries (..))
import Application.Service.MemStats
import Data.UUID.V4 (nextRandom)
import Generated.Types
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.ModelSupport
import IHP.Prelude
import Test.Hspec

-- mem_stats_snapshots round-trip: the worker writes one row per telemetry
-- call, the admin "Worker memory" panel reads it back. The label is unique
-- per run so dirty-DB reruns never see another run's rows in the series
-- averages.

spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = describe "MemStats snapshots" do
    it "records a snapshot and reads it back as the latest per label" do
        uuid <- nextRandom
        let label = "itest-memstats-" <> tshow uuid
            snapshot =
                MemSnapshot
                    { msLiveBytes = 3 * 1024 * 1024
                    , msMaxLiveBytes = 5 * 1024 * 1024
                    , msInUseBytes = 8 * 1024 * 1024
                    , msAllocatedBytes = 2 * 1024 * 1024 * 1024
                    , msGcs = 12
                    , msMajorGcs = 3
                    , msGcElapsedNs = 0
                    }
        recordMemSnapshot label snapshot
        latest <- latestMemStats
        row <- expectOne [r | r <- latest, mstLabel r == label]
        mstLiveBytes row `shouldBe` msLiveBytes snapshot
        mstMaxLiveBytes row `shouldBe` msMaxLiveBytes snapshot
        mstInUseBytes row `shouldBe` msInUseBytes snapshot
        mstAllocatedBytes row `shouldBe` msAllocatedBytes snapshot
        mstGcs row `shouldBe` 12
        mstMajorGcs row `shouldBe` 3
        series <- memStatsSeries 60 3600
        ours <- expectOne [s | s <- series, seriesName s == label]
        -- The label is unique per run, so exactly one bucket point exists
        -- and its averaged live-bytes equal the recorded snapshot.
        map snd (seriesPoints ours) `shouldBe` [3 * 1024 * 1024]
  where
    expectOne :: [a] -> IO a
    expectOne [single] = pure single
    expectOne others = expectationFailure (cs ("expected exactly one element, got " <> tshow (length others))) >> error "unreachable"
