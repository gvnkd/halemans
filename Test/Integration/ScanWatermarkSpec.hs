module Test.Integration.ScanWatermarkSpec (spec) where

import Application.Job.PollZabbix (recordScanWatermarks, scanReplayWatermarks)
import Data.Aeson (object)
import qualified Data.Map.Strict as Map
import Data.UUID.V4 (nextRandom)
import Generated.Types
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.ModelSupport
import IHP.Prelude
import Test.Hspec
import Test.Integration.Setup (integrationSource)

-- Persistent scan watermarks: the missing-problem scan must remember the max
-- event clock it already fetched per trigger even when nothing ingested
-- (no local alert row to key on). Trigger ids are unique per run so dirty-DB
-- reruns never see another run's rows.

spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = describe "zabbix scan watermarks" do
    it "persists per-trigger clocks and only advances" do
        uuid <- nextRandom
        source <- integrationSource "zabbix" ("itest-scan-wm-" <> tshow uuid) "http://127.0.0.1:1" (object [])
        let t1 = "itest-t1-" <> tshow uuid
            t2 = "itest-t2-" <> tshow uuid
            t3 = "itest-t3-" <> tshow uuid
        recordScanWatermarks source (Map.fromList [(t1, 1000), (t2, 2000)])
        -- A lower clock must not regress the stored watermark.
        recordScanWatermarks source (Map.fromList [(t1, 900), (t2, 2500)])
        watermarks <- scanReplayWatermarks source [t1, t2, t3]
        Map.lookup ("zabbix:trigger:" <> t1) watermarks `shouldBe` Just 1000
        Map.lookup ("zabbix:trigger:" <> t2) watermarks `shouldBe` Just 2500
        Map.lookup ("zabbix:trigger:" <> t3) watermarks `shouldBe` Nothing
