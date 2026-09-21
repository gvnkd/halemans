module Test.FlappingSpec where

import Application.Service.Flapping
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.UUID as UUID
import IHP.ModelSupport (Id' (..))
import IHP.Prelude
import Test.Hspec
import Web.View.Flapping.Index (formatRate)

spec :: Spec
spec = do
    describe "Application.Service.Flapping" do
        let params = FlapParams{minFlaps = 3, maxGapSeconds = 1800, windowSeconds = 3600}

        describe "detectFlapping" do
            it "ignores fingerprints without edges" do
                detectFlapping params [(testSubject, [])] `shouldBe` []

            it "rejects episodes below minFlaps" do
                let edges = [loud 0, quiet 100, loud 200, quiet 300, loud 400]
                detectFlapping params [(testSubject, edges)] `shouldBe` []

            it "ignores occurrence bumps (loud runs) — no quiet edge, no flap" do
                let edges = [loud 0, loud 100, loud 200, loud 300]
                detectFlapping params [(testSubject, edges)] `shouldBe` []

            it "reports a single flapping episode with gap stats and mttr" do
                let edges = [loud 0, quiet 100, loud 200, quiet 300, loud 400, quiet 500, loud 600]
                case detectFlapping params [(testSubject, edges)] of
                    [report] -> do
                        report.flapCount `shouldBe` 3
                        report.medianGapSeconds `shouldBe` 100
                        report.p90GapSeconds `shouldBe` 100
                        report.minGapSeconds `shouldBe` 100
                        report.maxGapSecondsObs `shouldBe` 100
                        report.mttrSeconds `shouldBe` Just 100
                        report.flapRatePerHour `shouldBe` 3
                        report.activeFrom `shouldBe` t 0
                        report.lastFlapAt `shouldBe` t 600
                    other -> expectationFailure ("expected one report, got " ++ cs (show (length other)))

            it "splits episodes on gaps above maxGapSeconds and keeps only qualifying ones" do
                let flaps3 = [loud 0, quiet 100, loud 200, quiet 300, loud 400, quiet 500, loud 600]
                    flaps2 = [quiet 700, loud 2501, quiet 2601, loud 2701, quiet 2801, loud 2901]
                case detectFlapping params [(testSubject, flaps3 ++ flaps2)] of
                    [report] -> do
                        report.flapCount `shouldBe` 3
                        report.lastFlapAt `shouldBe` t 600
                    other -> expectationFailure ("expected one report, got " ++ cs (show (length other)))

            it "collapses consecutive loud edges (row start right after refire counts once)" do
                let edges = [loud 0, quiet 100, loud 150, loud 160, quiet 200, loud 300, quiet 400, loud 500]
                case detectFlapping params [(testSubject, edges)] of
                    [report] -> report.flapCount `shouldBe` 3
                    other -> expectationFailure ("expected one report, got " ++ cs (show (length other)))

            it "handles timelines starting mid-quiet (no leading loud edge)" do
                let edges = [quiet 50, loud 100, quiet 150, loud 200, quiet 250, loud 300]
                case detectFlapping params [(testSubject, edges)] of
                    [report] -> do
                        report.flapCount `shouldBe` 3
                        report.mttrSeconds `shouldBe` Just 50
                        report.activeFrom `shouldBe` t 50
                    other -> expectationFailure ("expected one report, got " ++ cs (show (length other)))

            it "sorts reports by flap count descending" do
                let busy = [loud 0, quiet 10, loud 20, quiet 30, loud 40, quiet 50, loud 60, quiet 70, loud 80]
                    calm = [loud 0, quiet 100, loud 200, quiet 300, loud 400, quiet 500, loud 600]
                    reports = detectFlapping params [(testSubject, calm), (testSubject, busy)]
                map flapCount reports `shouldBe` [4, 3]

        describe "formatRate" do
            it "never renders scientific notation" do
                formatRate 0.01 `shouldBe` "0.01"
                formatRate 0.05 `shouldBe` "0.05"

            it "drops trailing zeros" do
                formatRate 3 `shouldBe` "3"
                formatRate 0.3 `shouldBe` "0.3"
                formatRate 2.25 `shouldBe` "2.25"

t :: Double -> UTCTime
t seconds = posixSecondsToUTCTime (realToFrac seconds)

loud :: Double -> FlapEdge
loud seconds = FlapEdge (t seconds) Loud

quiet :: Double -> FlapEdge
quiet seconds = FlapEdge (t seconds) Quiet

testSubject :: FlapSubject
testSubject =
    FlapSubject
        { fingerprint = "test:fingerprint"
        , latestAlertId = Id UUID.nil
        , title = "test alert"
        , severity = "warning"
        , effectiveEnv = Just "dev"
        , host = Just "host-01"
        , sourceName = Just "test-source"
        }
