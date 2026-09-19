module Test.Integration.MetricChartSpec (spec) where

import Application.Connector.GrafanaMetrics (MetricSeries (..))
import Application.Service.MetricChart (fetchAlertMetricSeries, metricWindowFor, seriesChartSvg)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.UUID.V4 (nextRandom)
import Generated.Types
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.ModelSupport (ModelContext, newRecord)
import IHP.Prelude
import System.Environment (lookupEnv)
import Test.Hspec
import Test.Integration.Setup (integrationSource)

-- End-to-end against the mock grafana (nix/mocks/mock_grafana.py): alert
-- generatorURL -> provisioning rule -> ds/query frames -> SVG. The mock is
-- wired in nix/checks.nix (MOCK_GRAFANA_URL, GRAFANA_TOKEN).

spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = do
    describe "grafana metric chart" do
        it "fetches the alert rule series from the mock and renders SVG" do
            baseUrl <- mockUrl
            suffix <- tshow <$> nextRandom
            source <- integrationSource "grafana" ("itest-grafana-metrics-" <> suffix) baseUrl (Aeson.object ["tokenEnv" Aeson..= ("GRAFANA_TOKEN" :: Text)])
            now <- getCurrentTime
            alert <-
                newRecord @Alert
                    |> set #sourceId (Just source.id)
                    |> set #fingerprint "grafana-mock-rule-cpu"
                    |> set #title "Mock CPU saturation"
                    |> set #sourceUrl (Just (baseUrl <> "/alerting/grafana/mock-rule-cpu/view?orgId=1"))
                    |> set #startedAt (Just (addUTCTime (-1800) now))
                    |> createRecord
            result <- fetchAlertMetricSeries source alert (metricWindowFor source alert now)
            case result of
                Left err -> expectationFailure (Text.unpack err)
                Right series -> do
                    length series `shouldBe` 2
                    map seriesName series `shouldBe` ["instance-1", "instance-2"]
                    let svg = seriesChartSvg series
                    Text.isInfixOf "<svg" svg `shouldBe` True
                    Text.isInfixOf "instance-1" svg `shouldBe` True
        it "returns Left for an unknown rule uid" do
            baseUrl <- mockUrl
            suffix <- tshow <$> nextRandom
            source <- integrationSource "grafana" ("itest-grafana-metrics-404-" <> suffix) baseUrl (Aeson.object ["tokenEnv" Aeson..= ("GRAFANA_TOKEN" :: Text)])
            now <- getCurrentTime
            alert <-
                newRecord @Alert
                    |> set #sourceId (Just source.id)
                    |> set #fingerprint "grafana-mock-unknown"
                    |> set #title "Unknown rule"
                    |> set #sourceUrl (Just (baseUrl <> "/alerting/grafana/no-such-rule/view"))
                    |> set #startedAt (Just (addUTCTime (-1800) now))
                    |> createRecord
            result <- fetchAlertMetricSeries source alert (metricWindowFor source alert now)
            result `shouldSatisfy` isLeft
  where
    mockUrl :: IO Text
    mockUrl = cs . fromMaybe "http://127.0.0.1:18086" <$> lookupEnv "MOCK_GRAFANA_URL"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
