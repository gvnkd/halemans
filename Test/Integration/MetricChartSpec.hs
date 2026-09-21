module Test.Integration.MetricChartSpec (spec) where

import Application.Connector.GrafanaMetrics (MetricSeries (..))
import Application.Service.MetricChart (fetchAlertMetricSeries, metricWindowFor, seriesChartSvg)
import Control.Lens ((^.))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as Text
import Data.UUID.V4 (nextRandom)
import Generated.Types
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.ModelSupport (ModelContext, newRecord)
import IHP.Prelude
import qualified Network.Wreq as Wreq
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
    describe "zabbix metric chart" do
        it "fetches trigger item history from the mock and renders SVG" do
            baseUrl <- zabbixMockUrl
            suffix <- tshow <$> nextRandom
            source <- integrationSource "zabbix" ("itest-zabbix-metrics-" <> suffix) baseUrl (Aeson.object ["tokenEnv" Aeson..= ("ZABBIX_TOKEN" :: Text)])
            now <- getCurrentTime
            alert <-
                newRecord @Alert
                    |> set #sourceId (Just source.id)
                    |> set #fingerprint "zabbix:trigger:42"
                    |> set #title "Mock CPU load high"
                    |> set #sourceUrl (Just (baseUrl <> "/tr_events.php?triggerid=42&eventid=1"))
                    |> set #startedAt (Just (addUTCTime (-1800) now))
                    |> createRecord
            result <- fetchAlertMetricSeries source alert (metricWindowFor source alert now)
            case result of
                Left err -> expectationFailure (Text.unpack err)
                Right series -> do
                    map seriesName series `shouldBe` ["CPU load", "Memory utilization (%)"]
                    let svg = seriesChartSvg series
                    Text.isInfixOf "<svg" svg `shouldBe` True
                    Text.isInfixOf "CPU load" svg `shouldBe` True
        it "serves a second view from the cache without re-hitting history.get" do
            baseUrl <- zabbixMockUrl
            suffix <- tshow <$> nextRandom
            source <- integrationSource "zabbix" ("itest-zabbix-metrics-cache-" <> suffix) baseUrl (Aeson.object ["tokenEnv" Aeson..= ("ZABBIX_TOKEN" :: Text)])
            now <- getCurrentTime
            alert <-
                newRecord @Alert
                    |> set #sourceId (Just source.id)
                    |> set #fingerprint "zabbix:trigger:42"
                    |> set #title "Mock CPU load high"
                    |> set #sourceUrl (Just (baseUrl <> "/tr_events.php?triggerid=42&eventid=1"))
                    |> set #startedAt (Just (addUTCTime (-1800) now))
                    |> createRecord
            before <- zabbixHistoryRequests
            result1 <- fetchAlertMetricSeries source alert (metricWindowFor source alert now)
            isRight result1 `shouldBe` True
            afterFirst <- zabbixHistoryRequests
            (afterFirst > before) `shouldBe` True
            now2 <- getCurrentTime
            result2 <- fetchAlertMetricSeries source alert (metricWindowFor source alert now2)
            isRight result2 `shouldBe` True
            afterSecond <- zabbixHistoryRequests
            afterSecond `shouldBe` afterFirst
        it "returns Left for a trigger with no numeric items" do
            baseUrl <- zabbixMockUrl
            suffix <- tshow <$> nextRandom
            source <- integrationSource "zabbix" ("itest-zabbix-metrics-text-" <> suffix) baseUrl (Aeson.object ["tokenEnv" Aeson..= ("ZABBIX_TOKEN" :: Text)])
            now <- getCurrentTime
            alert <-
                newRecord @Alert
                    |> set #sourceId (Just source.id)
                    |> set #fingerprint "zabbix:trigger:99"
                    |> set #title "Mock text trigger"
                    |> set #startedAt (Just (addUTCTime (-1800) now))
                    |> createRecord
            result <- fetchAlertMetricSeries source alert (metricWindowFor source alert now)
            result `shouldBe` Left "No numeric metric for this trigger"
        it "returns Left when the token env var is missing" do
            baseUrl <- zabbixMockUrl
            suffix <- tshow <$> nextRandom
            source <- integrationSource "zabbix" ("itest-zabbix-metrics-notoken-" <> suffix) baseUrl (Aeson.object ["tokenEnv" Aeson..= ("HALEMANS_NO_SUCH_TOKEN_ENV" :: Text)])
            now <- getCurrentTime
            alert <-
                newRecord @Alert
                    |> set #sourceId (Just source.id)
                    |> set #fingerprint "zabbix:trigger:42"
                    |> set #title "Mock CPU load high"
                    |> set #startedAt (Just (addUTCTime (-1800) now))
                    |> createRecord
            result <- fetchAlertMetricSeries source alert (metricWindowFor source alert now)
            result `shouldBe` Left "No Zabbix token configured (sources.config.tokenEnv)"
  where
    mockUrl :: IO Text
    mockUrl = cs . fromMaybe "http://127.0.0.1:18086" <$> lookupEnv "MOCK_GRAFANA_URL"

    zabbixMockUrl :: IO Text
    zabbixMockUrl = cs . fromMaybe "http://127.0.0.1:18087" <$> lookupEnv "MOCK_ZABBIX_URL"

    zabbixHistoryRequests :: IO Int
    zabbixHistoryRequests = do
        baseUrl <- zabbixMockUrl
        response <- Wreq.get (cs (baseUrl <> "/debug/stats"))
        let parsed = Aeson.decode (response ^. Wreq.responseBody) :: Maybe Aeson.Value
        pure (fromMaybe 0 (parsed >>= parseMaybe (Aeson.withObject "stats" (\o -> o Aeson..: "history_requests"))))

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
