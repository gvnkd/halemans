module Test.MetricChartSpec (spec) where

import Application.Connector.GrafanaMetrics (
    MetricSeries (..),
    ruleQueryFromRule,
    ruleUidFromSourceUrl,
    seriesFromResponse,
 )
import qualified Application.Service.MetricChart as MetricChart
import Data.Aeson (Value, object, (.=))
import qualified Data.Text as Text
import Data.Time (UTCTime)
import qualified Data.Time.Format as TimeFormat
import Generated.Types
import IHP.ModelSupport (newRecord)
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = do
    describe "ruleUidFromSourceUrl" do
        it "extracts the uid from a grafana generatorURL" do
            ruleUidFromSourceUrl "https://grafana.example/alerting/grafana/abc123/view?orgId=1&edit=1"
                `shouldBe` Just "abc123"
        it "handles URLs without a query string" do
            ruleUidFromSourceUrl "http://127.0.0.1:3000/alerting/grafana/dev-cpu-sim/view"
                `shouldBe` Just "dev-cpu-sim"
        it "returns Nothing for foreign URLs" do
            ruleUidFromSourceUrl "https://zabbix.example/tr_events.php?triggerid=42"
                `shouldBe` Nothing

    describe "ruleQueryFromRule" do
        it "picks the first query with an expr" do
            let rule =
                    object
                        [ "data"
                            .= [ object ["refId" .= ("B" :: Text), "model" .= object ["instant" .= True]]
                               , object
                                    [ "refId" .= ("A" :: Text)
                                    , "datasourceUid" .= ("ds-1" :: Text)
                                    , "model" .= object ["expr" .= ("up == 0" :: Text), "range" .= True]
                                    ]
                               ]
                        ]
            ruleQueryFromRule rule `shouldBe` Right ("ds-1", "up == 0")
        it "falls back to the model's datasource uid" do
            let rule =
                    object
                        [ "data"
                            .= [ object
                                    [ "model"
                                        .= object
                                            [ "expr" .= ("up == 0" :: Text)
                                            , "datasource" .= object ["uid" .= ("ds-2" :: Text)]
                                            ]
                                    ]
                               ]
                        ]
            ruleQueryFromRule rule `shouldBe` Right ("ds-2", "up == 0")
        it "fails when no query carries an expr" do
            ruleQueryFromRule (object ["data" .= [object ["refId" .= ("A" :: Text)]]])
                `shouldBe` Left "rule: no query with an expr found"

    describe "seriesFromResponse" do
        it "decodes multi-frame results into series with absolute timestamps" do
            let response =
                    object
                        [ "results"
                            .= object
                                [ "A"
                                    .= object
                                        [ "status" .= (200 :: Int)
                                        , "frames"
                                            .= [ responseFrame "instance-1" [1000, 2000] [1.5, 2.5]
                                               , responseFrame "instance-2" [1000, 2000] [7, 8]
                                               ]
                                        ]
                                ]
                        ]
            case seriesFromResponse response of
                Left err -> expectationFailure (Text.unpack err)
                Right series -> do
                    map seriesName series `shouldBe` ["instance-1", "instance-2"]
                    map (map snd . seriesPoints) series `shouldBe` [[1.5, 2.5], [7, 8]]
                    map (firstPointTime . seriesPoints) series
                        `shouldBe` [utc "1970-01-01T00:00:01Z", utc "1970-01-01T00:00:01Z"]
        it "fails on an empty results payload" do
            seriesFromResponse (object ["results" .= object ["A" .= object ["status" .= (200 :: Int), "frames" .= ([] :: [Value])]]])
                `shouldBe` Left "ds/query: no data frames"

    describe "metricWindowFor" do
        it "defaults to 60m lead and runs to now for firing alerts" do
            let source = newRecord @Source
                alert = newRecord @Alert |> set #startedAt (Just (utc "2026-09-19T10:00:00Z"))
                now = utc "2026-09-19T10:30:00Z"
                window = MetricChart.metricWindowFor source alert now
            window.mwMaxPoints `shouldBe` 500
            diffSeconds window.mwFrom (utc "2026-09-19T09:00:00Z") `shouldBe` 0
            diffSeconds window.mwTo now `shouldBe` 0
        it "applies source config metrics overrides and trails resolved alerts" do
            let source =
                    newRecord @Source
                        |> set
                            #config
                            (object ["metrics" .= object ["leadMinutes" .= (30 :: Int), "trailMinutes" .= (5 :: Int), "maxPoints" .= (100 :: Int)]])
                alert =
                    newRecord @Alert
                        |> set #startedAt (Just (utc "2026-09-19T10:00:00Z"))
                        |> set #resolvedAt (Just (utc "2026-09-19T10:20:00Z"))
                now = utc "2026-09-19T12:00:00Z"
                window = MetricChart.metricWindowFor source alert now
            window.mwMaxPoints `shouldBe` 100
            diffSeconds window.mwFrom (utc "2026-09-19T09:30:00Z") `shouldBe` 0
            diffSeconds window.mwTo (utc "2026-09-19T10:25:00Z") `shouldBe` 0

    describe "seriesChartSvg" do
        it "renders an inline SVG carrying the series names" do
            let series = [MetricSeries "cpu" [(utc "2026-09-19T10:00:00Z", 1.0), (utc "2026-09-19T10:01:00Z", 2.0)]]
                svg = MetricChart.seriesChartSvg series
            Text.isInfixOf "<svg" svg `shouldBe` True
            Text.isInfixOf "width=\"100%\"" svg `shouldBe` True
            Text.isInfixOf "cpu" svg `shouldBe` True
        it "renders a no-data chart for empty series" do
            let svg = MetricChart.seriesChartSvg []
            Text.isInfixOf "<svg" svg `shouldBe` True
        it "pads a single-sample domain instead of collapsing to the center" do
            let svg = MetricChart.seriesChartSvg [MetricSeries "trap" [(utc "2026-09-19T10:00:00Z", 5.0)]]
            Text.isInfixOf "NaN" svg `shouldBe` False
            Text.isInfixOf "Infinity" svg `shouldBe` False
            Text.isInfixOf "chart-dot-1" svg `shouldBe` True
        it "draws no dots for dense series" do
            let points = [(addUTCTime (fromIntegral (i * 60)) (utc "2026-09-19T09:00:00Z"), 1.0) | i <- [0 .. 99 :: Int]]
                svg = MetricChart.seriesChartSvg [MetricSeries "dense" points]
            Text.isInfixOf "<circle" svg `shouldBe` False
            Text.isInfixOf "chart-line-1" svg `shouldBe` True

    describe "downsample" do
        it "passes through when under the point cap" do
            let points = [(utc "2026-09-19T10:00:00Z", 1.0), (utc "2026-09-19T10:01:00Z", 2.0)]
            MetricChart.downsample 500 points `shouldBe` points
        it "returns empty for empty input" do
            MetricChart.downsample 500 [] `shouldBe` []
        it "buckets to the cap with mean values and ordered times" do
            let points =
                    [ (addUTCTime (fromIntegral (i * 60)) t0, fromIntegral i)
                    | i <- [0 .. 599 :: Int]
                    ]
                out = MetricChart.downsample 60 points
            length out `shouldBe` 60
            map snd out `shouldSatisfy` allOrderedAscending
            map snd out `shouldSatisfy` \vals -> minimum vals < maximum vals

    describe "missingRanges" do
        it "covers the whole window when nothing is cached" do
            MetricChart.missingRanges 120 from to []
                `shouldBe` [(from, to)]
        it "fetches nothing when cached points cover the window" do
            let cached = [(addUTCTime (fromIntegral (i * 60)) from, 1.0) | i <- [0 .. 59 :: Int]]
            MetricChart.missingRanges 120 from to cached `shouldBe` []
        it "splits around a cached run in the middle" do
            let cached = [(addUTCTime (fromIntegral (i * 60)) (addUTCTime 1800 from), 1.0) | i <- [0 .. 29 :: Int]]
            -- trailing edge is within maxGap of the run end: no tail refetch
            MetricChart.missingRanges 120 from to cached
                `shouldBe` [(from, addUTCTime 1800 from)]
        it "refetches the tail when the window extends past the cached coverage" do
            let cached = [(addUTCTime (fromIntegral (i * 60)) from, 1.0) | i <- [0 .. 29 :: Int]]
                farTo = addUTCTime 7200 from
            MetricChart.missingRanges 120 from farTo cached
                `shouldBe` [(addUTCTime 1740 from, farTo)]
  where
    from = utc "2026-09-19T10:00:00Z"
    to = utc "2026-09-19T11:00:00Z"
    t0 = utc "2026-09-19T09:00:00Z"
    allOrderedAscending vals = and [a <= b | (a, b) <- zip vals (drop 1 vals)]
    utc :: Text -> UTCTime
    utc = TimeFormat.parseTimeOrError True TimeFormat.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . Text.unpack
    diffSeconds :: UTCTime -> UTCTime -> Integer
    diffSeconds a b = floor (realToFrac (a `diffUTCTime` b) :: Double)

responseFrame :: Text -> [Double] -> [Double] -> Value
responseFrame name times values =
    object
        [ "schema" .= object ["fields" .= [object ["name" .= ("Time" :: Text), "type" .= ("time" :: Text)], object ["name" .= name, "type" .= ("number" :: Text)]]]
        , "data" .= object ["values" .= [times, values]]
        ]

firstPointTime :: [(UTCTime, Double)] -> UTCTime
firstPointTime ((t, _) : _) = t
firstPointTime [] = error "firstPointTime: empty series"
