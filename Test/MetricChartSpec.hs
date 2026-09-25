module Test.MetricChartSpec (spec) where

import Application.Connector.GrafanaMetrics (
    MetricSeries (..),
    buildExploreUrl,
    ruleQueryFromRule,
    ruleUidFromSourceUrl,
    seriesFromResponse,
 )
import qualified Application.Service.Chart as Chart
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
            case seriesFromResponse 500 response of
                Left err -> expectationFailure (Text.unpack err)
                Right series -> do
                    map seriesName series `shouldBe` ["instance-1", "instance-2"]
                    map (map snd . seriesPoints) series `shouldBe` [[1.5, 2.5], [7, 8]]
                    map (firstPointTime . seriesPoints) series
                        `shouldBe` [utc "1970-01-01T00:00:01Z", utc "1970-01-01T00:00:01Z"]
        it "fails on an empty results payload" do
            seriesFromResponse 500 (object ["results" .= object ["A" .= object ["status" .= (200 :: Int), "frames" .= ([] :: [Value])]]])
                `shouldBe` Left "ds/query: no data frames"
        it "thins frames denser than the point cap" do
            let response =
                    object
                        [ "results"
                            .= object
                                [ "A"
                                    .= object
                                        [ "status" .= (200 :: Int)
                                        , "frames"
                                            .= [ responseFrame "instance-1" [1000, 2000, 3000, 4000, 5000, 6000, 7000] [1, 2, 3, 4, 5, 6, 7]
                                               ]
                                        ]
                                ]
                        ]
            case seriesFromResponse 3 response of
                Left err -> expectationFailure (Text.unpack err)
                Right series -> do
                    length series `shouldBe` 1
                    map (map snd . seriesPoints) series `shouldBe` [[1, 4, 7]]
        it "names series from labels when the value field is the generic 'Value'" do
            let frame =
                    object
                        [ "schema"
                            .= object
                                [ "fields"
                                    .= [ object ["name" .= ("Time" :: Text), "type" .= ("time" :: Text)]
                                       , object
                                            [ "name" .= ("Value" :: Text)
                                            , "type" .= ("number" :: Text)
                                            , "labels" .= object ["__name__" .= ("up" :: Text), "instance" .= ("host1" :: Text)]
                                            ]
                                       ]
                                ]
                        , "data" .= object ["values" .= [[1000, 2000], [1, 2] :: [Double]]]
                        ]
                response = object ["results" .= object ["A" .= object ["status" .= (200 :: Int), "frames" .= [frame]]]]
            case seriesFromResponse 500 response of
                Left err -> expectationFailure (Text.unpack err)
                Right series -> map seriesName series `shouldBe` ["up{instance=\"host1\"}"]
        it "prefers config.displayNameFromDS over labels" do
            let frame =
                    object
                        [ "schema"
                            .= object
                                [ "fields"
                                    .= [ object ["name" .= ("Time" :: Text), "type" .= ("time" :: Text)]
                                       , object
                                            [ "name" .= ("Value" :: Text)
                                            , "type" .= ("number" :: Text)
                                            , "labels" .= object ["instance" .= ("host1" :: Text)]
                                            , "config" .= object ["displayNameFromDS" .= ("node_load1 host1" :: Text)]
                                            ]
                                       ]
                                ]
                        , "data" .= object ["values" .= [[1000, 2000], [1, 2] :: [Double]]]
                        ]
                response = object ["results" .= object ["A" .= object ["status" .= (200 :: Int), "frames" .= [frame]]]]
            case seriesFromResponse 500 response of
                Left err -> expectationFailure (Text.unpack err)
                Right series -> map seriesName series `shouldBe` ["node_load1 host1"]

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

    describe "chartDataSvg" do
        it "renders threshold lines with labels" do
            let series = [MetricChart.metricSeriesInfo (MetricSeries "cpu" [(utc "2026-09-19T10:00:00Z", 50.0), (utc "2026-09-19T10:01:00Z", 60.0)]) Nothing]
                data_ = MetricChart.MetricChartData series [Chart.ChartThreshold 70 "trigger threshold 70"] []
                svg = MetricChart.chartDataSvg Chart.ScaleAuto data_
            Text.isInfixOf "chart-threshold" svg `shouldBe` True
            Text.isInfixOf "trigger threshold 70" svg `shouldBe` True
        it "log scale handles wide magnitude spreads without NaN" do
            let mkInfo name pts = MetricChart.metricSeriesInfo (MetricSeries name pts) Nothing
                series =
                    [ mkInfo "big" [(addUTCTime (fromIntegral (i * 60)) t0, 1000 + fromIntegral i) | i <- [0 .. 20 :: Int]]
                    , mkInfo "small" [(addUTCTime (fromIntegral (i * 60)) t0, 1 + fromIntegral i * 0.1) | i <- [0 .. 20 :: Int]]
                    ]
                svg = MetricChart.chartDataSvg Chart.ScaleLog10 (MetricChart.MetricChartData series [] [])
            Text.isInfixOf "NaN" svg `shouldBe` False
            Text.isInfixOf "Infinity" svg `shouldBe` False
        it "log2 scale renders power-of-two ticks" do
            let series = [MetricChart.metricSeriesInfo (MetricSeries "m" [(addUTCTime (fromIntegral (i * 60)) t0, 2 ^^ i) | i <- [0 .. 8 :: Int]]) Nothing]
                svg = MetricChart.chartDataSvg Chart.ScaleLog2 (MetricChart.MetricChartData series [] [])
            Text.isInfixOf "NaN" svg `shouldBe` False
            Text.isInfixOf "Infinity" svg `shouldBe` False
            hasScientificNotation svg `shouldBe` False
        it "axis labels never use scientific notation" do
            let series = [MetricChart.metricSeriesInfo (MetricSeries "m" [(addUTCTime (fromIntegral (i * 60)) t0, 0.05 * fromIntegral i) | i <- [0 .. 20 :: Int]]) Nothing]
                svg = MetricChart.chartDataSvg Chart.ScaleLinear (MetricChart.MetricChartData series [] [])
            hasScientificNotation svg `shouldBe` False
        it "unit-aware axis labels shorten bytes and widen the left margin" do
            let series = [MetricChart.metricSeriesInfo (MetricSeries "disk" [(addUTCTime (fromIntegral (i * 3600)) t0, 5.5e9 + fromIntegral i * 1e6) | i <- [0 .. 20 :: Int]]) (Just "B")]
                svg = MetricChart.chartDataSvg Chart.ScaleLinear (MetricChart.MetricChartData series [] [])
                (_, _, _, (plotLeft, _)) = MetricChart.chartRenderMeta Chart.ScaleLinear (MetricChart.MetricChartData series [] [])
            Text.isInfixOf "GB" svg `shouldBe` True
            plotLeft `shouldSatisfy` (>= 60)
        it "log scale clamps non-positive samples to the baseline" do
            let series = [MetricChart.metricSeriesInfo (MetricSeries "mixed" [(addUTCTime (fromIntegral (i * 60)) t0, v) | (i, v) <- zip [0 ..] [5, 0, -3, 50, 500 :: Double]]) Nothing]
                svg = MetricChart.chartDataSvg Chart.ScaleLog10 (MetricChart.MetricChartData series [] [])
            Text.isInfixOf "NaN" svg `shouldBe` False
            Text.isInfixOf "Infinity" svg `shouldBe` False
        it "emits hover payload points and the padded time domain" do
            let series = [MetricChart.metricSeriesInfo (MetricSeries "m" [(addUTCTime (fromIntegral (i * 60)) t0, fromIntegral i) | i <- [0 .. 5 :: Int]]) Nothing]
                data_ = MetricChart.MetricChartData series [] []
                json = MetricChart.chartHoverJson data_
                (_, (tLo, tHi), (yLo, yHi), _) = MetricChart.chartRenderMeta Chart.ScaleAuto data_
            Text.isInfixOf "\"name\":\"m\"" json `shouldBe` True
            Text.isInfixOf "\"points\"" json `shouldBe` True
            tHi - tLo `shouldSatisfy` (> 300)
            yHi - yLo `shouldSatisfy` (> 0.1)

    describe "formatWithUnits" do
        it "scales bytes 1024-based" do
            Chart.formatWithUnits (Just "B") 5.5e9 `shouldBe` "5.1 GB"
            Chart.formatWithUnits (Just "B") 2048 `shouldBe` "2 KB"
            Chart.formatWithUnits (Just "B") 0 `shouldBe` "0 B"
        it "turns seconds into compounds" do
            Chart.formatWithUnits (Just "s") 45 `shouldBe` "45s"
            Chart.formatWithUnits (Just "s") 300 `shouldBe` "5m"
            Chart.formatWithUnits (Just "s") 7200 `shouldBe` "2h"
            Chart.formatWithUnits (Just "s") 180000 `shouldBe` "2d 2h"
        it "appends known units and compacts big plain numbers" do
            Chart.formatWithUnits (Just "%") 66.7 `shouldBe` "66.7%"
            Chart.formatWithUnits (Just "ms") 42 `shouldBe` "42 ms"
            Chart.formatWithUnits Nothing 123456789 `shouldBe` "123.5M"
            Chart.formatWithUnits Nothing 0.05 `shouldBe` "0.05"

    describe "thresholdsFromExpression" do
        it "extracts constants from new-style functions" do
            MetricChart.thresholdsFromExpression "last(/h/system.cpu.load)>80"
                `shouldBe` [80]
        it "extracts constants from old-style braces and multiple clauses" do
            MetricChart.thresholdsFromExpression "{h:k.last()}>=70 and {h:k2.last()}<5"
                `shouldBe` [70, 5]
        it "supports <=, <>, negative and decimal constants" do
            MetricChart.thresholdsFromExpression "{h:k.last()}<=0.5 or {h:k2.last()}<>-3"
                `shouldBe` [0.5, -3]
        it "supports K/M/G/T suffixes" do
            MetricChart.thresholdsFromExpression "last(/h/net.if.in)>1.5K"
                `shouldBe` [1500]
        it "skips macros and string comparisons" do
            MetricChart.thresholdsFromExpression "last(/h/k)>{$THRESHOLD}"
                `shouldBe` []
            MetricChart.thresholdsFromExpression "last(/h/k)=#DOWN"
                `shouldBe` []
        it "skips time-looking constants after comparison operators" do
            MetricChart.thresholdsFromExpression "{h:k.last(5m)}>70"
                `shouldBe` [70]

    describe "buildExploreUrl" do
        it "embeds the expr and datasource as a percent-encoded panes param" do
            let url = buildExploreUrl "https://grafana.example" "ds-1" (Just "prometheus") "up == 0"
            Text.isInfixOf "/explore?orgId=1" url `shouldBe` True
            Text.isInfixOf "pane-1" url `shouldBe` True
            Text.isInfixOf "up%20%3D%3D%200" url `shouldBe` True
            Text.isInfixOf "prometheus" url `shouldBe` True
            Text.isInfixOf "ds-1" url `shouldBe` True
        it "omits the datasource type when unknown" do
            let url = buildExploreUrl "https://grafana.example" "ds-1" Nothing "up"
            Text.isInfixOf "%22uid%22%3A%22ds-1%22" url `shouldBe` True
            Text.isInfixOf "type" url `shouldBe` False

    describe "parseScaleParam" do
        it "maps query params to scale modes" do
            MetricChart.parseScaleParam (Just "linear") `shouldBe` Chart.ScaleLinear
            MetricChart.parseScaleParam (Just "log10") `shouldBe` Chart.ScaleLog10
            MetricChart.parseScaleParam (Just "log2") `shouldBe` Chart.ScaleLog2
            MetricChart.parseScaleParam (Just "log") `shouldBe` Chart.ScaleLog10
            MetricChart.parseScaleParam (Just "auto") `shouldBe` Chart.ScaleAuto
            MetricChart.parseScaleParam Nothing `shouldBe` Chart.ScaleAuto
            MetricChart.parseScaleParam (Just "bogus") `shouldBe` Chart.ScaleAuto

    describe "metricWindowForRange" do
        it "anchors relative ranges at now" do
            let source = newRecord @Source
                alert = newRecord @Alert
                now = utc "2026-09-19T12:00:00Z"
                window = MetricChart.metricWindowForRange source alert now "24h"
            diffSeconds window.mwFrom (utc "2026-09-19T12:00:00Z") `shouldBe` -86400
            diffSeconds window.mwTo now `shouldBe` 0
            window.mwMaxPoints `shouldBe` 500
        it "falls back to the alert window for unknown ranges" do
            let source = newRecord @Source
                alert = newRecord @Alert |> set #startedAt (Just (utc "2026-09-19T10:00:00Z"))
                now = utc "2026-09-19T10:30:00Z"
                window = MetricChart.metricWindowForRange source alert now "bogus"
            diffSeconds window.mwFrom (utc "2026-09-19T09:00:00Z") `shouldBe` 0

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
    -- scientific notation can only appear inside <text> labels; the
    -- attribute soup ("stroke-linejoin", "fill-opacity") contains "e-"
    -- substrings and would false-positive a raw "e-" search
    hasScientificNotation svg =
        any (Text.isInfixOf "e-") [label | frag <- drop 1 (Text.splitOn "<text" svg), let label = Text.takeWhile (/= '<') (Text.drop 1 (Text.dropWhile (/= '>') frag))]
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
