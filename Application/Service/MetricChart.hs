module Application.Service.MetricChart (
    MetricSeries,
    MetricWindow (..),
    seriesChartSvg,
    metricWindowFor,
    fetchAlertMetricSeries,
) where

import Application.Connector.GrafanaMetrics (
    MetricSeries (..),
    dsQueryRange,
    ruleQueryGet,
    ruleUidFromSourceUrl,
 )
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (FromJSON, parseMaybe)
import Data.List (foldl')
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Format as TimeFormat
import qualified Diagrams.Backend.SVG as DS
import qualified Diagrams.Prelude as D
import Generated.Types
import qualified Graphics.Svg as SvgBuilder
import IHP.Prelude
import System.Environment (lookupEnv)

-- Alert detail metric chart. Series come from the alert's own rule re-run as
-- a range query (see Application.Connector.GrafanaMetrics); rendering follows
-- Application.Service.Reports conventions: pixel grid, theme via CSS classes
-- (chart-*), inline SVG, native <title> tooltips.

type Chart = D.QDiagram DS.SVG D.V2 Double D.Any

-- | Effective fetch window: lead minutes before alert start; resolved alerts
-- show trail minutes past the resolve time, firing alerts run to now.
-- Per-source overrides in sources.config.metrics:
-- {"leadMinutes":60,"trailMinutes":15,"maxPoints":500}.
data MetricWindow = MetricWindow
    { mwFrom :: UTCTime
    , mwTo :: UTCTime
    , mwMaxPoints :: Int
    }

metricWindowFor :: Source -> Alert -> UTCTime -> MetricWindow
metricWindowFor source alert now =
    MetricWindow
        { mwFrom = addUTCTime (negate (lead * 60)) start
        , mwTo = to
        , mwMaxPoints = maxPoints
        }
  where
    start = fromMaybe alert.firstSeenAt alert.startedAt
    lead = fromMaybe 60 (cfg "leadMinutes")
    trail = fromMaybe 15 (cfg "trailMinutes")
    maxPoints = fromMaybe 500 (cfg "maxPoints")
    to = case alert.resolvedAt of
        Just resolvedAt -> min now (addUTCTime (trail * 60) resolvedAt)
        Nothing -> now
    cfg :: (FromJSON a) => Text -> Maybe a
    cfg key = join results
      where
        results =
            parseMaybe
                ( Aeson.withObject
                    "source.config"
                    ( \o -> do
                        metrics <- o Aeson..:? "metrics" Aeson..!= Aeson.Object mempty
                        Aeson.withObject "metrics" (\m -> m Aeson..:? Key.fromText key) metrics
                    )
                )
                source.config

-- | Grafana path: alert generatorURL -> rule uid -> rule query -> ds/query.
-- Fails cleanly (Left) for non-grafana sources, missing token, or any
-- upstream error — the widget renders the message instead of the chart.
fetchAlertMetricSeries :: Source -> Alert -> MetricWindow -> IO (Either Text [MetricSeries])
fetchAlertMetricSeries source alert window = do
    outcome <- try (fetchAlertMetricSeriesUnchecked source alert window)
    pure case outcome of
        Left ex -> Left (tshow (ex :: SomeException))
        Right result -> result

fetchAlertMetricSeriesUnchecked :: Source -> Alert -> MetricWindow -> IO (Either Text [MetricSeries])
fetchAlertMetricSeriesUnchecked source alert window = case source.type_ of
    "grafana" -> do
        token <- case tokenEnv of
            Just envVar -> fmap cs <$> lookupEnv (cs envVar)
            Nothing -> pure Nothing
        case (token, ruleUidFromSourceUrl =<< alert.sourceUrl) of
            (Nothing, _) -> pure (Left "No Grafana token configured (sources.config.tokenEnv)")
            (_, Nothing) -> pure (Left "Alert has no Grafana rule link")
            (Just token, Just ruleUid) -> do
                ruleResult <- ruleQueryGet source.baseUrl token ruleUid
                case ruleResult of
                    Left err -> pure (Left err)
                    Right (datasourceUid, expr) ->
                        dsQueryRange source.baseUrl token datasourceUid expr window.mwFrom window.mwTo window.mwMaxPoints
    _ -> pure (Left "Metrics are only available for Grafana-sourced alerts")
  where
    tokenEnv :: Maybe Text
    tokenEnv = parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: "tokenEnv")) source.config

-- | Multi-series line chart, 900px wide, inline SVG. Series beyond four
-- colors reuse the palette cyclically; the legend row shows name + range.
seriesChartSvg :: [MetricSeries] -> Text
seriesChartSvg series =
    foldl' injectTitle (renderChartSvg w h (D.position (gridLines <> axisLabels <> legend <> plotted))) tooltips
  where
    w = 900
    paddingL = 60
    paddingR = 12
    paddingB = 28
    plotW = w - paddingL - paddingR
    plotH = 180
    legendH = 22
    h = legendH + plotH + paddingB
    nonEmpty = [s | s <- series, not (null s.seriesPoints)]
    allPoints = concatMap seriesPoints nonEmpty
    ts = map (posix . fst) allPoints
    vs = map snd allPoints
    tMin = minimumDef 0 ts
    tMax = maximumDef 1 ts
    vMin = minimumDef 0 vs
    vMax = maximumDef 1 vs
    tSpan = max 1e-9 (tMax - tMin)
    vRange = vMax - vMin
    vLo = vMin - vRange * 0.05
    vHi = vMax + vRange * 0.05
    vSpan = max 1e-9 (vHi - vLo)
    xOf t = paddingL + (posix t - tMin) / tSpan * plotW
    yOf v = paddingB + (v - vLo) / vSpan * plotH
    lineCls i = "chart-line-" <> show (i `mod` 4 + 1 :: Int)
    seriesDomId i = "metricseries-" <> show (i :: Int)
    plotted =
        [ ( D.p2 (0, 0)
          , D.stroke (D.fromVertices [D.p2 (xOf t, yOf v) | (t, v) <- s.seriesPoints] :: D.Path D.V2 Double)
                D.# D.lw D.veryThin
                D.# DS.svgClass (cs (lineCls i))
                D.# DS.svgId (cs (seriesDomId i))
          )
        | (i, s) <- zip [0 ..] nonEmpty
        ]
    tooltips =
        [ ( seriesDomId i
          , s.seriesName
                <> ": "
                <> show (length s.seriesPoints)
                <> " points, "
                <> formatValue lo
                <> " .. "
                <> formatValue hi
          )
        | (i, s) <- zip [0 ..] nonEmpty
        , let pts = map snd s.seriesPoints
        , let lo = minimumDef 0 pts
        , let hi = maximumDef 0 pts
        ]
    gridValues = [vLo + vSpan * fromIntegral i / 4 | i <- [0 .. 4] :: [Int]]
    tickTimes =
        [ addUTCTime (realToFrac (tSpan * fromIntegral i / 5 :: Double)) (fromPosix tMin)
        | i <- [0 .. 5] :: [Int]
        ]
    gridLines =
        [ (D.p2 (paddingL, yOf v), D.hrule plotW D.# D.lw D.thin D.# DS.svgClass "chart-grid")
        | v <- gridValues
        ]
            <> [ (D.p2 (xOf t, paddingB), D.vrule plotH D.# D.lw D.thin D.# DS.svgClass "chart-grid")
               | t <- tickTimes
               ]
    axisLabels =
        [ (D.p2 (paddingL - 6, yOf v), chartText "chart-text-muted" 1 0.5 (formatValue v))
        | v <- gridValues
        ]
            <> [ (D.p2 (xOf t, 6), chartText "chart-text-muted" 0.5 0.5 (formatTick t))
               | t <- tickTimes
               ]
    legend =
        [ ( D.p2 (legendX i, h - 6)
          , legendChip i D.<> D.strutX 4 D.<> chartText "chart-text-muted" 0 0.5 (truncateLabel 28 s.seriesName)
          )
        | (i, s) <- zip [0 ..] nonEmpty
        ]
    legendX i = paddingL + sum [legendWidth j + 24 | j <- [0 .. i - 1]]
    legendWidth j = 12 + 4 + 0.55 * 12 * fromIntegral (Text.length (truncateLabel 28 (nonEmpty !! j).seriesName))
    legendChip i = D.rect 12 4 D.# D.fc (D.sRGB24read "#6c757d") D.# D.lw D.none D.# DS.svgClass (cs (lineCls i))
    formatTick t = cs (TimeFormat.formatTime TimeFormat.defaultTimeLocale "%H:%M" t)
    formatValue v
        | abs v >= 1000 = show (round v :: Integer)
        | abs v >= 1 = show (roundTo 2 v)
        | otherwise = show (roundTo 4 v)

minimumDef :: Double -> [Double] -> Double
minimumDef d [] = d
minimumDef _ xs = minimum xs

maximumDef :: Double -> [Double] -> Double
maximumDef d [] = d
maximumDef _ xs = maximum xs

roundTo :: Int -> Double -> Double
roundTo n v = fromIntegral (round (v * 10 ^ n) :: Integer) / 10 ^ n

posix :: UTCTime -> Double
posix = realToFrac . POSIX.utcTimeToPOSIXSeconds

fromPosix :: Double -> UTCTime
fromPosix = POSIX.posixSecondsToUTCTime . realToFrac

chartText :: Text -> Double -> Double -> Text -> Chart
chartText cls ax ay content =
    D.alignedText ax ay (cs content)
        D.# D.fontSizeL 12
        D.# DS.svgClass (cs cls)

truncateLabel :: Int -> Text -> Text
truncateLabel maxChars label
    | Text.length label <= maxChars = label
    | otherwise = Text.take (maxChars - 1) label <> "…"

-- Duplicated from Application.Service.Reports (not exported there): the
-- invisible backdrop pins the viewBox so edge labels don't clip.
renderChartSvg :: Double -> Double -> Chart -> Text
renderChartSvg w h dia = cs (SvgBuilder.renderBS (D.renderDia DS.SVG opts framed))
  where
    opts = DS.SVGOptions (D.mkWidth w) Nothing "" [] False
    framed = D.centerXY dia D.<> (D.rect w h D.# D.fcA D.transparent D.# D.lw D.none)

injectTitle :: Text -> (Text, Text) -> Text
injectTitle svg (marker, tip) =
    case Text.breakOn ("id=\"" <> marker <> "\"") svg of
        (before, rest)
            | not (Text.null rest) ->
                let (tag, after) = Text.breakOn ">" rest
                 in before <> tag <> "><title>" <> escapeXml tip <> "</title>" <> Text.drop 1 after
        _ -> svg

escapeXml :: Text -> Text
escapeXml = Text.concatMap escape
  where
    escape '&' = "&amp;"
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape c = Text.singleton c
