module Application.Service.Reports (
    severityChartSvg,
    envChartSvg,
    volumeChartSvg,
    mttrChartSvg,
    formatDuration,
    severityCssClass,
    truncateLabel,
) where

import Data.Int (Int64)
import qualified Data.Text as Text
import qualified Diagrams.Backend.SVG as DS
import qualified Diagrams.Prelude as D
import qualified Graphics.Svg as SvgBuilder
import IHP.Prelude

-- Static report charts (/reports). Charts are drawn on a pixel grid (1 unit =
-- 1 px at the design width) and rendered to inline SVG. NO colors are baked
-- in: every painted element carries a CSS class (chart-*) and app.css maps
-- those onto the active theme pack's tokens, so charts follow data-theme.

type Chart = D.QDiagram DS.SVG D.V2 Double D.Any

data BarDatum = BarDatum
    { barLabel :: Text
    , barValue :: Double
    , barValueText :: Text
    , barClass :: Text
    }

fontSizePx :: Double
fontSizePx = 12

severityChartSvg :: [(Text, Int64)] -> Text
severityChartSvg rows = hbarChartSvg 900 (map mk rows)
  where
    mk (severity, n) =
        BarDatum
            { barLabel = severity
            , barValue = fromIntegral n
            , barValueText = show n
            , barClass = severityCssClass severity
            }

envChartSvg :: [(Text, Int64)] -> Text
envChartSvg rows = hbarChartSvg 900 (map mk rows)
  where
    mk (env, n) =
        BarDatum
            { barLabel = truncateLabel 20 env
            , barValue = fromIntegral n
            , barValueText = show n
            , barClass = "chart-bar-accent"
            }

volumeChartSvg :: [(Text, Int64)] -> Text
volumeChartSvg rows = vbarChartSvg 1800 (map mk rows)
  where
    mk (bucket, n) =
        BarDatum
            { barLabel = bucket
            , barValue = fromIntegral n
            , barValueText = show n
            , barClass = "chart-bar-accent"
            }

mttrChartSvg :: [(Text, Double)] -> Text
mttrChartSvg rows = hbarChartSvg 900 (map mk rows)
  where
    mk (severity, seconds) =
        BarDatum
            { barLabel = severity
            , barValue = seconds
            , barValueText = formatDuration seconds
            , barClass = severityCssClass severity
            }

hbarChartSvg :: Double -> [BarDatum] -> Text
hbarChartSvg w [] = renderChartSvg w 60 (emptyChart w)
hbarChartSvg w rows = renderChartSvg w h (D.vsep rowGap (map row rows))
  where
    maxVal = maximum (map barValue rows)
    padding = 4
    labelCol = 150
    valueCol = 64
    gap = 12
    barH = 18
    rowGap = 10
    marginV = 8
    h = fromIntegral (length rows) * (barH + rowGap) - rowGap + 2 * marginV
    barArea = w - 2 * padding - labelCol - valueCol - 2 * gap
    row datum =
        D.hcat
            [ D.strutX padding
            , labelBox datum
            , D.strutX gap
            , barBox datum
            , D.strutX gap
            , valueBox datum
            , D.strutX padding
            ]
    labelBox datum = D.alignR (chartText "chart-text" 1 0.5 datum.barLabel) D.<> D.alignR (D.strutX labelCol)
    barBox datum = D.alignL (bar (scaled datum) barH datum.barClass) D.<> D.alignL (D.strutX barArea)
    valueBox datum = D.alignL (chartText "chart-text-muted" 0 0.5 datum.barValueText) D.<> D.alignL (D.strutX valueCol)
    scaled datum = if maxVal <= 0 then 0 else max 2 (datum.barValue / maxVal * barArea)

vbarChartSvg :: Double -> [BarDatum] -> Text
vbarChartSvg w [] = renderChartSvg w 60 (emptyChart w)
vbarChartSvg w rows = renderChartSvg w h (D.position (bars <> valueLabels <> dayLabels <> [baseline]))
  where
    padding = 8
    plotH = 180
    bottomGap = 6
    topMargin = 24
    bottomMargin = 28
    h = topMargin + plotH + bottomMargin
    plotW = w - 2 * padding
    n = length rows
    slot = plotW / fromIntegral n
    barW = min 60 (slot * 0.7)
    maxVal = maximum (map barValue rows)
    colH value = if maxVal <= 0 then 0 else max 2 (value / maxVal * plotH)
    xCenter i = padding + slot * (fromIntegral i + 0.5)
    -- horizontal labels only: shrink the font until the longest label fits
    -- its slot (avg glyph width ≈ 0.55 em), never rotate
    maxLabelChars = maximum (map (Text.length . barLabel) rows)
    dayFontSize = max 7 (min fontSizePx (slot * 0.9 / (0.55 * fromIntegral maxLabelChars)))
    indexed = zip [0 ..] rows
    bars =
        [ (D.p2 (xCenter i, 0), D.alignB (bar barW (colH datum.barValue) datum.barClass))
        | (i, datum) <- indexed
        , datum.barValue > 0
        ]
    valueLabels =
        [ (D.p2 (xCenter i, colH datum.barValue + 4 + descent), baselineTextC fontSizePx "chart-text-muted" datum.barValueText)
        | (i, datum) <- indexed
        , datum.barValue > 0
        ]
    dayLabels = zipWith mkDay [0 ..] rows
    mkDay i datum = (D.p2 (xCenter i, negate (bottomGap + capHeight)), baselineTextC dayFontSize "chart-text-muted" datum.barLabel)
    -- Inter vertical metrics (fractions of em): digits ride on the baseline
    -- up to cap height; the em box extends a descent below it
    capHeight = 0.73 * dayFontSize
    descent = 0.25 * fontSizePx
    baseline = (D.p2 (padding, 0), D.alignL (D.hrule plotW D.# D.lw D.thin D.# DS.svgClass "chart-grid"))

emptyChart :: Double -> Chart
emptyChart w = chartText "chart-text-muted" 0.5 0.5 "no data"

renderChartSvg :: Double -> Double -> Chart -> Text
renderChartSvg w h dia = cs (SvgBuilder.renderBS (D.renderDia DS.SVG opts framed))
  where
    opts = DS.SVGOptions (D.mkWidth w) Nothing "" [] False
    -- invisible backdrop fixes the viewport: diagrams' text envelopes
    -- underestimate real glyph extents, so without an explicit frame the
    -- viewBox clips edge labels
    framed = D.centerXY dia D.<> (D.rect w h D.# D.fcA D.transparent D.# D.lw D.none)

bar :: Double -> Double -> Text -> Chart
bar len h cls =
    D.rect len h
        -- explicit fill: diagrams' default fill is fully transparent
        -- (fill-opacity=0); the neutral gray is only a fallback — app.css
        -- re-colors via the chart-* class
        D.# D.fc (D.sRGB24read "#6c757d")
        D.# D.lw D.none
        D.# DS.svgClass (cs cls)

chartText :: Text -> Double -> Double -> Text -> Chart
chartText = chartTextSized fontSizePx

chartTextSized :: Double -> Text -> Double -> Double -> Text -> Chart
chartTextSized sizePx cls ax ay content =
    D.alignedText ax ay (cs content)
        D.# D.fontSizeL sizePx
        D.# DS.svgClass (cs cls)

-- Centered text anchored on the alphabetic baseline. alignedText's vertical
-- anchors become dominant-baseline="text-before-edge"/"text-after-edge",
-- which Firefox positions differently than Chrome (volume-chart labels drift
-- up and overlap bars); every engine agrees on the alphabetic baseline.
-- Horizontal centering comes from the chart-text-c rule in app.css.
baselineTextC :: Double -> Text -> Text -> Chart
baselineTextC sizePx cls content =
    D.baselineText (cs content)
        D.# D.fontSizeL sizePx
        D.# DS.svgClass (cs cls <> " chart-text-c")

severityCssClass :: Text -> Text
severityCssClass severity = case Text.toLower severity of
    "critical" -> "chart-sev-critical"
    "disaster" -> "chart-sev-critical"
    "high" -> "chart-sev-high"
    "average" -> "chart-sev-high"
    "warning" -> "chart-sev-warning"
    "info" -> "chart-sev-info"
    "information" -> "chart-sev-info"
    _ -> "chart-sev-other"

truncateLabel :: Int -> Text -> Text
truncateLabel maxChars label
    | Text.length label <= maxChars = label
    | otherwise = Text.take (maxChars - 1) label <> "…"

formatDuration :: Double -> Text
formatDuration seconds
    | seconds < 90 = show (round seconds :: Int) <> "s"
    | seconds < 5400 = show (round (seconds / 60) :: Int) <> "m"
    | otherwise = show (round (seconds / 3600) :: Int) <> "h"
