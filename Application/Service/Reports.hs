module Application.Service.Reports (
    volumeChartSvg,
    formatPct,
    formatDuration,
    formatDurationHm,
    formatHours,
    severityCssClass,
    severityFillClass,
    severityRank,
    truncateLabel,
) where

import Data.Int (Int64)
import qualified Data.Text as Text
import IHP.Prelude
import Text.Printf (printf)

-- Static report charts (/reports). The volume chart is a hand-rolled inline
-- SVG on a pixel grid; the horizontal charts are plain HTML bar rows (see
-- Web.View.Reports.Index). NO colors are baked in: every painted element
-- carries a CSS class (chart-* for SVG fills, sev-*/bar-accent for HTML bar
-- backgrounds) and app.css maps those onto the active theme pack's tokens,
-- so charts follow data-theme.

-- | Stacked vertical bar chart, one bar per bucket, segments stacked by
-- severity in canonical severity order (critical at the bottom). Buckets
-- with no alerts render as a baseline tick, not a zero-height bar. The
-- label above a bar shows the bucket total; every segment carries a native
-- <title> tooltip with the full severity breakdown.
volumeChartSvg :: [(Text, [(Text, Int64)])] -> Text
volumeChartSvg [] =
    "<svg viewBox=\"0 0 1140 120\" width=\"100%\">"
        <> "<text x=\"570\" y=\"60\" text-anchor=\"middle\" font-size=\"12\" class=\"chart-text-muted\">no data</text>"
        <> "</svg>"
volumeChartSvg rows =
    Text.concat
        [ "<svg viewBox=\"0 0 1140 240\" width=\"100%\" role=\"img\">"
        , gridLines
        , Text.concat (zipWith bar [0 ..] bars)
        , Text.concat (zipWith xLabel [0 ..] bars)
        , "</svg>"
        ]
  where
    bars = map mk rows
    mk (bucket, segments) =
        VolumeBar
            { vbLabel = bucket
            , vbSegments = [(severityCssClass severity, severity, n) | (severity, n) <- sortOn (severityRank . fst) segments]
            , vbTotal = sum (map snd segments)
            }
    slot = (plotRight - plotLeft) / fromIntegral (length rows)
    barW = min 56 (slot * 0.7)
    pxPerUnit = plotH / fromIntegral (max 1 (maximum (map vbTotal bars)))
    xCenter i = plotLeft + slot * (fromIntegral i + 0.5)
    -- Shrink labels until the longest one fits its slot (mono glyph advance
    -- ≈ 0.6em), so dense buckets (24h "per hour" mode) never overlap.
    maxLabelChars = max 1 (maximum (map (Text.length . vbLabel) bars))
    labelFont = max 8 (min 12 (slot * 0.9 / (0.6 * fromIntegral maxLabelChars)))
    maxTotalChars = max 1 (maximum (map (Text.length . tshow . vbTotal) bars))
    totalFont = max 8 (min 12 (barW * 0.9 / (0.6 * fromIntegral maxTotalChars)))
    gridLines =
        Text.concat
            [ "<line x1=\"" <> fmt plotLeft <> "\" y1=\"" <> fmt y <> "\" x2=\"" <> fmt plotRight <> "\" y2=\"" <> fmt y <> "\" class=\"chart-grid\"/>"
            | y <- [30, 90, 150]
            ]
            <> "<line x1=\""
            <> fmt plotLeft
            <> "\" y1=\""
            <> fmt baseline
            <> "\" x2=\""
            <> fmt plotRight
            <> "\" y2=\""
            <> fmt baseline
            <> "\" class=\"chart-grid-strong\"/>"
    bar i datum
        | vbTotal datum == 0 =
            "<rect x=\"" <> fmt (xCenter i - barW / 2) <> "\" y=\"" <> fmt (baseline - 2) <> "\" width=\"" <> fmt barW <> "\" height=\"2\" class=\"chart-tick\"/>"
        | otherwise =
            "<g><title>" <> escapeXml (barTip datum) <> "</title>" <> Text.concat segRects <> totalLabel <> "</g>"
      where
        x = xCenter i - barW / 2
        heights = [max 1.5 (fromIntegral n * pxPerUnit) | (_, _, n) <- vbSegments datum, n > 0]
        segRects = zipWith seg (scanl (flip subtract) baseline heights) [(cls, n, hgt) | ((cls, _, n), hgt) <- zip (filter (\(_, _, n) -> n > 0) (vbSegments datum)) heights]
        seg yTop (cls, _, hgt) =
            "<rect x=\"" <> fmt x <> "\" y=\"" <> fmt (yTop - hgt) <> "\" width=\"" <> fmt barW <> "\" height=\"" <> fmt hgt <> "\" rx=\"3\" class=\"" <> cls <> "\"/>"
        totalLabel =
            text (xCenter i) (max 16 (baseline - sum heights - 8)) totalFont "chart-text-muted chart-text-c chart-mono" (tshow (vbTotal datum))
    barTip datum =
        Text.intercalate "\n" $
            [name <> ": " <> tshow n | (_, name, n) <- vbSegments datum, n > 0]
                <> ["total: " <> tshow (vbTotal datum)]
    xLabel i datum = text (xCenter i) 224 labelFont "chart-text-muted chart-text-c chart-mono" datum.vbLabel

data VolumeBar = VolumeBar
    { vbLabel :: Text
    , vbSegments :: [(Text, Text, Int64)] -- (css class, series name, value), bottom-to-top
    , vbTotal :: Int64
    }

-- Geometry constants from the redesign mockup (viewBox 0 0 1140 240).
plotLeft, plotRight, baseline :: Double
plotLeft = 40
plotRight = 1120
baseline = 200

plotH :: Double
plotH = baseline - 30

text :: Double -> Double -> Double -> Text -> Text -> Text
text x y sizePx cls content =
    "<text x=\""
        <> fmt x
        <> "\" y=\""
        <> fmt y
        <> "\" font-size=\""
        <> fmt sizePx
        <> "\" class=\""
        <> cls
        <> "\">"
        <> escapeXml content
        <> "</text>"

fmt :: Double -> Text
fmt = Text.pack . printf "%.1f"

escapeXml :: Text -> Text
escapeXml = Text.concatMap escape
  where
    escape '&' = "&amp;"
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape c = Text.singleton c

-- | Percentage for CSS widths, one decimal ("66.7%").
formatPct :: Double -> Text
formatPct pct = Text.pack (printf "%.1f" pct) <> "%"

-- | Seconds as "42s" / "10m" / "2h" (coarse single-unit form).
formatDuration :: Double -> Text
formatDuration seconds
    | seconds < 90 = tshow (round seconds :: Int) <> "s"
    | seconds < 5400 = tshow (round (seconds / 60) :: Int) <> "m"
    | otherwise = tshow (round (seconds / 3600) :: Int) <> "h"

-- | Seconds as "42s" / "58m" / "1h 22m" / "8h 04m" (mockup MTTR form:
-- hours always carry the minutes remainder, zero-padded).
formatDurationHm :: Double -> Text
formatDurationHm seconds
    | seconds < 90 = tshow (round seconds :: Int) <> "s"
    | seconds < 3600 = tshow (round (seconds / 60) :: Int) <> "m"
    | otherwise =
        let (hours, mins) = totalMinutes `divMod` 60
            totalMinutes = round (seconds / 60) :: Int
         in tshow hours <> "h " <> Text.pack (printf "%02d" mins) <> "m"

-- | Seconds as decimal hours ("4.2") for the KPI tile.
formatHours :: Double -> Text
formatHours = Text.pack . printf "%.1f" . (/ 3600)

-- | SVG fill class for a severity (volume chart tooltips keep the raw name).
severityCssClass :: Text -> Text
severityCssClass severity = case Text.toLower severity of
    "critical" -> "chart-sev-critical"
    "disaster" -> "chart-sev-critical"
    "high" -> "chart-sev-high"
    "average" -> "chart-sev-high"
    "warning" -> "chart-sev-warning-dim"
    "info" -> "chart-sev-info"
    "information" -> "chart-sev-info"
    _ -> "chart-sev-other"

-- | HTML background class for a severity bar row / legend swatch.
severityFillClass :: Text -> Text
severityFillClass severity = case Text.toLower severity of
    "critical" -> "sev-critical"
    "disaster" -> "sev-critical"
    "high" -> "sev-high"
    "average" -> "sev-high"
    "warning" -> "sev-warning"
    "info" -> "sev-info"
    "information" -> "sev-info"
    _ -> "sev-other"

-- Canonical severity ordering (most severe first); drives stacked-bar
-- segment order and severity option lists. Unknown severities sort last.
severityRank :: Text -> Int
severityRank severity = fromMaybe 99 (lookup (Text.toLower severity) ranks)
  where
    ranks = zip ["critical", "disaster", "high", "average", "warning", "info", "information"] [0 ..]

truncateLabel :: Int -> Text -> Text
truncateLabel maxChars label
    | Text.length label <= maxChars = label
    | otherwise = Text.take (maxChars - 1) label <> "…"
