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

import qualified Application.Service.Chart as Chart
import Data.Int (Int64)
import qualified Data.Text as Text
import IHP.Prelude
import Text.Printf (printf)

-- Static report charts (/reports). Charts render through the standard chart
-- widget Application.Service.Chart (Diagrams; one toolkit for all charts).
-- The horizontal charts are plain HTML bar rows (see Web.View.Reports.Index):
-- no SVG needed there, and HTML bars stay responsive/CSS-themed for free.
-- NO colors are baked in: every painted element carries a CSS class (chart-*
-- for SVG fills, sev-*/bar-accent for HTML bar backgrounds) and app.css maps
-- those onto the active theme pack's tokens, so charts follow data-theme.

-- | Stacked vertical bar chart, one bar per bucket, segments stacked by
-- severity in canonical severity order (critical at the bottom). Buckets
-- with no alerts render as a baseline tick, not a zero-height bar. The
-- label above a bar shows the bucket total; every bar carries a native
-- <title> tooltip with the full severity breakdown.
volumeChartSvg :: [(Text, [(Text, Int64)])] -> Text
volumeChartSvg rows =
    Chart.stackedBarChartSvg
        1140
        240
        [ Chart.StackedBar
            bucket
            [ Chart.BarSegment (severityCssClass severity) severity n
            | (severity, n) <- sortOn (severityRank . fst) segments
            ]
        | (bucket, segments) <- rows
        ]

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
