module Application.Service.Reports
    ( severityChartSvg
    , envChartSvg
    , volumeChartSvg
    , mttrChartSvg
    , formatDuration
    ) where

import IHP.Prelude
import Data.Int (Int64)
import Data.Time (Day)
import qualified Data.Text as Text
import qualified Data.Time.Format as TimeFormat
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG as DS
import qualified Graphics.Svg as SvgBuilder

chartSvg :: Double -> D.QDiagram DS.SVG D.V2 Double D.Any -> Text
chartSvg width dia = cs (SvgBuilder.renderBS (D.renderDia DS.SVG opts dia))
  where
    opts = DS.SVGOptions (D.mkWidth width) Nothing "" [] False

severityChartSvg :: [(Text, Int64)] -> Text
severityChartSvg rows = chartSvg 900 (hbarChart severityColor (map (second fromIntegral) rows))

envChartSvg :: [(Text, Int64)] -> Text
envChartSvg rows = chartSvg 900 (hbarChart (const envColor) (map (second fromIntegral) rows))

volumeChartSvg :: [(Day, Int64)] -> Text
volumeChartSvg rows = chartSvg 1200 (vbarChart (map (\(day, n) -> (dayLabel day, fromIntegral n)) rows))
  where
    dayLabel = cs . TimeFormat.formatTime TimeFormat.defaultTimeLocale "%m-%d"

mttrChartSvg :: [(Text, Double)] -> Text
mttrChartSvg rows = chartSvg 900 (hbarChart severityColor (map (second toMinutes) rows))
  where
    toMinutes seconds = seconds / 60

emptyChart :: D.QDiagram DS.SVG D.V2 Double D.Any
emptyChart = D.alignedText 0 0.5 "no data" D.# D.fontSizeL 1.2

hbarChart :: (Text -> D.Colour Double) -> [(Text, Double)] -> D.QDiagram DS.SVG D.V2 Double D.Any
hbarChart _ [] = emptyChart
hbarChart colorOf rows = D.vsep 0.7 (map (barRow maxVal) rows) D.# D.font "sans-serif"
  where
    maxVal = maximum (map snd rows)
    barLen = 40
    barRow maxValue (label, value) = D.hcat
        [ labelArea label
        , D.strutX 1
        , D.rect (len value) 1.8 D.# D.fc (colorOf label) D.# D.lw D.none D.# D.alignL
        , D.strutX 1
        , D.alignedText 0 0.5 (cs (valueLabel value)) D.# D.fontSizeL 1.1
        ]
      where
        len v = if maxValue <= 0 then 0 else max 0.15 (v / maxValue * barLen)
    labelArea label = D.alignedText 1 0.5 (cs label) D.# D.fontSizeL 1.1 D.<> D.strutX 14
    valueLabel value
        | value >= 90 = show (round value :: Int)
        | otherwise = show (fromIntegral (round (value * 10) :: Int) / (10 :: Double))

vbarChart :: [(Text, Double)] -> D.QDiagram DS.SVG D.V2 Double D.Any
vbarChart [] = emptyChart
vbarChart rows = D.hsep 0.4 (map (column maxVal) rows) D.# D.font "sans-serif"
  where
    maxVal = maximum (map snd rows)
    barHeight = 25
    column maxValue (label, value) = D.vcat
        [ D.alignedText 0.5 0 (cs (show (round value :: Int))) D.# D.fontSizeL 1
        , D.strutY 0.3
        , D.rect 1.6 (len value) D.# D.fc envColor D.# D.lw D.none
        , D.strutY 0.3
        , D.alignedText 0.5 1 (cs label) D.# D.fontSizeL 1
        ]
      where
        len v = if maxValue <= 0 then 0 else max 0.1 (v / maxValue * barHeight)

severityColor :: Text -> D.Colour Double
severityColor severity = D.sRGB24read (cs color)
  where
    color = case Text.toLower severity of
        "critical" -> "#dc3545" :: Text
        "disaster" -> "#dc3545"
        "high"     -> "#fd7e14"
        "average"  -> "#fd7e14"
        "warning"  -> "#ffc107"
        "info"     -> "#0dcaf0"
        "information" -> "#0dcaf0"
        _          -> "#6c757d"

envColor :: D.Colour Double
envColor = D.sRGB24read "#4c6ef5"

formatDuration :: Double -> Text
formatDuration seconds
    | seconds < 90 = show (round seconds :: Int) <> "s"
    | seconds < 5400 = show (round (seconds / 60) :: Int) <> "m"
    | otherwise = show (round (seconds / 3600) :: Int) <> "h"
