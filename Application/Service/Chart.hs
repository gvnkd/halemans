module Application.Service.Chart (
    LineSeries (..),
    lineChartSvg,
    ScaleMode (..),
    ChartThreshold (..),
    LineChartOptions (..),
    defaultLineChartOptions,
    lineChartSvgWith,
    lineChartTimeDomain,
    ChartLayout (..),
    lineChartLayout,
    formatWithUnits,
    roundTo,
    BarSegment (..),
    StackedBar (..),
    stackedBarChartSvg,
) where

import Data.Int (Int64)
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Format as TimeFormat
import qualified Diagrams.Backend.SVG as DS
import qualified Diagrams.Prelude as D
import qualified Graphics.Svg as SvgBuilder
import IHP.Prelude
import Text.Printf (printf)

-- Render the framed diagram and make the svg fluid-width: diagrams-svg pins
-- absolute width/height attributes, which under-fill (or overflow) the card
-- depending on its width — the old hand-rolled charts carried width="100%".
-- width="100%" + no height attribute + viewBox scales proportionally.
--
-- The output is ALSO z-order normalized: diagrams-svg paints stroked
-- segments after filled shapes no matter the composition order, so grid
-- lines would land on top of the bars. Move every chart-grid group right
-- after the frame backdrop so grids stay behind the data.
renderFluidSvg :: Double -> Double -> D.Diagram DS.SVG -> Text
renderFluidSvg frameW frameH dia =
    moveGridBehind
        ( Text.replace
            (" height=\"" <> fmt4 frameH <> "\"")
            ""
            (Text.replace ("width=\"" <> fmt4 frameW <> "\"") "width=\"100%\"" raw)
        )
  where
    raw = cs (SvgBuilder.renderBS (D.renderDia DS.SVG opts dia))
    opts = DS.SVGOptions (D.mkSizeSpec2D (Just frameW) (Just frameH)) Nothing "" [] False
    fmt4 x = Text.pack (printf "%.4f" x)

-- Extract every top-level <g class="chart-grid...">...</g> block and
-- re-insert it right after the first </g> (the frame backdrop close). Grid
-- groups wrap a single path (no nested <g>), so first-</g> chunking is safe
-- for them.
moveGridBehind :: Text -> Text
moveGridBehind svg =
    case Text.breakOn "</g>" svg of
        (before, after)
            | not (Text.null after) ->
                let (grids, others) = collect (Text.drop 4 after) [] mempty
                 in before <> "</g>" <> mconcat grids <> others
        _ -> svg
  where
    collect :: Text -> [Text] -> Text -> ([Text], Text)
    collect t grids others
        | Text.null t = (reverse grids, others)
        | otherwise =
            case Text.breakOn "<g " t of
                (pre, rest)
                    | Text.null rest -> (reverse grids, others <> pre)
                    | otherwise ->
                        let (chunk, afterChunk) = takeGroup rest
                         in if "class=\"chart-grid" `Text.isInfixOf` chunk
                                then collect afterChunk (chunk : grids) others
                                else collect afterChunk grids (others <> pre <> chunk)
    takeGroup t =
        let (body, rest) = Text.breakOn "</g>" t
         in (body <> "</g>", Text.drop 4 rest)

-- Standard line-chart widget — the project's reusable chart building block
-- on top of Diagrams (design_docs/milestone_13.md §9). Fixed 900x250 frame
-- with explicit margins; themed via CSS classes following the Reports
-- conventions: chart-grid, chart-text-muted, chart-line-N / chart-dot-N
-- (4-color palette, cycling). One native <title> tooltip per series.
--
-- Rendering recipe — the traps that made the first diagrams version
-- unusable (recorded in design_docs/ops-notes.md):
--   * BOTH output dimensions are pinned (mkSizeSpec2D). Pinning only the
--     width lets the envelope aspect shrink the height and rescale every
--     coordinate — labels landed in the plot center.
--   * The invisible backdrop is positioned at the FRAME CENTER, so the
--     composite envelope IS the frame and every coordinate maps 1:1. All
--     elements stay well inside the frame (margins are generous because
--     diagrams' text envelopes underestimate glyph extents).
--   * Elements are placed by D.position at absolute chart coordinates —
--     mind the origin conventions: alignedText anchors at its alignment
--     point, circles at their center, but a stroked fromVertices path keeps
--     origin (0,0) with vertices already absolute — placing it at a non-zero
--     point double-offsets it (grid lines once landed at 2x coordinates,
--     inflating the envelope to 1740px and rescaling the whole chart).
--   * Degenerate domains (a single fresh sample) are padded to a readable
--     window instead of exploding through tiny-span guards.
--   * Sparse series (<= 25 points) get dots: a one-vertex path has no
--     segment and would be invisible.

data LineSeries = LineSeries
    { seriesName :: Text
    , seriesPoints :: [(UTCTime, Double)]
    , seriesUnits :: Maybe Text -- zabbix item units ("B", "s", "%"); Nothing = plain
    }
    deriving (Eq, Show)

-- Y-axis scaling. ScaleAuto picks logarithmic when the data spans more
-- than ~1.3 orders of magnitude (ratio >= 20): a trigger chart mixing
-- e.g. memory % (~65) with a leak score (~8) squashes the small series
-- onto the baseline on a linear scale. Log scales come in base-10 and
-- base-2 flavours.
data ScaleMode = ScaleLinear | ScaleLog10 | ScaleLog2 | ScaleAuto
    deriving (Eq, Show)

-- Horizontal reference line, e.g. a zabbix trigger threshold.
data ChartThreshold = ChartThreshold
    { ctValue :: Double
    , ctLabel :: Text
    }
    deriving (Eq, Show)

data LineChartOptions = LineChartOptions
    { lcScale :: ScaleMode
    , lcThresholds :: [ChartThreshold]
    }
    deriving (Eq, Show)

defaultLineChartOptions :: LineChartOptions
defaultLineChartOptions = LineChartOptions ScaleAuto []

-- | The padded [tLo, tHi] epoch-second domain the line chart renders its
-- x axis over (5% margins, degenerate-span guard). The hover-tooltip JSON
-- embeds this so the browser maps cursor x back to a timestamp using the
-- exact geometry the server rendered with.
lineChartTimeDomain :: [LineSeries] -> (Double, Double)
lineChartTimeDomain series = (tLo, tHi)
  where
    ts = concatMap (map (posix . fst) . seriesPoints) series
    tMin = minimumDef 0 ts
    tMax = maximumDef 1 ts
    tPad = max 30 ((tMax - tMin) * 0.05)
    tLo = tMin - tPad
    tHi = tMax + tPad

-- | Multi-series line chart as a self-contained inline SVG.
lineChartSvg :: [LineSeries] -> Text
lineChartSvg = lineChartSvgWith defaultLineChartOptions

-- | The resolved geometry of a line chart: effective scale (Auto
-- resolved), padded time/value domains, and the x plot strip. Exported so
-- the hover-tooltip payload (app.js) replicates the exact server-side
-- coordinate mapping for hit-testing.
data ChartLayout = ChartLayout
    { clScale :: ScaleMode
    , clTimeDomain :: (Double, Double)
    , clValueDomain :: (Double, Double)
    , clPlotLeft :: Double
    , clPlotRight :: Double
    }
    deriving (Eq, Show)

lineChartLayout :: LineChartOptions -> [LineSeries] -> ChartLayout
lineChartLayout opts series =
    ChartLayout
        { clScale = scale
        , clTimeDomain = lineChartTimeDomain series
        , clValueDomain = (dLo, dHi)
        , clPlotLeft = plotLeft
        , clPlotRight = plotRight
        }
  where
    nonEmpty = [s | s <- series, not (null s.seriesPoints)]
    vs = concatMap (map snd . seriesPoints) nonEmpty
    thrValues = [ctValue t | t <- lcThresholds opts, scale == ScaleLinear || ctValue t > 0]
    vMinRaw = minimumDef 1 vs
    vMaxRaw = maximumDef (vMinRaw + 1) vs
    scale = case lcScale opts of
        ScaleLinear -> ScaleLinear
        ScaleLog10 -> ScaleLog10
        ScaleLog2 -> ScaleLog2
        ScaleAuto
            | vMinRaw > 0 && vMaxRaw / vMinRaw >= 20 -> ScaleLog10
            | otherwise -> ScaleLinear
    minPositive = minimumDef 1 [v | v <- vs ++ thrValues, v > 0]
    floorVal = minPositive / logBaseOf scale
    logBaseOf ScaleLog2 = 2
    logBaseOf _ = 10
    toDom v = case scale of
        ScaleLinear -> v
        _ -> logBase (logBaseOf scale) (max v floorVal)
    (dMin, dMax) =
        let lo = minimumDef 0 (map toDom (vs ++ thrValues))
            hi = maximumDef 1 (map toDom (vs ++ thrValues))
         in (lo, hi)
    dRange = dMax - dMin
    (dLo, dHi)
        | dRange < 1e-9 = (dMin - 1, dMax + 1)
        | otherwise = (dMin - dRange * 0.05, dMax + dRange * 0.05)
    dSpan = dHi - dLo
    gridValues = gridValuesFor scale dLo dSpan
    -- The y axis is formatted in the series' units when every series
    -- agrees on them ("B" -> GB-short labels); mixed or unit-less series
    -- get compact plain numbers so long byte counts stay on screen.
    axisUnits = case nub [u | s <- nonEmpty, Just u <- [s.seriesUnits], not (Text.null u)] of
        [u] -> Just u
        _ -> Nothing
    axisLabelTexts = map (formatWithUnits axisUnits) gridValues
    -- Widen the left margin to the widest axis label instead of clipping
    -- ("1234567890" at the fixed 60px margin rendered out of frame).
    plotLeft = max 60 (9 * maximumDef 4 (map (fromIntegral . Text.length) axisLabelTexts) + 14)
    plotRight = 900 - 30

-- | Multi-series line chart with explicit scale and threshold lines.
lineChartSvgWith :: LineChartOptions -> [LineSeries] -> Text
lineChartSvgWith opts series =
    renderFluidSvg frameW frameH framed
  where
    framed = D.position elements D.<> frameBackdrop
    frameBackdrop =
        D.position
            [
                ( D.p2 (frameW / 2, frameH / 2)
                , D.rect frameW frameH D.# D.lw D.none D.# D.fcA D.transparent
                )
            ]
    elements :: [(D.P2 Double, D.Diagram DS.SVG)]
    elements =
        case nonEmpty of
            [] -> [(D.p2 (frameW / 2, frameH / 2), textD ("no data" :: Text) 0.5 0.5)]
            _ -> gridLinesE <> axisLabelsE <> legendE <> thresholdE <> seriesLinesE <> dotMarkersE

    -- frame geometry (diagrams coordinates, y up; svg flips on output)
    frameW = 900
    frameH = 250
    legendH = 24
    layout = lineChartLayout opts series
    plotLeft = layout.clPlotLeft
    plotRight = layout.clPlotRight
    plotTop = frameH - legendH - 8
    plotBottom = 34
    plotW = plotRight - plotLeft
    plotH = plotTop - plotBottom

    nonEmpty = [s | s <- series, not (null s.seriesPoints)]
    scale = layout.clScale
    (tLo, tHi) = layout.clTimeDomain
    (dLo, dHi) = layout.clValueDomain
    tSpan = tHi - tLo
    dSpan = dHi - dLo
    logBaseOf ScaleLog2 = 2
    logBaseOf _ = 10
    toDom v = case scale of
        ScaleLinear -> v
        _ -> logBase (logBaseOf scale) (max v floorVal)
    floorVal = case scale of
        ScaleLinear -> 1
        _ -> 10 ^^ floor (logBase (logBaseOf scale) (max minPositive 1e-12)) / logBaseOf scale
    minPositive = minimumDef 1 [v | v <- vs ++ map ctValue (lcThresholds opts), v > 0]
    vs = concatMap (map snd . seriesPoints) nonEmpty
    fromDom d = case scale of
        ScaleLinear -> d
        _ -> logBaseOf scale ** d
    xOfT t = plotLeft + (posix t - tLo) / tSpan * plotW
    yOfV v = min plotTop (max plotBottom (plotBottom + (toDom v - dLo) / dSpan * plotH))
    gridValues = gridValuesFor scale dLo dSpan
    axisUnits = case nub [u | s <- nonEmpty, Just u <- [s.seriesUnits], not (Text.null u)] of
        [u] -> Just u
        _ -> Nothing
    tickTimes =
        [ addUTCTime (realToFrac (tSpan * fromIntegral i / 5 :: Double)) (fromPosix tLo)
        | i <- [0 .. 5] :: [Int]
        ]
    paletteIdx i = i `mod` 4 + 1 :: Int
    lineCls i = "chart-line-" <> show (paletteIdx i)
    dotCls i = "chart-dot-" <> show (paletteIdx i)

    segD (x1, y1) (x2, y2) =
        D.stroke (D.fromVertices [D.p2 (x1, y1), D.p2 (x2, y2)] :: D.Path D.V2 Double)
    textD :: Text -> Double -> Double -> D.Diagram DS.SVG
    textD content ax ay =
        D.alignedText ax ay (cs content) D.# D.fontSizeL 14 D.# DS.svgClass "chart-text-muted"

    gridLinesE :: [(D.P2 Double, D.Diagram DS.SVG)]
    gridLinesE =
        [ (D.p2 (0, 0), segD (plotLeft, yOfV v) (plotRight, yOfV v) D.# D.lw D.thin D.# DS.svgClass "chart-grid")
        | v <- gridValues
        ]
            <> [ (D.p2 (0, 0), segD (xOfT t, plotBottom) (xOfT t, plotTop) D.# D.lw D.thin D.# DS.svgClass "chart-grid")
               | t <- tickTimes
               ]
    axisLabelsE :: [(D.P2 Double, D.Diagram DS.SVG)]
    axisLabelsE =
        [ (D.p2 (plotLeft - 6, yOfV v), textD (formatWithUnits axisUnits v) 1 0.5)
        | v <- gridValues
        ]
            <> [ let isLastTick = i == length tickTimes - 1
                     -- the last tick's centered label would overhang the
                     -- right frame edge (worse with day.month labels)
                     tickX = if isLastTick then plotRight else xOfT t
                     tickAlign = if isLastTick then 1 else 0.5
                  in (D.p2 (tickX, 16), textD (formatTick t) tickAlign 0.5)
               | (i, t) <- zip [0 :: Int ..] tickTimes
               ]
    legendE :: [(D.P2 Double, D.Diagram DS.SVG)]
    legendE =
        [ ( D.p2 (legendX i + 6, frameH - 13)
          , DS.svgTitle
                (cs s.seriesName)
                ( D.hcat
                    [ D.rect 12 4 D.# D.lw D.none D.# D.fc D.black D.# D.fillOpacity 1 D.# DS.svgClass (cs (dotCls i))
                    , D.strutX 4
                    , textD (truncateLabel 28 s.seriesName) 0 0.5
                    ]
                )
          )
        | (i, s) <- zip [0 ..] nonEmpty
        ]
    legendX i = plotLeft + sum [legendWidth j + 24 | j <- [0 .. i - 1]]
    -- ~8px per glyph at the pinned 14px chart font
    legendWidth j = 18 + 8 * fromIntegral (Text.length (truncateLabel 28 (nonEmpty !! j).seriesName))
    thresholdE :: [(D.P2 Double, D.Diagram DS.SVG)]
    thresholdE = concat (zipWith thresholdPair placedThresholds labelYs)
      where
        -- Bottom-up pass keeps labels of close threshold lines from
        -- overlapping (two zabbix constants 10 units apart land ~15px
        -- apart on a small frame).
        placedThresholds = sortOn ctValue (lcThresholds opts)
        rawLabelYs =
            [ min (plotTop - 4) (max (plotBottom + 6) (if thrY > plotTop - 20 then thrY - 14 else thrY + 14))
            | t <- placedThresholds
            , let thrY = yOfV (ctValue t)
            ]
        labelYs :: [Double]
        labelYs = go (plotBottom + 6) rawLabelYs
        go _ [] = []
        go prev (raw : rest) =
            let y = max raw (prev + 13)
             in y : go y rest
    thresholdPair t thrLabelY = [(D.p2 (0, 0), lineD), (D.p2 (plotRight - 4, thrLabelY), labelD)]
      where
        thrY = yOfV (ctValue t)
        lineD =
            segD (plotLeft, thrY) (plotRight, thrY)
                D.# D.lw D.thin
                D.# D.dashing [5, 3] 0
                D.# DS.svgClass "chart-threshold"
                D.# DS.svgTitle (cs (ctLabel t))
        labelD =
            D.alignedText 1 0.5 (cs (truncateLabel 24 (ctLabel t)))
                D.# D.fontSizeL 14
                D.# DS.svgClass "chart-text-muted chart-threshold-label"
    seriesLinesE :: [(D.P2 Double, D.Diagram DS.SVG)]
    seriesLinesE =
        [ ( D.p2 (0, 0)
          , D.stroke (D.fromVertices [D.p2 (xOfT t, yOfV v) | (t, v) <- s.seriesPoints] :: D.Path D.V2 Double)
                D.# D.lw D.veryThin
                D.# DS.svgClass (cs (lineCls i))
                D.# DS.svgTitle (cs (seriesTip s))
          )
        | (i, s) <- zip [0 ..] nonEmpty
        ]
    dotMarkersE :: [(D.P2 Double, D.Diagram DS.SVG)]
    dotMarkersE =
        [ ( D.p2 (xOfT t, yOfV v)
          , D.circle 2 D.# D.lw D.none D.# D.fc D.black D.# D.fillOpacity 1 D.# DS.svgClass (cs (dotCls i))
          )
        | (i, s) <- zip [0 ..] nonEmpty
        , length s.seriesPoints <= 25
        , (t, v) <- s.seriesPoints
        ]
    seriesTip s =
        let pts = map snd s.seriesPoints
         in s.seriesName
                <> ": "
                <> show (length s.seriesPoints)
                <> " points, "
                <> formatWithUnits s.seriesUnits (minimumDef 0 pts)
                <> " .. "
                <> formatWithUnits s.seriesUnits (maximumDef 0 pts)
    formatTick :: UTCTime -> Text
    -- Multi-day ranges (the past-week view) need the date, otherwise all
    -- ticks read as bare times.
    formatTick t
        | tSpan > 2 * 86400 = cs (TimeFormat.formatTime TimeFormat.defaultTimeLocale "%d.%m %H:%M" t)
        | otherwise = cs (TimeFormat.formatTime TimeFormat.defaultTimeLocale "%H:%M" t)

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

truncateLabel :: Int -> Text -> Text
truncateLabel maxChars label
    | Text.length label <= maxChars = label
    | otherwise = Text.take (maxChars - 1) label <> "…"

-- Grid line values for a resolved scale/domain: 1/2/5*10^k ticks for
-- base-10 logs, pure 2^k for base-2 logs (when enough exist inside the
-- domain), else even spacing in domain space.
gridValuesFor :: ScaleMode -> Double -> Double -> [Double]
gridValuesFor scale dLo dSpan =
    let logTicks = case scale of
            ScaleLog10 ->
                [ m * 10 ^^ k
                | k <- [floor dLo :: Int .. ceiling dHi]
                , m <- [1, 2, 5 :: Double]
                , let d = logBase 10 m + fromIntegral k
                , d > dLo + dSpan * 0.01
                , d < dHi - dSpan * 0.01
                ]
            ScaleLog2 ->
                [ 2 ^^ k
                | k <- [floor dLo :: Int .. ceiling dHi]
                , let d = fromIntegral k
                , d > dLo + dSpan * 0.01
                , d < dHi - dSpan * 0.01
                ]
            _ -> []
     in case scale of
            ScaleLinear -> evenSpacing
            _ | length logTicks >= 3 -> take 8 logTicks
            _ -> evenSpacing
  where
    dHi = dLo + dSpan
    evenSpacing = [domainToValue (dLo + dSpan * fromIntegral i / 4) | i <- [0 .. 4] :: [Int]]
    domainToValue d = case scale of
        ScaleLinear -> d
        ScaleLog2 -> 2 ** d
        _ -> 10 ** d

-- Value formatting with zabbix-style unit awareness. Bytes scale
-- 1024-based into KB..PB, seconds into s/m/h/d compounds, everything
-- else gets a compact plain number (K/M/G/T for magnitudes) with the
-- unit appended when known. Decimal only — `show 0.05` would render
-- "5.0e-2".
formatWithUnits :: Maybe Text -> Double -> Text
formatWithUnits mUnits v = case fmap Text.toLower mUnits of
    Just "b" -> scaledSuffix 1024 ["B", "KB", "MB", "GB", "TB", "PB"] v
    Just "bytes" -> scaledSuffix 1024 ["B", "KB", "MB", "GB", "TB", "PB"] v
    Just "s" -> humanSeconds v
    Just "uptime" -> humanSeconds v
    Just "%" -> formatPlain v <> "%"
    Just "" -> formatPlain v
    Just u -> formatPlain v <> " " <> u
    Nothing -> formatPlain v

formatPlain :: Double -> Text
formatPlain v
    | abs v >= 1e12 = trimZeros (Text.pack (printf "%.1fT" (v / 1e12)))
    | abs v >= 1e9 = trimZeros (Text.pack (printf "%.1fG" (v / 1e9)))
    | abs v >= 1e6 = trimZeros (Text.pack (printf "%.1fM" (v / 1e6)))
    | abs v >= 1e4 = trimZeros (Text.pack (printf "%.1fK" (v / 1e3)))
    | abs v >= 1000 = tshow (round v :: Integer)
    | abs v >= 1 = trimZeros (Text.pack (printf "%.2f" v))
    | otherwise = trimZeros (Text.pack (printf "%.4f" v))
  where
    trimZeros t =
        let t' = Text.dropWhileEnd (== '0') t
         in if Text.isSuffixOf "." t' then Text.dropEnd 1 t' else t'

-- CorePrelude's head/!! are Maybe-returning; pattern-match instead.
scaledSuffix :: Double -> [Text] -> Double -> Text
scaledSuffix _ suffixes v
    | v == 0 = case suffixes of (s0 : _) -> "0 " <> s0; [] -> "0"
scaledSuffix base suffixes v = case drop e suffixes of
    (s : _) -> trimZeros (Text.pack (printf "%.1f" scaled)) <> " " <> s
    [] -> trimZeros (Text.pack (printf "%.1f" v))
  where
    e = max 0 (min (length suffixes - 1) (floor (logBase base (abs v)) :: Int))
    scaled = v / (base ^^ e)
    trimZeros t =
        let t' = Text.dropWhileEnd (== '0') t
         in if Text.isSuffixOf "." t' then Text.dropEnd 1 t' else t'

humanSeconds :: Double -> Text
humanSeconds v
    | v < 0 = "-" <> humanSeconds (abs v)
    | v < 60 = formatPlain v <> "s"
    | v < 3600 = formatPlain (v / 60) <> "m"
    | v < 86400 = formatPlain (v / 3600) <> "h"
    | otherwise =
        let days = floor (v / 86400) :: Int
            hours = floor ((v - fromIntegral days * 86400) / 3600) :: Int
         in tshow days <> "d " <> tshow hours <> "h"

-- ---------------------------------------------------------------------------
-- Stacked vertical bar chart (the /reports volume widget)

-- | One colored segment of a stacked bar. The CSS class carries the theme
-- color (e.g. chart-sev-critical); the name feeds the tooltip.
data BarSegment = BarSegment
    { segCssClass :: Text
    , segName :: Text
    , segValue :: Int64
    }
    deriving (Eq, Show)

-- | One bar: label on the x axis, segments bottom-to-top (caller sorts).
data StackedBar = StackedBar
    { barLabel :: Text
    , barSegments :: [BarSegment]
    }
    deriving (Eq, Show)

-- | Stacked bar chart in a frameW x frameH frame: one bar per bucket, the
-- bucket total above each bar, a native <title> tooltip with the full
-- per-series breakdown, empty buckets rendered as baseline ticks. Buckets
-- with no bars at all render a "no data" placeholder.
stackedBarChartSvg :: Double -> Double -> [StackedBar] -> Text
stackedBarChartSvg frameW frameH bars =
    renderFluidSvg frameW frameH framed
  where
    framed = D.position elements D.<> frameBackdrop
    frameBackdrop =
        D.position
            [
                ( D.p2 (frameW / 2, frameH / 2)
                , D.rect frameW frameH D.# D.lw D.none D.# D.fcA D.transparent
                )
            ]
    elements :: [(D.P2 Double, D.Diagram DS.SVG)]
    elements = case bars of
        [] -> [(D.p2 (frameW / 2, frameH / 2), barText ("no data" :: Text) 12 "chart-text-muted" 0.5 0.5)]
        _ -> gridLinesE <> barsE <> xLabelsE

    -- frame geometry (diagrams coordinates, y up; svg flips on output):
    -- the mockup's svg-space baseline y=200 / top y=30 map to diagrams y=40
    -- and frameH-30 respectively.
    plotLeft = 40
    plotRight = frameW - 20
    baseline = 40
    plotTop = frameH - 30
    plotH = plotTop - baseline
    slot = (plotRight - plotLeft) / fromIntegral (length bars)
    barW = min 56 (slot * 0.7)
    pxPerUnit = plotH / fromIntegral (max 1 (maximum (map barTotal bars)))
    xCenter i = plotLeft + slot * (fromIntegral i + 0.5)
    barTotal bar = sum [segValue seg | seg <- barSegments bar]
    positive bar = [seg | seg <- barSegments bar, segValue seg > 0]
    -- Shrink labels until the longest one fits its slot (mono glyph advance
    -- ~0.6em), so dense buckets (24h "per hour" mode) never overlap.
    maxLabelChars = max 1 (maximum (map (Text.length . barLabel) bars))
    labelFont = max 8 (min 12 (slot * 0.9 / (0.6 * fromIntegral maxLabelChars)))
    maxTotalChars = max 1 (maximum (map (Text.length . tshow . barTotal) bars))
    totalFont = max 8 (min 12 (barW * 0.9 / (0.6 * fromIntegral maxTotalChars)))

    gridLinesE =
        [ ( D.p2 (0, 0)
          , segAbs (plotLeft, y) (plotRight, y) D.# D.lw D.thin D.# DS.svgClass "chart-grid"
          )
        | y <- [baseline + plotH * f | f <- [0, 1 / 3, 2 / 3] :: [Double]]
        ]
            <> [
                   ( D.p2 (0, 0)
                   , segAbs (plotLeft, baseline) (plotRight, baseline) D.# D.lw D.thin D.# DS.svgClass "chart-grid-strong"
                   )
               ]

    barsE = concat (zipWith barElement [0 ..] bars)

    barElement i bar
        | barTotal bar == 0 =
            [
                ( D.p2 (xCenter i, baseline - 1)
                , D.rect barW 2 D.# D.lw D.none D.# D.fc D.black D.# D.fillOpacity 1 D.# DS.svgClass "chart-tick"
                )
            ]
        | otherwise = [(D.p2 (0, 0), barGroup (xCenter i) bar)]

    -- segments + total label, each translated to its absolute spot (translate
    -- keeps content placement under mconcat; the group origin stays (0,0)).
    -- diagrams coords are y-up: segment bottoms ACCUMULATE upward from the
    -- baseline (the old svg-space formula stacked downward below the plot).
    barGroup cx bar =
        DS.svgTitle (cs (barTip bar)) (mconcat (segRects <> [totalLabelD]))
      where
        heights = [max 1.5 (fromIntegral (segValue seg) * pxPerUnit) | seg <- positive bar]
        yBottoms = scanl (+) baseline heights
        segRects =
            [ D.translate (D.r2 (cx, yBottom + h / 2)) segRect
            | (seg, h, yBottom) <- zip3 (positive bar) heights yBottoms
            , let segRect = D.roundedRect barW h 3 D.# D.lw D.none D.# D.fc D.black D.# D.fillOpacity 1 D.# DS.svgClass (cs (segCssClass seg))
            ]
        totalLabelD =
            D.translate
                (D.r2 (cx, min (frameH - 16) (baseline + sum heights + 8)))
                (barText (tshow (barTotal bar)) totalFont "chart-text-muted chart-text-c chart-mono" 0.5 0.5)
    barTip bar =
        Text.intercalate "\n" $
            [segName seg <> ": " <> tshow (segValue seg) | seg <- positive bar]
                <> ["total: " <> tshow (barTotal bar)]

    -- stroked absolute-coordinate segment, origin (0,0) (see recipe above)
    segAbs (x1, y1) (x2, y2) =
        D.stroke (D.fromVertices [D.p2 (x1, y1), D.p2 (x2, y2)] :: D.Path D.V2 Double)

    barText :: Text -> Double -> Text -> Double -> Double -> D.Diagram DS.SVG
    barText content sizePx cls ax ay =
        D.alignedText ax ay (cs content)
            D.# D.fontSizeL sizePx
            D.# DS.svgClass (cs cls)

    xLabelsE =
        [ ( D.p2 (xCenter i, 16)
          , barText (barLabel bar) labelFont "chart-text-muted chart-text-c chart-mono" 0.5 0.5
          )
        | (i, bar) <- zip [0 ..] bars
        ]
