module Application.Service.Chart (
    LineSeries (..),
    lineChartSvg,
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
    }
    deriving (Eq, Show)

-- | Multi-series line chart as a self-contained inline SVG.
lineChartSvg :: [LineSeries] -> Text
lineChartSvg series =
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
            _ -> gridLinesE <> axisLabelsE <> legendE <> seriesLinesE <> dotMarkersE

    -- frame geometry (diagrams coordinates, y up; svg flips on output)
    frameW = 900
    frameH = 250
    legendH = 24
    plotLeft = 60
    plotRight = frameW - 30
    plotTop = frameH - legendH - 8
    plotBottom = 34
    plotW = plotRight - plotLeft
    plotH = plotTop - plotBottom

    nonEmpty = [s | s <- series, not (null s.seriesPoints)]
    allPoints = concatMap seriesPoints nonEmpty
    ts = map (posix . fst) allPoints
    vs = map snd allPoints
    tMin = minimumDef 0 ts
    tMax = maximumDef 1 ts
    vMin = minimumDef 0 vs
    vMax = maximumDef 1 vs
    -- Degenerate domains (e.g. a single fresh sample) pad to a readable
    -- window instead of exploding through a tiny-span guard.
    tPad = max 30 ((tMax - tMin) * 0.05)
    tLo = tMin - tPad
    tHi = tMax + tPad
    vRange = vMax - vMin
    (vLo, vHi)
        | vRange < 1e-9 = (vMin - 1, vMax + 1)
        | otherwise = (vMin - vRange * 0.05, vMax + vRange * 0.05)
    tSpan = tHi - tLo
    vSpan = vHi - vLo
    xOfT t = plotLeft + (posix t - tLo) / tSpan * plotW
    yOfV v = plotBottom + (v - vLo) / vSpan * plotH
    gridValues = [vLo + vSpan * fromIntegral i / 4 | i <- [0 .. 4] :: [Int]]
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
        D.alignedText ax ay (cs content) D.# D.fontSizeL 12 D.# DS.svgClass "chart-text-muted"

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
        [ (D.p2 (plotLeft - 6, yOfV v), textD (formatValue v) 1 0.5)
        | v <- gridValues
        ]
            <> [ (D.p2 (xOfT t, 16), textD (formatTick t) 0.5 0.5)
               | t <- tickTimes
               ]
    legendE :: [(D.P2 Double, D.Diagram DS.SVG)]
    legendE =
        [ ( D.p2 (legendX i + 6, frameH - 13)
          , D.hcat
                [ D.rect 12 4 D.# D.lw D.none D.# D.fc D.black D.# D.fillOpacity 1 D.# DS.svgClass (cs (dotCls i))
                , D.strutX 4
                , textD (truncateLabel 28 s.seriesName) 0 0.5
                ]
          )
        | (i, s) <- zip [0 ..] nonEmpty
        ]
    legendX i = plotLeft + sum [legendWidth j + 24 | j <- [0 .. i - 1]]
    legendWidth j = 18 + 7 * fromIntegral (Text.length (truncateLabel 28 (nonEmpty !! j).seriesName))
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
                <> formatValue (minimumDef 0 pts)
                <> " .. "
                <> formatValue (maximumDef 0 pts)
    formatTick :: UTCTime -> Text
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

truncateLabel :: Int -> Text -> Text
truncateLabel maxChars label
    | Text.length label <= maxChars = label
    | otherwise = Text.take (maxChars - 1) label <> "…"

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
