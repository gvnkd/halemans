module Application.Service.MetricChart (
    MetricSeries,
    MetricWindow (..),
    MetricChartData (..),
    MetricSeriesInfo (..),
    metricSeriesInfo,
    seriesChartSvg,
    chartDataSvg,
    chartHoverJson,
    chartRenderMeta,
    metricWindowFor,
    metricWindowForRange,
    parseScaleParam,
    fetchAlertMetricSeries,
    thresholdsFromExpression,
    downsample,
    missingRanges,
) where

import Application.Connector.GrafanaMetrics (
    MetricSeries (..),
    buildExploreUrl,
    datasourceTypeGet,
    dsQueryRange,
    ruleQueryGet,
    ruleUidFromSourceUrl,
 )
import Application.Connector.Zabbix (ZabbixTriggerItem (..), historyGet, triggerItemsGet)
import qualified Application.Service.Chart as Chart
import Application.Service.MetricCache (readCachedSeries, storeSeries)
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (FromJSON, parseMaybe)
import Data.Char (isDigit)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Format as TimeFormat
import qualified Data.Vector as Vector
import Generated.Types
import IHP.ModelSupport (Id' (..), ModelContext)
import IHP.Prelude
import System.Environment (lookupEnv)
import Text.Read (readMaybe, reads)
import qualified "cryptonite" Crypto.Hash as CryptoHash

-- Alert detail metric chart. Series come from the alert's own rule re-run as
-- a range query (see Application.Connector.GrafanaMetrics); rendering follows
-- Application.Service.Reports conventions: pixel grid, theme via CSS classes
-- (chart-*), inline SVG, native <title> tooltips.

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
    lead = fromMaybe 60 (metricsCfg source "leadMinutes")
    trail = fromMaybe 15 (metricsCfg source "trailMinutes")
    maxPoints = fromMaybe 500 (metricsCfg source "maxPoints")
    to = case alert.resolvedAt of
        Just resolvedAt -> min now (addUTCTime (trail * 60) resolvedAt)
        Nothing -> now

-- SRE-selected relative windows (the chart's range control): "1h", "6h",
-- "24h", "7d" end at now; anything else (including "alert" and missing)
-- falls back to the alert-centric metricWindowFor.
metricWindowForRange :: Source -> Alert -> UTCTime -> Text -> MetricWindow
metricWindowForRange source alert now range = case range of
    "1h" -> rel 1
    "6h" -> rel 6
    "24h" -> rel 24
    "7d" -> rel (7 * 24)
    _ -> metricWindowFor source alert now
  where
    rel hours = MetricWindow (addUTCTime (negate (fromIntegral hours * 3600)) now) now maxPoints
    maxPoints = fromMaybe 500 (metricsCfg source "maxPoints")

-- The chart's scale control: "linear"/"log10"/"log2" select a mode
-- explicitly ("log" is accepted as log10), everything else
-- (missing/unknown/"auto") lets the widget pick from the data spread.
parseScaleParam :: Maybe Text -> Chart.ScaleMode
parseScaleParam (Just "linear") = Chart.ScaleLinear
parseScaleParam (Just "log10") = Chart.ScaleLog10
parseScaleParam (Just "log2") = Chart.ScaleLog2
parseScaleParam (Just "log") = Chart.ScaleLog10
parseScaleParam _ = Chart.ScaleAuto

-- One plotted series plus its zabbix item units ("B", "s", "%"), when the
-- source reports them — the chart formats axis/tooltip values with them.
data MetricSeriesInfo = MetricSeriesInfo
    { msiSeries :: MetricSeries
    , msiUnits :: Maybe Text
    }
    deriving (Eq, Show)

metricSeriesInfo :: MetricSeries -> Maybe Text -> MetricSeriesInfo
metricSeriesInfo = MetricSeriesInfo

-- | Series plus reference lines for the alert's chart. mcdLinks are
-- (label, url) pairs rendered above the chart — the upstream graph/explore
-- deep links (zabbix item graphs, grafana explore).
data MetricChartData = MetricChartData
    { mcdSeries :: [MetricSeriesInfo]
    , mcdThresholds :: [Chart.ChartThreshold]
    , mcdLinks :: [(Text, Text)]
    }
    deriving (Eq, Show)

-- Per-source overrides in sources.config.metrics (documented in
-- design_docs/milestone_13.md): leadMinutes, trailMinutes, maxPoints,
-- maxSeries (default 8, grafana only), cacheRetentionDays (default 7),
-- freshenSeconds (default 60).
metricsCfg :: (FromJSON a) => Source -> Text -> Maybe a
metricsCfg source key = join results
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

-- | Fetch the data backing the alert's chart, source-type specific:
-- grafana resolves the alert rule's expr through the datasource proxy,
-- zabbix resolves the trigger's items and pulls raw history.get (no
-- trends.get needed) plus the trigger expression for threshold lines.
-- Both paths cache hourly buckets locally
-- (Application.Service.MetricCache) and only re-fetch uncovered
-- sub-ranges. Fails cleanly (Left) — the widget renders the message.
fetchAlertMetricSeries :: (?modelContext :: ModelContext) => Source -> Alert -> MetricWindow -> IO (Either Text MetricChartData)
fetchAlertMetricSeries source alert window = do
    outcome <- try (fetchAlertMetricSeriesUnchecked source alert window)
    pure case outcome of
        Left ex -> Left (tshow (ex :: SomeException))
        Right result -> result

fetchAlertMetricSeriesUnchecked :: (?modelContext :: ModelContext) => Source -> Alert -> MetricWindow -> IO (Either Text MetricChartData)
fetchAlertMetricSeriesUnchecked source alert window = case source.type_ of
    "grafana" -> do
        token <- tokenFromEnv
        case (token, ruleUidFromSourceUrl =<< alert.sourceUrl) of
            (Nothing, _) -> pure (Left "No Grafana token configured (sources.config.tokenEnv)")
            (_, Nothing) -> pure (Left "Alert has no Grafana rule link")
            (Just token, Just ruleUid) -> do
                ruleResult <- ruleQueryGet source.baseUrl token ruleUid
                case ruleResult of
                    Left err -> pure (Left err)
                    Right (datasourceUid, expr) -> do
                        let digest = tshow (CryptoHash.hashWith CryptoHash.SHA256 (cs expr :: ByteString))
                        seriesResult <-
                            forEachSeries
                                source
                                (\name -> "g:" <> datasourceUid <> ":" <> digest <> ":" <> name)
                                (\from to -> dsQueryRange source.baseUrl token datasourceUid expr from to window.mwMaxPoints)
                                window
                        case seriesResult of
                            Left err -> pure (Left err)
                            Right series -> do
                                dsType <- datasourceTypeGet source.baseUrl token datasourceUid
                                let exploreUrl = buildExploreUrl source.baseUrl datasourceUid dsType expr
                                pure (Right (MetricChartData [metricSeriesInfo s Nothing | s <- series] [] [("Explore in Grafana", exploreUrl)]))
    "zabbix" -> do
        token <- tokenFromEnv
        case token of
            Nothing -> pure (Left "No Zabbix token configured (sources.config.tokenEnv)")
            Just token -> case Text.stripPrefix "zabbix:trigger:" alert.fingerprint of
                Nothing -> pure (Left "Alert fingerprint is not zabbix trigger-scoped")
                Just triggerId -> do
                    itemsResult <- triggerItemsGet source.baseUrl token [triggerId]
                    case itemsResult of
                        Left err -> pure (Left err)
                        Right items -> do
                            let numericItems = [item | item <- items, item.ztiValueType `elem` (["0", "3"] :: [Text])]
                            case numericItems of
                                [] -> pure (Left "No numeric metric for this trigger")
                                _ -> do
                                    let thresholds =
                                            [ Chart.ChartThreshold v ("trigger threshold " <> formatThreshold v)
                                            | v <- nub (concatMap (thresholdsFromExpression . ztiExpression) items)
                                            ]
                                    perItem <- forM numericItems \item -> do
                                        pointsResult <-
                                            cachedSeries
                                                source
                                                ("z:" <> item.ztiItemId)
                                                window
                                                (\from to -> historyGet source.baseUrl token item.ztiItemId (historyTable item) (posixFloor from) (posixCeil to) historyPageLimit)
                                        pure ((\points -> metricSeriesInfo (MetricSeries (seriesLabel item) (limitPoints window points)) (itemUnits item)) <$> pointsResult)
                                    let links =
                                            [ ("Zabbix graph", source.baseUrl <> "/history.php?action=showgraph" <> Text.concat ["&itemids[]=" <> item.ztiItemId | item <- numericItems])
                                            ]
                                    pure ((\infos -> MetricChartData infos thresholds links) <$> sequenceEither perItem)
    _ -> pure (Left "Metrics are only available for Grafana- and Zabbix-sourced alerts")
  where
    tokenFromEnv :: IO (Maybe Text)
    tokenFromEnv = case tokenEnv of
        Just envVar -> fmap cs <$> lookupEnv (cs envVar)
        Nothing -> pure Nothing
    tokenEnv :: Maybe Text
    tokenEnv = parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: "tokenEnv")) source.config
    historyTable item = if item.ztiValueType == "3" then 3 else 0 :: Int
    historyPageLimit = 10000
    seriesLabel item =
        if Text.null item.ztiUnits
            then item.ztiName
            else item.ztiName <> " (" <> item.ztiUnits <> ")"
    -- Zabbix "B" units mean bytes; "" means plain. Anything else ("s",
    -- "ms", "bps", custom) passes through to the formatter.
    itemUnits item = if Text.null item.ztiUnits then Nothing else Just item.ztiUnits

-- One seed fetch covers the whole window and doubles as the gap-filler: the
-- per-series cache merge draws missing sub-ranges from the seed instead of
-- re-running the query per series. The old design re-fetched the full
-- multi-frame response once per series, which is quadratic in the rule's
-- frame count and OOMs the server on many-series exprs. The chart keeps at
-- most maxSeries (sources.config.metrics.maxSeries, default 8) — a line
-- chart can't usefully show more anyway. Left short-circuits the widget.
forEachSeries ::
    (?modelContext :: ModelContext) =>
    Source ->
    (Text -> Text) ->
    (UTCTime -> UTCTime -> IO (Either Text [MetricSeries])) ->
    MetricWindow ->
    IO (Either Text [MetricSeries])
forEachSeries source seriesKey fetchRange window = do
    seed <- fetchRange window.mwFrom window.mwTo
    case seed of
        Left err -> pure (Left err)
        Right allSeries -> do
            let series = take maxSeries allSeries
            perSeries <- forM series \s -> do
                pointsResult <-
                    cachedSeries
                        source
                        (seriesKey s.seriesName)
                        window
                        (\from to -> pure (Right [p | p <- s.seriesPoints, fst p >= from, fst p <= to]))
                pure ((\points -> s{seriesPoints = limitPoints window points}) <$> pointsResult)
            pure (sequenceEither perSeries)
  where
    maxSeries = fromMaybe 8 (metricsCfg source "maxSeries")

-- Read cached coverage for [from, to], fetch only the missing sub-ranges,
-- merge, and store the freshly fetched points. Closed buckets are immutable;
-- the bucket containing now is trusted only within the freshen window.
cachedSeries ::
    (?modelContext :: ModelContext) =>
    Source ->
    Text ->
    MetricWindow ->
    (UTCTime -> UTCTime -> IO (Either Text [(UTCTime, Double)])) ->
    IO (Either Text [(UTCTime, Double)])
cachedSeries source seriesKey window fetchRange = do
    let freshen = fromMaybe 60 (metricsCfg source "freshenSeconds")
    cached <- readCachedSeries (sourceUuid source) seriesKey window.mwFrom window.mwTo freshen
    let gaps = missingRanges maxGap window.mwFrom window.mwTo cached
    fetchedResults <- forM gaps (uncurry fetchRange)
    case [err | Left err <- fetchedResults] of
        (err : _) -> pure (Left err)
        [] -> do
            let fetched = concat [points | Right points <- fetchedResults]
                merged = dedupPoints (cached ++ fetched)
            storeSeries (sourceUuid source) seriesKey fetched retentionDays
            pure (Right merged)
  where
    retentionDays = fromMaybe 7 (metricsCfg source "cacheRetentionDays")
    maxGap = 120 -- sampling slower than this breaks one run into two
    sourceUuid s = case s.id of
        Id uuid -> uuid

-- Complement of the cached point coverage within [from, to]: cached points
-- are grouped into maximal runs (consecutive spacing <= maxGap) and the
-- returned intervals are what still needs an upstream fetch.
missingRanges :: NominalDiffTime -> UTCTime -> UTCTime -> [(UTCTime, Double)] -> [(UTCTime, UTCTime)]
missingRanges maxGap from to cached = go from (map runRange (toRuns (filter inRange cached)))
  where
    inRange (t, _) = t >= from && t <= to
    toRuns :: [(UTCTime, Double)] -> [[(UTCTime, Double)]]
    toRuns [] = []
    toRuns (p : ps) = reverse run : restRuns
      where
        (run, rest) = spanAdjacent [p] ps
        restRuns = case rest of
            [] -> []
            (q : qs) -> toRuns (q : qs)
        spanAdjacent :: [(UTCTime, Double)] -> [(UTCTime, Double)] -> ([(UTCTime, Double)], [(UTCTime, Double)])
        spanAdjacent acc [] = (acc, [])
        spanAdjacent acc@(a : _) (q : qs)
            | fst q `diffUTCTime` fst a <= maxGap = spanAdjacent (q : acc) qs
            | otherwise = (acc, q : qs)
        spanAdjacent [] qs = ([], qs)
    runRange :: [(UTCTime, Double)] -> (UTCTime, UTCTime)
    runRange run = case run of
        (firstPoint : _) -> (fst firstPoint, fst (lastOf run))
        [] -> (from, to) -- unreachable: runs start non-empty
      where
        lastOf [single] = single
        lastOf (_ : more) = lastOf more
        lastOf [] = error "runRange: empty run"
    go cursor []
        | to `diffUTCTime` cursor > maxGap = [(cursor, to)]
        | otherwise = []
    go cursor ((runFrom, runTo) : rest) =
        [(cursor, runFrom) | runFrom `diffUTCTime` cursor > maxGap] ++ go (max cursor runTo) rest

sequenceEither :: [Either a b] -> Either a [b]
sequenceEither = go []
  where
    go acc (Right x : rest) = go (acc ++ [x]) rest
    go _ (Left e : _) = Left e
    go acc [] = Right acc

limitPoints :: MetricWindow -> [(UTCTime, Double)] -> [(UTCTime, Double)]
limitPoints window = downsample window.mwMaxPoints

-- | Fixed-count time-bucket downsampling (mean per bucket), so a week of
-- 1-minute samples renders at maxPoints points without trends.get. Single
-- strict pass over (sum, count) buckets — the previous fromListWith (++)
-- copy-accumulated per bucket, which is quadratic on week-range zabbix
-- history.
downsample :: Int -> [(UTCTime, Double)] -> [(UTCTime, Double)]
downsample maxPoints points
    | maxPoints <= 0 = points
    | length points <= maxPoints = points
    | otherwise =
        [ (fromPosix (tMin + (fromIntegral idx + 0.5) * bucketDur), s / fromIntegral c)
        | (idx, (s, c)) <- IntMap.toList buckets
        ]
  where
    times = map fst points
    tMin = posix (minimum times)
    tMax = posix (maximum times)
    bucketDur = max 1e-9 ((tMax - tMin) / fromIntegral maxPoints)
    bucketIdx t = min (maxPoints - 1) (floor ((posix t - tMin) / bucketDur) :: Int)
    buckets :: IntMap.IntMap (Double, Int)
    buckets =
        IntMap.fromListWithKey
            (\_ (s1, c1) (s2, c2) -> (s1 + s2, c1 + c2))
            [(bucketIdx t, (v, 1)) | (t, v) <- points]

-- nubBy is quadratic on sorted input; week-range history runs into hundreds
-- of thousands of points. First occurrence wins (cached beats freshly
-- fetched), matching the old nubBy-after-sort semantics.
dedupPoints :: [(UTCTime, Double)] -> [(UTCTime, Double)]
dedupPoints = Map.toAscList . Map.fromListWith (\new old -> old)

posixFloor :: UTCTime -> Integer
posixFloor t = floor (POSIX.utcTimeToPOSIXSeconds t)

posixCeil :: UTCTime -> Integer
posixCeil t = ceiling (POSIX.utcTimeToPOSIXSeconds t)

-- | Multi-series line chart, rendered by the standard chart widget
-- Application.Service.Chart (fixed frame, CSS-class theme, tooltips).
seriesChartSvg :: [MetricSeries] -> Text
seriesChartSvg series =
    Chart.lineChartSvg [Chart.LineSeries s.seriesName s.seriesPoints Nothing | s <- series]

-- | Full chart render: series plus thresholds, with the requested scale.
chartDataSvg :: Chart.ScaleMode -> MetricChartData -> Text
chartDataSvg scale data_ =
    Chart.lineChartSvgWith (chartOptions scale data_) lineSeries
  where
    lineSeries = toLineSeries data_

-- | Hover-tooltip payload: per series the points as [epochSeconds, raw,
-- display] triples (display is the unit-aware formatted value, so the
-- browser shows "5.3 GB" without unit logic in JS) plus the chart layout
-- (scale, padded domains, plot strip) so the cursor position maps back to
-- the exact server-rendered coordinates for hit-testing.
chartHoverJson :: MetricChartData -> Text
chartHoverJson data_ =
    cs (Aeson.encode [seriesJson s | s <- data_.mcdSeries])
  where
    seriesJson s =
        Aeson.object
            [ "name" Aeson..= s.msiSeries.seriesName
            , "points" Aeson..= map (pointJson s.msiUnits) s.msiSeries.seriesPoints
            ]
    pointJson units (t, v) =
        Aeson.Array
            ( Vector.fromList
                [ Aeson.toJSON (posix t)
                , Aeson.toJSON v
                , Aeson.toJSON (Chart.formatWithUnits units v)
                ]
            )

-- (scale text, padded time domain, padded value domain, plot x strip)
chartRenderMeta :: Chart.ScaleMode -> MetricChartData -> (Text, (Double, Double), (Double, Double), (Double, Double))
chartRenderMeta scale data_ =
    (scaleText layout.clScale, layout.clTimeDomain, layout.clValueDomain, (layout.clPlotLeft, layout.clPlotRight))
  where
    layout = Chart.lineChartLayout (chartOptions scale data_) (toLineSeries data_)
    scaleText Chart.ScaleLinear = "linear"
    scaleText Chart.ScaleLog10 = "log10"
    scaleText Chart.ScaleLog2 = "log2"
    scaleText Chart.ScaleAuto = "linear"

chartOptions :: Chart.ScaleMode -> MetricChartData -> Chart.LineChartOptions
chartOptions scale data_ = Chart.LineChartOptions scale data_.mcdThresholds

toLineSeries :: MetricChartData -> [Chart.LineSeries]
toLineSeries data_ = [Chart.LineSeries s.msiSeries.seriesName s.msiSeries.seriesPoints s.msiUnits | s <- data_.mcdSeries]

-- Threshold constants out of a zabbix trigger expression. Expressions
-- compare item functions against constants: "last(/h/k)>70",
-- "{h:k.last()}>=80 and {h:k2.avg(5m)}<5"; the constants become reference
-- lines. {$MACRO} right-hand sides are unresolvable here and skipped, as
-- are non-numeric string comparisons (={#...}).
thresholdsFromExpression :: Text -> [Double]
thresholdsFromExpression = go
  where
    go :: Text -> [Double]
    go t = case Text.uncons t of
        Nothing -> []
        Just (c, rest)
            | c == '>' || c == '<' || c == '=' ->
                let (afterOp, consumedTwo) = case Text.uncons rest of
                        Just ('=', after) -> (after, True)
                        Just ('>', after) | c == '<' -> (after, True) -- <>
                        _ -> (rest, False)
                    -- "=#..." is a string comparison, not numeric
                    isStringEq = c == '=' && not consumedTwo && maybe False ((== '#') . fst) (Text.uncons afterOp)
                 in (if isStringEq then [] else parseConstant afterOp) ++ go afterOp
            | otherwise -> go rest
    parseConstant :: Text -> [Double]
    parseConstant t0 =
        let t1 = Text.dropWhile isSpaceText t0
         in case Text.uncons t1 of
                Just (c, _)
                    | isDigitChar c || c == '-' || c == '.' ->
                        let (numText, afterNum) = Text.span isNumberChar t1
                            (mult, _) = Text.span isSuffixChar afterNum
                         in case readScaled numText mult of
                                Just v -> [v]
                                Nothing -> []
                _ -> []
    isSpaceText c = c == ' ' || c == '\t'
    isDigitChar c = isDigit c
    isNumberChar c = isDigitChar c || c == '.' || c == '-' || c == '+'
    isSuffixChar c = c == 'K' || c == 'M' || c == 'G' || c == 'T'
    readScaled :: Text -> Text -> Maybe Double
    readScaled numText suffix = do
        base <- case reads (Text.unpack numText) of
            [(v, "")] -> Just (v :: Double)
            _ -> Nothing
        pure (base * multiplier suffix)
    multiplier :: Text -> Double
    multiplier "K" = 1e3
    multiplier "M" = 1e6
    multiplier "G" = 1e9
    multiplier "T" = 1e12
    multiplier _ = 1

formatThreshold :: Double -> Text
formatThreshold v
    | v == fromIntegral (round v :: Integer) = tshow (round v :: Integer)
    | otherwise = tshow v

posix :: UTCTime -> Double
posix = realToFrac . POSIX.utcTimeToPOSIXSeconds

fromPosix :: Double -> UTCTime
fromPosix = POSIX.posixSecondsToUTCTime . realToFrac
