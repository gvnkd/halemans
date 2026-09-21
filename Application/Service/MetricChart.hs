module Application.Service.MetricChart (
    MetricSeries,
    MetricWindow (..),
    seriesChartSvg,
    metricWindowFor,
    fetchAlertMetricSeries,
    downsample,
    missingRanges,
) where

import Application.Connector.GrafanaMetrics (
    MetricSeries (..),
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
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Format as TimeFormat
import Generated.Types
import IHP.ModelSupport (Id' (..), ModelContext)
import IHP.Prelude
import System.Environment (lookupEnv)
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

-- Per-source overrides in sources.config.metrics (documented in
-- design_docs/milestone_13.md): leadMinutes, trailMinutes, maxPoints,
-- cacheRetentionDays (default 7), freshenSeconds (default 60).
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

-- | Fetch the series backing the alert's chart, source-type specific:
-- grafana resolves the alert rule's expr through the datasource proxy,
-- zabbix resolves the trigger's items and pulls raw history.get (no
-- trends.get needed). Both paths cache hourly buckets locally
-- (Application.Service.MetricCache) and only re-fetch uncovered
-- sub-ranges. Fails cleanly (Left) — the widget renders the message.
fetchAlertMetricSeries :: (?modelContext :: ModelContext) => Source -> Alert -> MetricWindow -> IO (Either Text [MetricSeries])
fetchAlertMetricSeries source alert window = do
    outcome <- try (fetchAlertMetricSeriesUnchecked source alert window)
    pure case outcome of
        Left ex -> Left (tshow (ex :: SomeException))
        Right result -> result

fetchAlertMetricSeriesUnchecked :: (?modelContext :: ModelContext) => Source -> Alert -> MetricWindow -> IO (Either Text [MetricSeries])
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
                        forEachSeries
                            source
                            (\name -> "g:" <> datasourceUid <> ":" <> digest <> ":" <> name)
                            (\from to -> dsQueryRange source.baseUrl token datasourceUid expr from to window.mwMaxPoints)
                            window
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
                                    perItem <- forM numericItems \item -> do
                                        pointsResult <-
                                            cachedSeries
                                                source
                                                ("z:" <> item.ztiItemId)
                                                window
                                                (\from to -> historyGet source.baseUrl token item.ztiItemId (historyTable item) (posixFloor from) (posixCeil to) historyPageLimit)
                                        pure ((\points -> MetricSeries (seriesLabel item) (limitPoints window points)) <$> pointsResult)
                                    pure (sequenceEither perItem)
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

-- Grafana frames arrive as several named series; each gets its own cache
-- key, gap fetch, and merge. Left short-circuits the whole widget.
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
        Right series -> do
            perSeries <- forM series \s -> do
                pointsResult <-
                    cachedSeries
                        source
                        (seriesKey s.seriesName)
                        window
                        ( \from to ->
                            fetchRange from to >>= \case
                                Left err -> pure (Left err)
                                Right fetched -> pure (Right (concat [f.seriesPoints | f <- fetched, f.seriesName == s.seriesName]))
                        )
                pure ((\points -> s{seriesPoints = limitPoints window points}) <$> pointsResult)
            pure (sequenceEither perSeries)

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
                merged = nubBy (\a b -> fst a == fst b) (sortOn fst (cached ++ fetched))
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
-- 1-minute samples renders at maxPoints points without trends.get.
downsample :: Int -> [(UTCTime, Double)] -> [(UTCTime, Double)]
downsample maxPoints points
    | maxPoints <= 0 = points
    | length points <= maxPoints = points
    | otherwise =
        [ (fromPosix (tMin + (fromIntegral idx + 0.5) * bucketDur), avg vals)
        | (idx, vals) <- IntMap.toList (IntMap.fromListWith (++) [(bucketIdx t, [v]) | (t, v) <- points])
        ]
  where
    times = map fst points
    tMin = posix (minimum times)
    tMax = posix (maximum times)
    bucketDur = max 1e-9 ((tMax - tMin) / fromIntegral maxPoints)
    bucketIdx t = min (maxPoints - 1) (floor ((posix t - tMin) / bucketDur) :: Int)
    avg vals = sum vals / fromIntegral (length vals)

posixFloor :: UTCTime -> Integer
posixFloor t = floor (POSIX.utcTimeToPOSIXSeconds t)

posixCeil :: UTCTime -> Integer
posixCeil t = ceiling (POSIX.utcTimeToPOSIXSeconds t)

-- | Multi-series line chart, rendered by the standard chart widget
-- Application.Service.Chart (fixed frame, CSS-class theme, tooltips).
seriesChartSvg :: [MetricSeries] -> Text
seriesChartSvg series =
    Chart.lineChartSvg [Chart.LineSeries s.seriesName s.seriesPoints | s <- series]

posix :: UTCTime -> Double
posix = realToFrac . POSIX.utcTimeToPOSIXSeconds

fromPosix :: Double -> UTCTime
fromPosix = POSIX.posixSecondsToUTCTime . realToFrac
