module Application.Service.MemStats (
    logMemStats,
    formatMemStats,
    parseMemStatsFlag,
    formatBytes,
    MemSnapshot (..),
    snapshotFromRTS,
    recordMemSnapshot,
    MemStatLatest (..),
    latestMemStats,
    memStatsSeries,
) where

import Application.Service.Chart (LineSeries (..))
import Application.Service.Log (logInfo, logWarn)
import Control.Exception.Safe (try)
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import GHC.Stats (GCDetails (..), RTSStats (..))
import qualified GHC.Stats
import IHP.ModelSupport (ModelContext)
import IHP.Prelude
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
import System.Environment (lookupEnv)
import System.Log.FastLogger (FastLogger)

-- One-line RTS memory snapshot at the end of a heavy job window, for
-- correlating worker memory growth with the job that ran while it grew.
-- Writes a mem_stats_snapshots row (history behind the admin "Worker memory"
-- panel) and logs the same counters. Requires the -T RTS flag: compiled into
-- the nix prod binaries via ihp.rtsFlags in flake.nix (the prod build has no
-- -rtsopts, so runtime flags cannot be passed any other way); local runs can
-- pass +RTS -T. With -T missing (or HALEMANS_MEMSTATS=0) the call is a
-- silent no-op. The INSERT is failure-isolated on purpose: telemetry must
-- never kill the poll loop.
logMemStats :: (?modelContext :: ModelContext, ?context :: context, HasField "logger" context FastLogger) => Text -> IO ()
logMemStats label = do
    enabled <- parseMemStatsFlag . fmap cs <$> lookupEnv "HALEMANS_MEMSTATS"
    rtsEnabled <- GHC.Stats.getRTSStatsEnabled
    when (enabled && rtsEnabled) do
        stats <- GHC.Stats.getRTSStats
        let snapshot = snapshotFromRTS stats
        insertResult <- try (recordMemSnapshot label snapshot)
        case insertResult of
            Left err -> logWarn ("memstats: recording snapshot failed: " <> tshow (err :: SomeException))
            Right () -> pure ()
        logInfo (formatMemStats label snapshot)

-- | HALEMANS_MEMSTATS=0|false|no|off disables the snapshot (default enabled).
parseMemStatsFlag :: Maybe Text -> Bool
parseMemStatsFlag = \case
    Just value -> Text.toLower (Text.strip value) `notElem` ["0", "false", "no", "off"]
    Nothing -> True

-- | The counters we render and persist, kept as a flat record so formatting
-- stays pure and unit-testable without a live RTS.
data MemSnapshot = MemSnapshot
    { msLiveBytes :: Int64
    , msMaxLiveBytes :: Int64
    , msInUseBytes :: Int64
    , msAllocatedBytes :: Int64
    , msGcs :: Int64
    , msMajorGcs :: Int64
    , msGcElapsedNs :: Int64
    }

snapshotFromRTS :: RTSStats -> MemSnapshot
snapshotFromRTS RTSStats{..} =
    MemSnapshot
        { msLiveBytes = fromIntegral (gcdetails_live_bytes gc)
        , msMaxLiveBytes = fromIntegral max_live_bytes
        , msInUseBytes = fromIntegral (gcdetails_mem_in_use_bytes gc)
        , msAllocatedBytes = fromIntegral allocated_bytes
        , msGcs = fromIntegral gcs
        , msMajorGcs = fromIntegral major_gcs
        , msGcElapsedNs = gc_elapsed_ns
        }

recordMemSnapshot :: (?modelContext :: ModelContext) => Text -> MemSnapshot -> IO ()
recordMemSnapshot label MemSnapshot{..} =
    void
        ( sqlExecTyped
            [typedSql|
            INSERT INTO mem_stats_snapshots (label, live_bytes, max_live_bytes, in_use_bytes, allocated_bytes, gcs, major_gcs)
            VALUES (${label}, ${msLiveBytes}, ${msMaxLiveBytes}, ${msInUseBytes}, ${msAllocatedBytes}, ${msGcs}, ${msMajorGcs})
        |]
        )

formatMemStats :: Text -> MemSnapshot -> Text
formatMemStats label MemSnapshot{..} =
    "memstats "
        <> label
        <> " live="
        <> formatBytes msLiveBytes
        <> " maxLive="
        <> formatBytes msMaxLiveBytes
        <> " inUse="
        <> formatBytes msInUseBytes
        <> " allocTotal="
        <> gib msAllocatedBytes
        <> " gcs="
        <> tshow msGcs
        <> "/"
        <> tshow msMajorGcs
        <> " gcWall="
        <> millis msGcElapsedNs
  where
    gib bytes = tshow (round (fromIntegral bytes / (1024 * 1024 * 1024) :: Double)) <> "GiB"
    millis ns = tshow (round (fromIntegral ns / 1_000_000 :: Double)) <> "ms"

-- | Human-readable byte size (MiB/GiB for big values, KiB below 1 MiB) —
-- shared by the log line and the admin panel.
formatBytes :: Int64 -> Text
formatBytes bytes
    | bytes >= 1024 * 1024 * 1024 = tshow (round (fromIntegral bytes / (1024 * 1024 * 1024) :: Double)) <> "GiB"
    | bytes >= 1024 * 1024 = tshow (round (fromIntegral bytes / (1024 * 1024) :: Double)) <> "MiB"
    | otherwise = tshow (round (fromIntegral bytes / 1024 :: Double)) <> "KiB"

-- | Latest snapshot per label (DISTINCT ON keeps one row per label).
data MemStatLatest = MemStatLatest
    { mstLabel :: Text
    , mstLiveBytes :: Int64
    , mstMaxLiveBytes :: Int64
    , mstInUseBytes :: Int64
    , mstAllocatedBytes :: Int64
    , mstGcs :: Int64
    , mstMajorGcs :: Int64
    , mstCreatedAt :: UTCTime
    }

latestMemStats :: (?modelContext :: ModelContext) => IO [MemStatLatest]
latestMemStats = do
    rows <-
        sqlQueryTyped
            [typedSql|
            SELECT DISTINCT ON (label) label, live_bytes, max_live_bytes, in_use_bytes, allocated_bytes, gcs, major_gcs, created_at
            FROM mem_stats_snapshots
            ORDER BY label, created_at DESC
        |]
    pure
        [ MemStatLatest
            (get #label row)
            (get #live_bytes row)
            (get #max_live_bytes row)
            (get #in_use_bytes row)
            (get #allocated_bytes row)
            (get #gcs row)
            (get #major_gcs row)
            (get #created_at row)
        | row <- rows
        ]

-- | Per-label live-bytes series bucketed to `bucketSeconds` over the last
-- `horizon` — the admin "Worker memory" chart. Labels with no rows in the
-- window simply produce no series.
memStatsSeries :: (?modelContext :: ModelContext) => Double -> NominalDiffTime -> IO [LineSeries]
memStatsSeries bucketSeconds horizon = do
    cutoff <- addUTCTime (negate horizon) <$> getCurrentTime
    rows <-
        sqlQueryTyped
            [typedSql|
            SELECT label,
                   to_timestamp(floor(extract(epoch FROM created_at)::float8 / ${bucketSeconds}::float8) * ${bucketSeconds}::float8) AS bucket,
                   avg(live_bytes)::float8 AS live_avg
            FROM mem_stats_snapshots
            WHERE created_at > ${cutoff}
            GROUP BY label, bucket
            ORDER BY bucket
        |]
    let byLabel =
            Map.toList
                ( Map.fromListWith
                    (flip (<>))
                    [ (get #label row, [(bucket, live)])
                    | row <- rows
                    , Just bucket <- [get #bucket row]
                    , Just live <- [get #live_avg row]
                    ]
                )
    pure [LineSeries label points (Just "B") | (label, points) <- byLabel]
