module Application.Service.MetricCache (
    readCachedSeries,
    storeSeries,
) where

import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import qualified Data.Vector as Vector
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.TypedSql

-- Local metric cache (milestone 13): hourly buckets of fetched series in the
-- generalized [MetricSeries] point shape. Closed buckets (bucket_start + 1h
-- <= now) are immutable and served forever; the bucket containing now is
-- volatile and only trusted within the freshen window. Retention is
-- opportunistic delete-on-store per source, driven by the caller.

-- | Points in [from, to] from FRESH buckets only. Stale-bucket points are
-- dropped so the caller re-fetches those ranges upstream.
readCachedSeries :: (?modelContext :: ModelContext) => UUID -> Text -> UTCTime -> UTCTime -> NominalDiffTime -> IO [(UTCTime, Double)]
readCachedSeries sourceId seriesKey from to freshen = do
    now <- getCurrentTime
    rows <-
        sqlQueryTyped
            [typedSql| SELECT bucket_start, points, fetched_at FROM metric_cache
        WHERE source_id = ${sourceId} AND series_key = ${seriesKey}
          AND bucket_start >= ${bucketFloor from} AND bucket_start <= ${bucketFloor to}
        ORDER BY bucket_start ASC |]
    let freshRows = [row | row <- rows, isFresh now (get #bucket_start row) (get #fetched_at row)]
    pure
        [ (t, v)
        | row <- freshRows
        , (t, v) <- decodePoints (get #points row)
        , t >= from
        , t <= to
        ]
  where
    isFresh now bucketStart fetchedAt =
        addUTCTime 3600 bucketStart <= now || addUTCTime freshen fetchedAt >= now

-- | Split points into hourly buckets and upsert them. Then opportunistic
-- retention delete for this source (days = 0 or negative disables).
storeSeries :: (?modelContext :: ModelContext) => UUID -> Text -> [(UTCTime, Double)] -> Int -> IO ()
storeSeries sourceId seriesKey points retentionDays = do
    forEach (bucketGroups points) \(bucketStart, bucketPoints) -> do
        let payload = encodePoints bucketPoints
        void
            ( sqlExecTyped
                [typedSql| INSERT INTO metric_cache (source_id, series_key, bucket_start, points)
                    VALUES (${sourceId}, ${seriesKey}, ${bucketStart}, ${payload})
                    ON CONFLICT (source_id, series_key, bucket_start)
                    DO UPDATE SET points = EXCLUDED.points, fetched_at = NOW() |]
            )
    when (retentionDays > 0) do
        now <- getCurrentTime
        let cutoff = addUTCTime (negate (fromIntegral retentionDays * 86400)) now
        void (sqlExecTyped [typedSql| DELETE FROM metric_cache WHERE source_id = ${sourceId} AND fetched_at < ${cutoff} |])
  where
    bucketGroups =
        map (\grp@((t, _) : _) -> (bucketFloor t, grp))
            . groupBy (\(a, _) (b, _) -> bucketFloor a == bucketFloor b)
            . sortOn fst

bucketFloor :: UTCTime -> UTCTime
bucketFloor t = posixSecondsToUTCTime (fromIntegral hourStart)
  where
    posix = utcTimeToPOSIXSeconds t :: NominalDiffTime
    hourStart = 3600 * floor (posix / 3600) :: Integer

encodePoints :: [(UTCTime, Double)] -> Aeson.Value
encodePoints points =
    Aeson.Array
        ( Vector.fromList
            [ Aeson.Array
                ( Vector.fromList
                    [ Aeson.Number (realToFrac (utcTimeToPOSIXSeconds t))
                    , Aeson.Number (realToFrac v)
                    ]
                )
            | (t, v) <- points
            ]
        )

decodePoints :: Aeson.Value -> [(UTCTime, Double)]
decodePoints (Aeson.Array rows) = mapMaybe decodePair (Vector.toList rows)
decodePoints _ = []

decodePair :: Aeson.Value -> Maybe (UTCTime, Double)
decodePair (Aeson.Array arr) =
    case mapMaybe decodeNumber (Vector.toList arr) of
        [secs, v] -> Just (posixSecondsToUTCTime (realToFrac secs), v)
        _ -> Nothing
decodePair _ = Nothing

decodeNumber :: Aeson.Value -> Maybe Double
decodeNumber (Aeson.Number n) = Just (realToFrac n)
decodeNumber _ = Nothing
