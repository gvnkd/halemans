module Application.Service.Api.RateLimit
    ( Bucket (..)
    , emptyBucket
    , allowRequest
    , checkLimit
    , LimitClass (..)
    , limitPerMinute
    ) where

import IHP.Prelude
import Data.IORef
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- Per-token token bucket, in-memory (design_docs/milestone_6.md §6): state
-- loss on restart is acceptable, limits are abuse guards not quotas.
data Bucket = Bucket
    { bucketTokens :: !Double
    , bucketUpdatedAt :: !UTCTime
    } deriving (Eq, Show)

emptyBucket :: UTCTime -> Bucket
emptyBucket = Bucket 0

buckets :: IORef (Map Text Bucket)
buckets = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE buckets #-}

-- Pure bucket step: refills at perMinute/60 tokens per second, capacity
-- perMinute. Returns (allowed, retryAfterSeconds, newBucket).
allowRequest :: Int -> UTCTime -> Bucket -> (Bool, Int, Bucket)
allowRequest perMinute now previous =
    let rate = fromIntegral perMinute / 60
        capacity = fromIntegral perMinute
        elapsed = max 0 (realToFrac (diffUTCTime now previous.bucketUpdatedAt))
        available = min capacity (previous.bucketTokens + elapsed * rate)
    in if available >= 1
        then (True, 0, Bucket (available - 1) now)
        else (False, ceiling ((1 - available) / rate), Bucket available now)

-- Returns Just retryAfterSeconds when the request must be rejected.
checkLimit :: Text -> Int -> IO (Maybe Int)
checkLimit key perMinute = do
    now <- getCurrentTime
    atomicModifyIORef' buckets \state ->
        let previous = Map.findWithDefault (Bucket (fromIntegral perMinute) now) key state
            (allowed, retryAfter, bucket) = allowRequest perMinute now previous
        in (Map.insert key bucket state, if allowed then Nothing else Just retryAfter)

data LimitClass = LimitApi | LimitMetrics deriving (Eq, Show)

-- Defaults per design §6; overridable for tests.
limitPerMinute :: LimitClass -> IO Int
limitPerMinute limitClass = do
    let (envName, defaultLimit) = case limitClass of
            LimitApi -> ("HALEMANS_API_RATE_LIMIT", 120)
            LimitMetrics -> ("HALEMANS_METRICS_RATE_LIMIT", 6)
    override <- lookupEnv envName
    pure (fromMaybe defaultLimit (override >>= readMaybe))
