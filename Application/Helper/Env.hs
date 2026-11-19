module Application.Helper.Env where

import IHP.Prelude
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- Worker concurrency knobs, env-overridable so small deployments can throttle
-- job bursts (e.g. a Zabbix flood enqueueing hundreds of enrichments) without
-- a rebuild. Defaults: 4, below the IHP default of 16. A value < 1 or
-- unparseable falls back to the default.
enrichJobConcurrency :: Int
enrichJobConcurrency = unsafePerformIO (envPositiveInt "HALEMANS_ENRICH_JOB_CONCURRENCY" 4)
{-# NOINLINE enrichJobConcurrency #-}

llmJobConcurrency :: Int
llmJobConcurrency = unsafePerformIO (envPositiveInt "HALEMANS_LLM_JOB_CONCURRENCY" 4)
{-# NOINLINE llmJobConcurrency #-}

envPositiveInt :: Text -> Int -> IO Int
envPositiveInt name fallback = do
    raw <- lookupEnv (cs name)
    pure
        ( case raw >>= readMaybe . cs of
            Just n | n >= 1 -> n
            _ -> fallback
        )
