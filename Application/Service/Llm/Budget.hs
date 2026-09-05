module Application.Service.Llm.Budget
( budgetExceeded
, rateLimitDelaySeconds
, dedupeWindowSeconds
, withinDedupeWindow
, dailyTokenBudget
, rateLimitPerMinute
, promptTokenBudget
, backoffSeconds
) where

import IHP.Prelude
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import qualified Data.Text as Text

-- Cost & rate controls (design_docs/milestone_4.md §6). The pure predicates
-- here are unit-tested directly; the job supplies DB state (today's counter
-- row, recent completion timestamps, prior analyses) and applies the
-- decisions.

budgetExceeded :: Int -> Int -> Int -> Bool
budgetExceeded cap tokensIn tokensOut = tokensIn + tokensOut >= cap

-- Sliding-window rate limiter over recent request timestamps (newest first).
-- Just delay = wait that many seconds before the next request is allowed.
rateLimitDelaySeconds :: Int -> UTCTime -> [UTCTime] -> Maybe Int
rateLimitDelaySeconds perMinute now recent =
    let windowStart = addUTCTime (-60) now
        inWindow = takeWhile (> windowStart) recent
    in if length inWindow < perMinute
        then Nothing
        else case reverse inWindow of
            [] -> Nothing
            (oldest:_) -> Just (max 1 (ceiling (diffUTCTime (addUTCTime 60 oldest) now)))

withinDedupeWindow :: Int -> UTCTime -> UTCTime -> Bool
withinDedupeWindow windowSeconds now priorCreatedAt =
    diffUTCTime now priorCreatedAt <= fromIntegral windowSeconds

dailyTokenBudget :: IO Int
dailyTokenBudget = envInt "LLM_DAILY_TOKEN_BUDGET" 1000000

rateLimitPerMinute :: IO Int
rateLimitPerMinute = envInt "LLM_RATE_PER_MINUTE" 20

promptTokenBudget :: IO Int
promptTokenBudget = envInt "LLM_PROMPT_TOKEN_BUDGET" 4096

dedupeWindowSeconds :: IO Int
dedupeWindowSeconds = envInt "LLM_DEDUPE_WINDOW_SECONDS" 3600

-- Retry backoff for retriable provider errors (milestone_4.md §4), same
-- env-override pattern as HALEMANS_WRITEBACK_BACKOFF_SECONDS.
backoffSeconds :: IO [Int]
backoffSeconds = do
    raw <- lookupEnv "HALEMANS_LLM_BACKOFF_SECONDS"
    pure case raw of
        Just raw -> case mapM (readMaybe . cs) (Text.splitOn "," (cs raw)) of
            Just parsed@(_:_) -> parsed
            _ -> defaultBackoff
        Nothing -> defaultBackoff
    where
        defaultBackoff = [60, 60]

envInt :: String -> Int -> IO Int
envInt name fallback = do
    raw <- lookupEnv name
    pure (fromMaybe fallback (raw >>= readMaybe))
