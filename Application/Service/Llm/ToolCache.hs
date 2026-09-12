module Application.Service.Llm.ToolCache (
    cachedToolCall,
    currentToolCacheTtl,
    defaultTtlSeconds,
    isFailureText,
    freshEnough,
) where

import Control.Monad (void)
import qualified Data.Text as Text
import Generated.Types (LlmToolCache, LlmToolCache' (..), LlmToolCacheConfig, LlmToolCacheConfig' (..))
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.HaskellSupport (set)
import IHP.ModelSupport (ModelContext, createRecord, newRecord, updateRecord)
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, query)
import IHP.TypedSql (sqlExecTyped, typedSql)

-- Short-lived cache for the read-only LLM agent tools (milestone 10 §6).
-- executeToolCall routes every tool through cachedToolCall keyed by
-- (tool, raw arguments). The singleton llm_tool_cache_configs row carries
-- the TTL and on/off switch (Admin → LLM); no row = defaults (enabled,
-- defaultTtlSeconds). Failure texts are never cached so a recovering
-- integration is seen immediately; rows older than the TTL are evicted on
-- write.

defaultTtlSeconds :: Int
defaultTtlSeconds = 300

-- Effective TTL in seconds; Nothing disables the cache entirely.
currentToolCacheTtl :: (?modelContext :: ModelContext) => IO (Maybe Int)
currentToolCacheTtl = do
    rows <- query @LlmToolCacheConfig |> fetch
    pure case head rows of
        Just row
            | not row.enabled -> Nothing
            | row.ttlSeconds <= 0 -> Nothing
            | otherwise -> Just row.ttlSeconds
        Nothing -> Just defaultTtlSeconds

cachedToolCall :: (?modelContext :: ModelContext) => Text -> Text -> IO Text -> IO Text
cachedToolCall tool arguments action = do
    maybeTtl <- currentToolCacheTtl
    case maybeTtl of
        Nothing -> action
        Just ttlSeconds -> do
            now <- getCurrentTime
            cached <-
                query @LlmToolCache
                    |> filterWhere (#tool, tool)
                    |> filterWhere (#arguments, arguments)
                    |> fetchOneOrNothing
            case cached of
                Just row | freshEnough ttlSeconds now row.fetchedAt -> pure row.response
                _ -> do
                    response <- action
                    unless (isFailureText response) do
                        _ <- case cached of
                            Just row -> updateRecord (row |> set #response response |> set #fetchedAt now)
                            Nothing ->
                                createRecord
                                    ( newRecord @LlmToolCache
                                        |> set #tool tool
                                        |> set #arguments arguments
                                        |> set #response response
                                        |> set #fetchedAt now
                                    )
                        evictOlderThan now ttlSeconds
                    pure response

freshEnough :: Int -> UTCTime -> UTCTime -> Bool
freshEnough ttlSeconds now fetchedAt = diffUTCTime now fetchedAt < fromIntegral ttlSeconds

evictOlderThan :: (?modelContext :: ModelContext) => UTCTime -> Int -> IO ()
evictOlderThan now ttlSeconds = do
    let cutoff = addUTCTime (negate (fromIntegral ttlSeconds)) now
    void
        ( sqlExecTyped
            [typedSql|
        DELETE FROM llm_tool_cache WHERE fetched_at < ${cutoff}
    |]
        )

-- Tool failures come back in-band as text (soft-fail contract); those must
-- not be cached. Legitimate empty results ("no jira tickets found") ARE
-- cached — they are valid negatives, not failures.
isFailureText :: Text -> Bool
isFailureText response = any (`Text.isPrefixOf` response) failurePrefixes
  where
    failurePrefixes =
        [ "cmdb lookup failed"
        , "cmdb not configured"
        , "cmdb unavailable"
        , "jira search failed"
        , "jira not configured"
        , "jira unavailable"
        , "jira issue not found"
        , "assets lookup failed"
        , "assets not configured"
        , "invalid arguments"
        , "unknown tool"
        ]
