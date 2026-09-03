module Application.Job.PollZabbix where

import IHP.Prelude
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Generated.Types
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingestEvents)
import qualified Application.Connector.Zabbix as Zabbix
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import System.Environment (lookupEnv)

-- Self-rescheduling zabbix poller (milestone 0). Seeded by EnqueuePollers
-- (via `seed`); drops duplicate pending siblings before rescheduling so
-- re-running seed never spawns a second loop.
instance Job PollZabbixJob where
    perform _job = do
        sources <- query @Source
            |> filterWhere (#type_, "zabbix" :: Text)
            |> filterWhere (#enabled, True)
            |> fetch
        forM_ sources pollSource

        now <- getCurrentTime
        next <- newRecord @PollZabbixJob
            |> set #runAt (addUTCTime 5 now)
            |> createRecord
        let nextId = get #id next
        _ <- sqlExecTyped [typedSql|
            DELETE FROM poll_zabbix_jobs
            WHERE status = 'job_status_not_started' AND id <> ${nextId}
        |]
        pure ()

    -- Pick up future-run_at reschedules quickly (default is 60s).
    queuePollInterval = 3 * 1000000
    maxAttempts = 3

pollSource :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Source -> IO ()
pollSource source = do
    let tokenEnv :: Maybe Text
        tokenEnv = parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: "tokenEnv")) source.config
    token <- case tokenEnv of
        Just envVar -> fmap cs <$> lookupEnv (cs envVar)
        Nothing -> pure Nothing
    case token of
        Nothing -> pure () -- token not issued yet (seed not run); try next cycle
        Just token -> do
            let cursor = fromMaybe 0 ((\t -> floor (utcTimeToPOSIXSeconds t) :: Integer) <$> source.lastSyncCursor)
            result <- Zabbix.eventGet source.baseUrl token cursor
            case result of
                Left err -> pure () -- soft-fail; surfaced via logs in phase 5 (source health)
                Right events -> do
                    ingestEvents source (map (Zabbix.toNormalizedEvent source.baseUrl) events)
                    case maximumMaybe (map (.clock) events) of
                        Just maxClock -> do
                            _ <- source
                                |> set #lastSyncCursor (Just (posixSecondsToUTCTime (fromIntegral maxClock)))
                                |> updateRecord
                            pure ()
                        Nothing -> pure ()

maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just (maximum xs)
