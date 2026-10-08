module Application.Job.Mattermost (
    enqueueNotify,
    enqueueSyncIfPosted,
    enqueueBannerRefreshes,
    bannerIntervalSeconds,
) where

import Application.Service.Mattermost (deliverNotify, syncAlertPosts)
import Application.Service.Mattermost.Banner (bannerChannelEnabled, bannerEnabledChannels, refreshChannelBanner)
import qualified Application.Service.Mattermost.Render as Render
import Control.Monad (void, when)
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Generated.Types
import IHP.Fetch (fetch)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlQueryTyped, typedSql)

-- Outbound Mattermost delivery. Ingest and the alert pipeline only enqueue;
-- the worker owns the HTTP so a slow Mattermost server never blocks
-- ingestion. kind 'notify' = initial root post + details thread (the
-- service de-duplicates onto a plain sync when the pair already has a
-- post); kind 'sync' = re-render + patch existing root posts; kind
-- 'banner' = refresh the channel banner statistics (no alert attached —
-- channel carries the notification_channels row NAME).
--
-- Banner cadence: each banner job reschedules its successor at
-- +bannerIntervalSeconds while the channel still has the banner flag,
-- which gives the 60s periodic refresh; enqueueBannerRefreshes (the
-- AutoClose sweep + every alert event) reseeds the chain when it died
-- (job exhaustion) and piles no deeper than one pending row per channel.

instance Job MattermostJob where
    perform job = case job.kind of
        "notify" -> case (job.alertId, job.ruleId) of
            (Just alertId, Just ruleId) -> do
                alert <- fetch alertId
                rule <- fetch ruleId
                result <- deliverNotify alert rule
                case result of
                    Right () -> pure ()
                    Left err -> error (cs err) -- job retry/backoff
            _ -> pure ()
        "banner" -> case job.channel of
            Just channelName -> do
                result <- refreshChannelBanner channelName
                case result of
                    Left err -> error (cs err) -- job retry/backoff + visible last_error
                    Right () -> do
                        stillEnabled <- bannerChannelEnabled channelName
                        when stillEnabled do
                            now <- getCurrentTime
                            _ <-
                                newRecord @MattermostJob
                                    |> set #kind ("banner" :: Text)
                                    |> set #channel (Just channelName)
                                    |> set #runAt (addUTCTime bannerIntervalSeconds now)
                                    |> createRecord
                            pure ()
            Nothing -> pure ()
        _ -> case job.alertId of
            Just alertId -> do
                alert <- fetch alertId
                result <- syncAlertPosts alert
                case result of
                    Right () -> pure ()
                    Left err -> error (cs err) -- job retry/backoff + visible last_error
            Nothing -> pure ()

    maxAttempts = 5

-- | Banner self-reschedule interval: the effective "periodic refresh".
bannerIntervalSeconds :: NominalDiffTime
bannerIntervalSeconds = 60

-- | Fired by Notify.fireRule for rules whose channel is mattermost.
enqueueNotify :: (?modelContext :: ModelContext) => Alert -> NotificationRule -> IO ()
enqueueNotify alert rule =
    void do
        newRecord @MattermostJob
            |> set #alertId (Just (get #id alert))
            |> set #ruleId (Just (get #id rule))
            |> set #kind ("notify" :: Text)
            |> createRecord

-- | Called from the ingest fan-out (publishAlertUpdate) on every alert
-- transition: only state-changing kinds sync, and only when the alert
-- actually has Mattermost posts to patch.
enqueueSyncIfPosted :: (?modelContext :: ModelContext) => Alert -> Text -> IO ()
enqueueSyncIfPosted alert eventKind =
    when (Render.syncsKind eventKind) do
        posted <-
            query @MattermostPost
                |> filterWhere (#alertId, get #id alert)
                |> fetch
        unless (null posted) do
            _ <-
                newRecord @MattermostJob
                    |> set #alertId (Just (get #id alert))
                    |> set #kind ("sync" :: Text)
                    |> set #eventKind (Just eventKind)
                    |> createRecord
            pure ()

-- | Banner refresh trigger for every banner-enabled channel: the AutoClose
-- sweep (chain backstop) and the alert-event fan-out (near-real-time). One
-- pending banner row per channel at most — the worker debounces actual PUTs
-- within bannerDebounceSeconds on top.
enqueueBannerRefreshes :: (?modelContext :: ModelContext) => IO ()
enqueueBannerRefreshes = do
    channels <- bannerEnabledChannels
    forM_ channels \channel -> do
        pending <- pendingBanners (channel.name)
        when (pending == 0) do
            _ <-
                newRecord @MattermostJob
                    |> set #kind ("banner" :: Text)
                    |> set #channel (Just channel.name)
                    |> createRecord
            pure ()
  where
    pendingBanners channelName = do
        rows <-
            sqlQueryTyped
                [typedSql|
                    SELECT count(*)::int FROM mattermost_jobs
                    WHERE kind = 'banner' AND channel = ${channelName}
                      AND status::text IN ('job_status_not_started', 'job_status_running', 'job_status_retry')
                |]
        pure case rows of
            (n : _) -> n
            [] -> 0
