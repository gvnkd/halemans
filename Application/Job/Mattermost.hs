module Application.Job.Mattermost (
    enqueueNotify,
    enqueueSyncIfPosted,
    enqueueRefireCards,
    enqueueMissingCards,
    enqueueBannerRefreshes,
    bannerIntervalSeconds,
) where

import Application.Service.Mattermost (deliverNotify, syncAlertPosts)
import Application.Service.Mattermost.Banner (BannerOutcome (..), bannerChannelEnabled, bannerEnabledChannels, refreshChannelBanner)
import qualified Application.Service.Mattermost.Render as Render
import Application.Service.RuleMatch (ruleInScope, ruleMatches)
import Control.Monad (filterM, void, when)
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
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
                    Right outcome -> do
                        stillEnabled <- bannerChannelEnabled channelName
                        when stillEnabled do
                            now <- getCurrentTime
                            -- A denied PUT (403/404 — permission/feature
                            -- missing) re-attempts quietly after a long
                            -- backoff; the chain stays at 60s otherwise.
                            let delay = case outcome of
                                    BannerDenied -> bannerDeniedRetrySeconds
                                    _ -> bannerIntervalSeconds
                            _ <-
                                newRecord @MattermostJob
                                    |> set #kind ("banner" :: Text)
                                    |> set #channel (Just channelName)
                                    |> set #runAt (addUTCTime delay now)
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

-- | A denied PUT (403/404) never fixes itself by retrying fast — re-attempt
-- quietly after this backoff (an admin may grant the permission or the
-- server may get the feature).
bannerDeniedRetrySeconds :: NominalDiffTime
bannerDeniedRetrySeconds = 900

-- | Fired by Notify.fireRule for rules whose channel is mattermost.
enqueueNotify :: (?modelContext :: ModelContext) => Alert -> NotificationRule -> IO ()
enqueueNotify alert rule =
    void do
        newRecord @MattermostJob
            |> set #alertId (Just (get #id alert))
            |> set #ruleId (Just (get #id rule))
            |> set #kind ("notify" :: Text)
            |> createRecord

-- | Card restoration on refire. Refires deliberately do not dispatch
-- notifications (a refire is not a new alert), and enqueueSyncIfPosted
-- no-ops when the alert has no posts — so an alert whose mattermost_posts
-- rows were dropped while it was terminal (deleteOnClose deleting the
-- post on resolve, or an admin purge) would stay cardless forever after
-- resolving and refiring. Re-enqueue the initial delivery for the alert's
-- matching mattermost rules; deliverNotify dedupes onto a plain sync when
-- a post still exists, so repeated refires are harmless.
-- The dispatch predicates are the leaf RuleMatch ones (Notify itself
-- cannot be imported here — it imports this module).
enqueueRefireCards :: (?modelContext :: ModelContext) => Alert -> IO ()
enqueueRefireCards alert = do
    rules <-
        query @NotificationRule
            |> filterWhere (#enabled, True)
            |> fetch
    forM_ rules \rule -> do
        channelOrNothing <-
            query @NotificationChannel
                |> filterWhere (#name, rule.channel)
                |> fetchOneOrNothing
        case channelOrNothing of
            Just channel
                | channel.enabled
                , channel.type_ == "mattermost"
                , ruleMatches alert rule ->
                    ruleInScope alert rule >>= \inScope -> when inScope (enqueueNotify alert rule)
            _ -> pure ()

-- | One-shot backfill (per-channel admin action): enqueue the initial card
-- delivery for every active, NOT suppressed alert matched by one of the
-- channel's enabled rules whose (alert, rule) pair has no posts row —
-- deliverNotify dedupes on exactly that pair, so this is the precise
-- "banner counts it, the channel has no card" set. deliverNotify further
-- dedupes onto sync when a row exists by run time, so a repeat run only
-- costs no-op jobs. Returns the number of enqueued notify jobs.
enqueueMissingCards :: (?modelContext :: ModelContext) => Text -> IO Int
enqueueMissingCards channelRowName = do
    rules <-
        query @NotificationRule
            |> filterWhere (#channel, channelRowName)
            |> filterWhere (#enabled, True)
            |> fetch
    active <-
        query @Alert
            |> filterWhereIn (#status, ["firing", "ack", "stalled"] :: [Text])
            |> filterWhere (#suppressed, False)
            |> fetch
    counts <- forM rules \rule -> do
        let matched = filter (\alert -> ruleMatches alert rule) active
        inScope <- filterM (\alert -> ruleInScope alert rule) matched
        withoutPosts <- filterM (pairMissing rule) inScope
        forM_ withoutPosts \alert ->
            void do
                newRecord @MattermostJob
                    |> set #alertId (Just (get #id alert))
                    |> set #ruleId (Just (get #id rule))
                    |> set #kind ("notify" :: Text)
                    |> createRecord
        pure (length withoutPosts)
    pure (sum counts)
  where
    pairMissing rule alert = do
        posted <-
            query @MattermostPost
                |> filterWhere (#alertId, get #id alert)
                |> filterWhere (#notificationRuleId, Just (get #id rule))
                |> fetch
        pure (null posted)

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
-- sweep (chain backstop) and the alert-event fan-out (near-real-time). No
-- enqueue when the chain is alive: a pending row OR any banner job touched
-- within the last 5 minutes (a denied channel retries on its own 15-minute
-- backoff — the backstop must not pile attempts on top).
enqueueBannerRefreshes :: (?modelContext :: ModelContext) => IO ()
enqueueBannerRefreshes = do
    channels <- bannerEnabledChannels
    forM_ channels \channel -> do
        alive <- chainAlive (channel.name)
        unless alive do
            _ <-
                newRecord @MattermostJob
                    |> set #kind ("banner" :: Text)
                    |> set #channel (Just channel.name)
                    |> createRecord
            pure ()
  where
    chainAlive channelName = do
        rows <-
            sqlQueryTyped
                [typedSql|
                    SELECT count(*)::int FROM mattermost_jobs
                    WHERE kind = 'banner' AND channel = ${channelName}
                      AND ( status::text IN ('job_status_not_started', 'job_status_running', 'job_status_retry')
                            OR updated_at > now() - make_interval(secs => ${windowSeconds}) )
                |]
        pure case rows of
            (n : _) -> n > 0
            [] -> False
    windowSeconds = 300 :: Double
