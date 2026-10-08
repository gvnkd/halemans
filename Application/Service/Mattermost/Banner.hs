module Application.Service.Mattermost.Banner (
    BannerCounts (..),
    Trend (..),
    BannerOutcome (..),
    bannerSeverities,
    renderBannerText,
    bannerBarColor,
    trendOf,
    countsJson,
    parseCounts,
    bannerDebounceSeconds,
    bannerEnabledChannels,
    bannerChannelEnabled,
    bannerCandidateSeverities,
    refreshChannelBanner,
) where

import Application.Pipeline.Grouping (severityRank)
import Application.Service.Mattermost (mattermostConfigForRule, mattermostTargetForRule)
import qualified Application.Service.Mattermost.Api as Api
import Application.Service.Mattermost.Render (bannerEnabledFromJson)
import Application.Service.RuleMatch (ruleInScope, ruleMatches)
import Control.Monad (filterM)
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.List (nubBy)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlQueryTyped, typedSql)
import System.IO (hFlush, stdout)

-- Mattermost channel banner statistics. For every notification channel row
-- with config "banner": true, the worker refreshes the MM channel banner
-- with the counts of ACTIVE (firing/ack) alerts matched by the channel's
-- rules (severity threshold + match expression + team host-group scope — the
-- exact dispatch predicates, via Application.Service.RuleMatch), firing and
-- acked shown as total (acked). Per-severity trend arrows compare against
-- the alert_stats_snapshots row nearest to now - bannerTrendMinutes. The
-- counters carry the severity colors (emoji); the banner BAR stays a neutral
-- gray so it never merges with them (green only when nothing is active).
-- This module sits BELOW Service.Mattermost's importers (the job worker
-- imports it), so it must not import Notify/Helper.Ingest — hence RuleMatch.

data BannerCounts = BannerCounts
    { bcTotal :: Int
    , bcAcked :: Int
    }
    deriving (Eq, Show)

data Trend = TrendUp | TrendDown | TrendFlat
    deriving (Eq, Show)

-- | What a refresh achieved: the banner went up; the server DENIED it
-- (403/404 — missing permission or no banner feature) and nothing was
-- updated; or nothing was attempted (soft skip: debounced, no rules/targets,
-- missing config). The worker schedules by it: denied re-attempts quietly
-- after a long backoff, skipped/refreshed keep the normal 60s chain.
data BannerOutcome = BannerRefreshed | BannerDenied | BannerSkipped
    deriving (Eq, Show)

bannerSeverities :: [Text]
bannerSeverities = ["critical", "high", "warning", "info"]

-- | Duplicate banner PUTs within this window are skipped (event-driven
-- enqueues would otherwise hammer the MM API on every alert transition).
bannerDebounceSeconds :: Int
bannerDebounceSeconds = 30

bannerNeutralColor :: Text
bannerNeutralColor = "#98A2AD"

bannerClearColor :: Text
bannerClearColor = "#3FB950"

severityEmoji :: Text -> Text
severityEmoji = \case
    "critical" -> "🔴"
    "high" -> "🟠"
    "warning" -> "🟡"
    _ -> "🔵"

severityLabel :: Text -> Text
severityLabel = \case
    "critical" -> "crit"
    "high" -> "high"
    "warning" -> "warn"
    _ -> "info"

trendArrow :: Trend -> Text
trendArrow = \case
    TrendUp -> "🔺"
    TrendDown -> "🔻"
    TrendFlat -> "➖"

trendOf :: Int -> Int -> Trend
trendOf before after = case compare after before of
    GT -> TrendUp
    LT -> TrendDown
    EQ -> TrendFlat

-- | One banner line: "🔴 crit 5 (3)🔺 · 🟠 high 2 (1)➖ · ...". All four
-- severities always render (zeros included — the full picture at a glance);
-- all-clear collapses to a single checkmark line.
renderBannerText :: [(Text, BannerCounts, Trend)] -> Text
renderBannerText items
    | all (\(_, counts, _) -> bcTotal counts == 0) items = "✅ no active alerts"
    | otherwise =
        Text.intercalate
            " · "
            [ severityEmoji sev
                <> " "
                <> severityLabel sev
                <> " "
                <> tshow (bcTotal counts)
                <> " ("
                <> tshow (bcAcked counts)
                <> ")"
                <> trendArrow trend
            | (sev, counts, trend) <- items
            ]

-- | The banner BAR color: neutral gray while anything is active (the
-- counters carry the severity colors), green when all-clear.
bannerBarColor :: [(Text, BannerCounts)] -> Text
bannerBarColor counts
    | all (\(_, c) -> bcTotal c == 0) counts = bannerClearColor
    | otherwise = bannerNeutralColor

countsJson :: [(Text, BannerCounts)] -> Aeson.Value
countsJson counts =
    Aeson.object
        [ Key.fromText sev
            Aeson..= Aeson.object
                [ "total" Aeson..= total
                , "acked" Aeson..= acked
                ]
        | (sev, BannerCounts total acked) <- counts
        ]

-- | The inverse of countsJson; unknown shapes yield no history (all trends
-- flat).
parseCounts :: Aeson.Value -> [(Text, BannerCounts)]
parseCounts value = case value of
    Aeson.Object object_ ->
        [ (sev, counts)
        | sev <- bannerSeverities
        , Just counts <- [lookupSeverity object_ sev]
        ]
    _ -> []
  where
    lookupSeverity object_ sev = do
        raw <- KeyMap.lookup (Key.fromText sev) object_
        parseMaybe
            ( Aeson.withObject "severity counts" \o ->
                BannerCounts <$> o .: "total" <*> o .: "acked"
            )
            raw

-- | All enabled mattermost channel rows with the banner flag on (the
-- enqueue/sweep domain).
bannerEnabledChannels :: (?modelContext :: ModelContext) => IO [NotificationChannel]
bannerEnabledChannels = do
    channels <-
        query @NotificationChannel
            |> filterWhere (#type_, "mattermost" :: Text)
            |> filterWhere (#enabled, True)
            |> orderByAsc #name
            |> fetch
    pure (filter (bannerEnabledFromJson . (.config)) channels)

-- | Cheap re-check used by the worker when deciding whether to reschedule
-- the next periodic banner job.
bannerChannelEnabled :: (?modelContext :: ModelContext) => Text -> IO Bool
bannerChannelEnabled channelName = do
    channelOrNothing <-
        query @NotificationChannel
            |> filterWhere (#name, channelName)
            |> fetchOneOrNothing
    pure case channelOrNothing of
        Just channel ->
            channel.type_
                == "mattermost"
                && channel.enabled
                && bannerEnabledFromJson channel.config
        Nothing -> False

-- | Recompute and PUT the banner for one notification channel. Soft skips
-- (missing/disabled channel, flag off, unset token, no rules, debounced)
-- and a server DENIAL (403/404) return Right; only retryable failures
-- (network, 5xx) return Left so the job retries with a visible last_error.
-- On success a snapshot row is written for the trend comparison.
refreshChannelBanner :: (?modelContext :: ModelContext) => Text -> IO (Either Text BannerOutcome)
refreshChannelBanner channelName = do
    channelOrNothing <-
        query @NotificationChannel
            |> filterWhere (#name, channelName)
            |> fetchOneOrNothing
    case channelOrNothing of
        Just channel
            | channel.type_ == "mattermost"
            , channel.enabled
            , bannerEnabledFromJson channel.config ->
                runRefresh channel
        _ -> pure (Right BannerSkipped)

runRefresh :: (?modelContext :: ModelContext) => NotificationChannel -> IO (Either Text BannerOutcome)
runRefresh channel = do
    debounced <- recentlySucceeded (channel.name)
    if debounced
        then pure (Right BannerSkipped)
        else do
            configOrNothing <- Api.configForChannel channel
            case configOrNothing of
                Nothing -> pure (Right BannerSkipped)
                Just config -> refreshWithConfig channel config

-- | Skip when a banner job for this channel SUCCEEDED within the debounce
-- window (the job row's updated_at is stamped at completion) — event-driven
-- enqueues must not double-refresh.
recentlySucceeded :: (?modelContext :: ModelContext) => Text -> IO Bool
recentlySucceeded channelName = do
    let secs = fromIntegral bannerDebounceSeconds :: Double
    rows <-
        sqlQueryTyped
            [typedSql|
                SELECT count(*)::int FROM mattermost_jobs
                WHERE kind = 'banner' AND channel = ${channelName}
                  AND status::text = 'job_status_succeeded'
                  AND updated_at > now() - make_interval(secs => ${secs})
            |]
    pure case rows of
        (n : _) -> n > 0
        [] -> False

refreshWithConfig :: (?modelContext :: ModelContext) => NotificationChannel -> Api.MattermostConfig -> IO (Either Text BannerOutcome)
refreshWithConfig channel config = do
    rules <-
        query @NotificationRule
            |> filterWhere (#channel, channel.name)
            |> filterWhere (#enabled, True)
            |> fetch
    if null rules
        then pure (Right BannerSkipped)
        else do
            -- SQL prefilter before the Haskell matching: the old query
            -- fetched EVERY firing/ack alert in the DB (labels/annotations
            -- JSONB included) on each 60s tick per banner channel. A rule's
            -- threshold accepts exactly the canonical severities at or
            -- above it (severityRank), suppressed alerts never count for
            -- any rule — both pushed into the query.
            active <-
                query @Alert
                    |> filterWhereIn (#status, ["firing", "ack"] :: [Text])
                    |> filterWhere (#suppressed, False)
                    |> filterWhereIn (#severity, bannerCandidateSeverities rules)
                    |> fetch
            matched <- bannerMatchedAlerts active rules
            let counts = bannerCountsFor matched
            items <- withTrends channel config counts
            let text = renderBannerText items
                color = bannerBarColor counts
            targets <- bannerTargets rules
            if null targets
                then pure (Right BannerSkipped)
                else pushBanners channel counts text color targets

-- | The canonical severities ANY of the channel's rules can dispatch on:
-- a rule's threshold accepts every severity with severityRank >= its own,
-- so the union over the rules is the severities at or above the LOWEST
-- threshold. A non-canonical threshold (rank 0) accepts everything.
bannerCandidateSeverities :: [NotificationRule] -> [Text]
bannerCandidateSeverities rules =
    [ sev
    | sev <- bannerSeverities
    , severityRank sev >= minimum (map (severityRank . (.severityThreshold)) rules)
    ]

-- | Active alerts matched by ANY of the channel's enabled rules (the exact
-- dispatch predicates: severity threshold + match expression + team
-- host-group scope) and NOT suppressed (blackout-covered or source-muted
-- alerts stay out of the statistics — the banner shows what is actually
-- notifying), deduplicated by alert id.
bannerMatchedAlerts :: (?modelContext :: ModelContext) => [Alert] -> [NotificationRule] -> IO [Alert]
bannerMatchedAlerts active rules = do
    idSets <- forM rules \rule -> do
        let candidates = filter (\alert -> ruleMatches alert rule) active
        scoped <- filterM (\alert -> ruleInScope alert rule) candidates
        pure (Set.fromList (map (get #id) scoped))
    let matchedIds = Set.unions idSets
    pure (filter (\alert -> not alert.suppressed && Set.member (get #id alert) matchedIds) active)

-- | Per canonical severity: total active + how many of them are acked.
-- Non-canonical severities bucket into info (there is no fifth banner slot).
bannerCountsFor :: [Alert] -> [(Text, BannerCounts)]
bannerCountsFor alerts =
    [ (sev, BannerCounts (length sevAlerts) (length (filter (\a -> a.status == "ack") sevAlerts)))
    | sev <- bannerSeverities
    , let sevAlerts = [alert | alert <- alerts, severityBucket alert == sev]
    ]
  where
    severityBucket alert = case alert.severity of
        "critical" -> "critical"
        "high" -> "high"
        "warning" -> "warning"
        _ -> "info"

-- | Attach the per-severity trend by comparing against the snapshot nearest
-- to now - bannerTrendMinutes; no history yet -> all flat.
withTrends :: (?modelContext :: ModelContext) => NotificationChannel -> Api.MattermostConfig -> [(Text, BannerCounts)] -> IO [(Text, BannerCounts, Trend)]
withTrends channel config counts = do
    now <- getCurrentTime
    let windowSeconds = fromIntegral (Api.mmBannerTrendMinutes config) * 60 :: NominalDiffTime
        target = addUTCTime (negate windowSeconds) now
    previousRows <-
        sqlQueryTyped
            [typedSql|
                SELECT counts FROM alert_stats_snapshots
                WHERE channel = ${channelName} AND created_at <= ${target}
                ORDER BY created_at DESC LIMIT 1
            |] ::
            IO [Aeson.Value]
    let previous = maybe [] parseCounts (listToMaybe previousRows)
        countsFor sev = fromMaybe (BannerCounts 0 0) (lookup sev counts)
        trendFor sev = case lookup sev previous of
            Just before -> trendOf (bcTotal before) (bcTotal (countsFor sev))
            Nothing -> TrendFlat
    pure [(sev, countsFor sev, trendFor sev) | sev <- bannerSeverities]
  where
    channelName = channel.name

-- | The MM channels to banner: every rule's resolved target (config +
-- team/channel), deduplicated — several rules on one channel row may share
-- the same MM channel.
bannerTargets :: (?modelContext :: ModelContext) => [NotificationRule] -> IO [(Api.MattermostConfig, (Text, Text))]
bannerTargets rules = do
    perRule <- forM rules \rule -> do
        configOrNothing <- mattermostConfigForRule rule
        targetOrError <- mattermostTargetForRule rule
        pure case (configOrNothing, targetOrError) of
            (Just cfg, Right target) -> Just (cfg, target)
            _ -> Nothing
    pure (nubBy (\a b -> snd a == snd b) (catMaybes perRule))

-- | PUT the banner to every resolved target. Any success → snapshot +
-- BannerRefreshed. No success but at least one target answered 403/404 →
-- BannerDenied (logged once per run: permission or missing feature, an
-- admin must act) with NO retry storm. Only genuine failures (network/5xx)
-- come back Left so the job retries.
pushBanners :: (?modelContext :: ModelContext) => NotificationChannel -> [(Text, BannerCounts)] -> Text -> Text -> [(Api.MattermostConfig, (Text, Text))] -> IO (Either Text BannerOutcome)
pushBanners channel counts text color targets = do
    results <- forM targets \(targetConfig, (teamName, mmChannel)) -> do
        resolved <- Api.resolveChannel targetConfig teamName mmChannel
        case resolved of
            Left err -> pure (Left err)
            Right channelId -> do
                result <- Api.putBanner targetConfig channelId text color
                pure case result of
                    Api.BannerPutOk -> Right ()
                    Api.BannerPutDenied -> Left (deniedLabel teamName mmChannel)
                    Api.BannerPutError err -> Left err
    let oks = [() | Right () <- results]
        failures = [err | Left err <- results]
        denied = filter isDenied failures
        retryable = filter (not . isDenied) failures
    if not (null oks)
        then snapshotAndReturn BannerRefreshed
        else case retryable of
            (retryErr : _) -> pure (Left retryErr)
            [] -> do
                -- Every target denied the PUT (403/404 — the bot lacks
                -- channel-management permission or the server has no
                -- channel-banner feature): soft-skip with one log line, no
                -- retry storm, no snapshot.
                putStrLn
                    ( "mattermost banner: channel \""
                        <> channel.name
                        <> "\" targets ("
                        <> Text.intercalate "; " denied
                        <> "): server denied the banner PUT (403/404) — grant the bot channel-management permission, or disable \"banner\" in the channel config (requires MM 10.9+ with channel banners)"
                    )
                hFlush stdout
                pure (Right BannerDenied)
  where
    deniedLabel teamName mmChannel = "mattermost banner: denied " <> teamName <> "/" <> mmChannel
    isDenied err = "mattermost banner: denied " `Text.isPrefixOf` err
    snapshotAndReturn outcome = do
        _ <-
            newRecord @AlertStatsSnapshot
                |> set #channel channel.name
                |> set #counts (countsJson counts)
                |> createRecord
        pure (Right outcome)
