module Application.Service.Mattermost (
    renderContextFor,
    deliverNotify,
    syncAlertPosts,
    statusSnapshot,
    actionSecret,
    mattermostConfigForRule,
    mattermostUsernameFromSettings,
    mattermostTarget,
    mattermostTargetForRule,
    activeMattermostTemplate,
    purgeResolvedMattermostPosts,
    purgeMattermostPostsForChannel,
    PurgeMattermostMode (..),
    PurgeMattermostSummary (..),
) where

import Application.Service.ActionTokens (ensureActionToken)
import Application.Service.Http (statusCodeOfError)
import Application.Service.Mattermost.Api (MattermostConfig)
import qualified Application.Service.Mattermost.Api as Api
import Application.Service.Mattermost.Render (MattermostRenderContext (..), mattermostAttachmentTemplateName, mattermostColorTemplateName, mattermostDetailsTemplateName, mattermostFieldsTemplateName, mattermostRootTemplateName, mattermostStatusTemplateName, renderDetailsMessage, renderRootMessage, renderRootProps)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Semigroup (Semigroup)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (traverse)
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import System.Environment (lookupEnv)

-- Orchestration for the Mattermost notification channel: builds the render
-- context (actor names, public URLs), delivers the initial root post +
-- details thread, and patches root posts on status changes. The server
-- address + bot token resolve from the rule's notification_channels row
-- (base_url + config.tokenEnv, the source-credential pattern); an
-- unconfigured channel is a silent skip, like push without VAPID keys. All
-- HTTP errors return Left so the caller (the job) decides between retry and
-- soft-skip. The Ack action lives in Application.Service.Mattermost.Actions —
-- it imports the alert pipeline, and this module is imported from the ingest
-- fan-out, so the dependency must stay one-directional.

-- | The mattermost config behind a rule (via its channel row); Nothing when
-- the channel is missing, disabled, or lacks baseUrl/token.
mattermostConfigForRule :: (?modelContext :: ModelContext) => NotificationRule -> IO (Maybe MattermostConfig)
mattermostConfigForRule rule = do
    channelOrNothing <- query @NotificationChannel |> filterWhere (#name, rule.channel) |> fetchOneOrNothing
    case channelOrNothing of
        Just channel | channel.enabled -> Api.configForChannel channel
        _ -> pure Nothing

-- | Mattermost username stored in a user profile (users.settings
-- "mattermostUsername"). This is the PRIMARY identity the MM Ack action
-- matches the clicker by (displayName is only a fallback); "" = unset.
mattermostUsernameFromSettings :: Aeson.Value -> Text
mattermostUsernameFromSettings settings =
    fromMaybe "" (parseMaybe (Aeson.withObject "settings" (\o -> o Aeson..:? "mattermostUsername" Aeson..!= "")) settings)

-- | Shared-secret path segment of the action endpoint. Falls back to the bot
-- token env var so a working setup needs only the channel row + one secret.
actionSecret :: IO Text
actionSecret = do
    explicit <- lookupEnv "MATTERMOST_ACTION_SECRET"
    token <- lookupEnv "MATTERMOST_TOKEN"
    pure (cs (fromMaybe "" (explicit <|> token)))

-- | Public URL of the Halemans instance for deep links and the Ack action
-- endpoint. The worker renders outside a request, so this reads env rather
-- than request-derived URLs: HALEMANS_BASE_URL wins, then the standard IHP
-- vars (IHP_BASEURL / APPROOT) so an instance already configured for the web
-- side needs no extra variable.
publicBaseUrl :: IO Text
publicBaseUrl = do
    explicit <- lookupEnv "HALEMANS_BASE_URL"
    ihp <- lookupEnv "IHP_BASEURL"
    approot <- lookupEnv "APPROOT"
    pure (cs (fromMaybe "" (explicit <|> ihp <|> approot)))

-- | Render context for one (rule, alert): resolves the ack/close actor
-- names, the alert deep-link, and — when a public base URL is configured —
-- the Ack button integration URL with its shared secret.
renderContextFor :: (?modelContext :: ModelContext) => Maybe NotificationRule -> Alert -> IO MattermostRenderContext
renderContextFor rule alert = do
    base <- publicBaseUrl
    secret <- actionSecret
    ackedBy <- traverse (fmap (.displayName) . fetch) alert.acknowledgedBy
    closedBy <- traverse (fmap (.displayName) . fetch) alert.closedBy
    let alertUrl = base <> "/alerts/" <> tshow (get #id alert)
        actionUrl =
            if Text.null base
                then Nothing
                else Just (base <> "/hooks/mattermost/actions/" <> secret)
    -- One-time markdown Ack link for firing alerts. The token is ROTATED on
    -- every render (re-rendered root posts invalidate earlier unused links);
    -- nothing is minted for terminal states or without a public base URL.
    ackUrl <- case (statusText' alert, Text.null base) of
        ("firing", False) -> do
            token <- ensureActionToken (get #id alert) "ack"
            pure (Just (alertUrl <> "/ack-link?token=" <> token))
        _ -> pure Nothing
    pure
        MattermostRenderContext
            { mrcRuleName = maybe "-" (.name) rule
            , mrcAckedBy = ackedBy
            , mrcAckedAt = alert.acknowledgedAt
            , mrcClosedBy = closedBy
            , mrcActionUrl = actionUrl
            , mrcAckUrl = ackUrl
            , mrcAlertUrl = alertUrl
            , mrcAlertId = tshow (get #id alert)
            }
  where
    statusText' a = a.status

-- | Channel config "ackAction": False suppresses the interactive Ack BUTTON
-- (ackAction in Render renders nothing without an action URL). The one-time
-- [Ack] markdown link is template-controlled and stays.
respectAckFlag :: MattermostRenderContext -> MattermostConfig -> MattermostRenderContext
respectAckFlag context config =
    if Api.mmAckAction config then context else context{mrcActionUrl = Nothing}

-- | Active template body for a mattermost_* llm_prompt_templates row; Nothing
-- when no row is active (the renderer falls back to its built-in default).
activeMattermostTemplate :: (?modelContext :: ModelContext) => Text -> IO (Maybe Text)
activeMattermostTemplate name = do
    template <-
        query @LlmPromptTemplate
            |> filterWhere (#name, name)
            |> filterWhere (#active, True)
            |> fetchOneOrNothing
    pure ((.body) <$> template)

-- | Where a rule posts, pure part: the rule's channelConfig wins; otherwise
-- the rule's team defaults (Admin → Teams stores {"mattermost":{"team",
-- "channel"}} in teams.defaults); the MM team name falls back to "halemans".
-- Left carries the job-visible error.
mattermostTarget :: Aeson.Value -> NotificationRule -> Either Text (Text, Text)
mattermostTarget teamDefaults rule =
    let fromTeam key = nestedConfigText ["mattermost", key] teamDefaults
        teamName = configText "team" rule.channelConfig `orElse` fromTeam "team" `orElse` "halemans"
        channelName = configText "channel" rule.channelConfig `orElse` fromTeam "channel"
     in if Text.null channelName
            then Left ("mattermost: rule \"" <> rule.name <> "\" has no channel (set the rule channelConfig or the team's Mattermost channel)")
            else Right (teamName, channelName)

-- | mattermostTarget with the rule's team defaults fetched from its team.
mattermostTargetForRule :: (?modelContext :: ModelContext) => NotificationRule -> IO (Either Text (Text, Text))
mattermostTargetForRule rule = do
    teamDefaults <- case get #teamId rule of
        Just teamId -> do
            teamOrNothing <- fetchOneOrNothing teamId
            pure (maybe (Aeson.object []) (get #defaults) teamOrNothing)
        Nothing -> pure (Aeson.object [])
    pure (mattermostTarget teamDefaults rule)

-- | Initial delivery: resolve the channel, post the root message, post the
-- details into its thread, remember the mapping. When the (alert, rule)
-- already has a root post, falls through to a sync instead of duplicating
-- the channel post. A TERMINAL alert never gets a fresh card: the notify
-- job can outrun a fast source resolve (queued at expose, resolved before
-- the worker ran it) — posting then would leave an undeletable orphan card
-- (the resolve's sync already no-op'd on the not-yet-existing post).
deliverNotify :: (?modelContext :: ModelContext) => Alert -> NotificationRule -> IO (Either Text ())
deliverNotify alert rule = do
    existing <-
        query @MattermostPost
            |> filterWhere (#alertId, get #id alert)
            |> filterWhere (#notificationRuleId, Just (get #id rule))
            |> fetchOneOrNothing
    case existing of
        Just _ -> syncAlertPosts alert
        Nothing
            | isTerminalStatus (statusSnapshot alert) -> pure (Right ())
            | otherwise -> do
                configOrNothing <- mattermostConfigForRule rule
                case configOrNothing of
                    Nothing -> pure (Right ())
                    Just config -> deliver config
  where
    deliver config = do
        target <- mattermostTargetForRule rule
        case target of
            Left err -> pure (Left err)
            Right (teamName, channelName) -> do
                resolved <- Api.resolveChannel config teamName channelName
                case resolved of
                    Left err -> pure (Left err)
                    Right channelId -> do
                        rawContext <- renderContextFor (Just rule) alert
                        rootTemplate <- activeMattermostTemplate mattermostRootTemplateName
                        detailsTemplate <- activeMattermostTemplate mattermostDetailsTemplateName
                        statusTemplate <- activeMattermostTemplate mattermostStatusTemplateName
                        fieldsTemplate <- activeMattermostTemplate mattermostFieldsTemplateName
                        colorTemplate <- activeMattermostTemplate mattermostColorTemplateName
                        propsTemplate <- activeMattermostTemplate mattermostAttachmentTemplateName
                        let colorOverrides = Api.mmColorOverrides config
                            context = respectAckFlag rawContext config
                        root <- Api.createPost config channelId (renderRootMessage rootTemplate colorOverrides context alert) Nothing (renderRootProps statusTemplate colorTemplate fieldsTemplate propsTemplate colorOverrides context alert)
                        case root of
                            Left err -> pure (Left err)
                            Right rootPostId -> do
                                _ <- Api.createPost config channelId (renderDetailsMessage detailsTemplate colorOverrides context alert) (Just rootPostId) (Aeson.object [])
                                now <- getCurrentTime
                                _ <-
                                    newRecord @MattermostPost
                                        |> set #alertId (get #id alert)
                                        |> set #notificationRuleId (Just (get #id rule))
                                        |> set #rootPostId rootPostId
                                        |> set #channelId channelId
                                        |> set #renderedStatus (statusSnapshot alert)
                                        |> set #createdAt now
                                        |> set #updatedAt now
                                        |> createRecord
                                pure (Right ())

-- | Status sync: re-render and patch every root post the alert has. Each
-- post resolves its server config through the rule that created it (falling
-- back to the first enabled mattermost channel). Missing config or posts are
-- silent no-ops. A PATCH failure is RETURNED (the job layer turns it into a
-- retry with a visible last_error — a silently skipped patch leaves the MM
-- card stale forever, which is worse than a red job).
-- With the channel config "deleteOnClose": true, a sync landing on a
-- TERMINAL state (resolved/closed) DELETES the root post instead of patching
-- it and removes the MattermostPost row — the channel keeps no gray
-- tombstone, and a later refire re-delivers a fresh card.
syncAlertPosts :: (?modelContext :: ModelContext) => Alert -> IO (Either Text ())
syncAlertPosts alert = do
    posts <-
        query @MattermostPost
            |> filterWhere (#alertId, get #id alert)
            |> fetch
    if null posts
        then pure (Right ())
        else do
            fallback <- firstEnabledConfig
            results <- forM posts \post -> do
                configOrNothing <- case post.notificationRuleId of
                    Just ruleId -> do
                        rule <- fetch ruleId
                        mattermostConfigForRule rule
                    Nothing -> pure fallback
                case configOrNothing of
                    Nothing -> pure (Right ())
                    Just config
                        | Api.mmDeleteOnClose config && isTerminalStatus (statusSnapshot alert) ->
                            deleteTerminalPost config post
                    Just config -> do
                        rawContext <- renderContextFor Nothing alert
                        rootTemplate <- activeMattermostTemplate mattermostRootTemplateName
                        statusTemplate <- activeMattermostTemplate mattermostStatusTemplateName
                        fieldsTemplate <- activeMattermostTemplate mattermostFieldsTemplateName
                        colorTemplate <- activeMattermostTemplate mattermostColorTemplateName
                        propsTemplate <- activeMattermostTemplate mattermostAttachmentTemplateName
                        detailsTemplate <- activeMattermostTemplate mattermostDetailsTemplateName
                        let colorOverrides = Api.mmColorOverrides config
                            context = respectAckFlag rawContext config
                            rootMessage = renderRootMessage rootTemplate colorOverrides context alert
                            rootProps = renderRootProps statusTemplate colorTemplate fieldsTemplate propsTemplate colorOverrides context alert
                            detailsMessage = renderDetailsMessage detailsTemplate colorOverrides context alert
                        result <- Api.patchPost config post.rootPostId rootMessage rootProps
                        case result of
                            Right () -> touchPostRow post (statusSnapshot alert)
                            Left err
                                | statusCodeOfError err == Just 404 ->
                                    -- The root post is GONE (deleted in
                                    -- Mattermost by hand or an external
                                    -- cleanup) while the alert is still
                                    -- active: re-deliver a fresh card
                                    -- (root + details thread) instead of
                                    -- retrying a patch that can never land.
                                    recreateCard config post rootMessage rootProps detailsMessage (statusSnapshot alert)
                                | otherwise -> pure (Left err)
            pure case [err | Left err <- results] of
                [] -> Right ()
                (firstErr : _) -> Left firstErr
  where
    firstEnabledConfig = firstEnabledMattermostConfig

statusSnapshot :: Alert -> Text
statusSnapshot alert = alert.status

isTerminalStatus :: Text -> Bool
isTerminalStatus status = status `elem` (["resolved", "closed"] :: [Text])

-- | Patch-landed bookkeeping: stamp the rendered status on the row.
touchPostRow :: (?modelContext :: ModelContext) => MattermostPost -> Text -> IO (Either Text ())
touchPostRow post renderedStatus = do
    now <- getCurrentTime
    _ <- post |> set #renderedStatus renderedStatus |> set #updatedAt now |> updateRecord
    pure (Right ())

-- | The tracked root post no longer exists in Mattermost (404 on patch —
-- deleted by hand or an external cleanup) while the alert is still active:
-- re-deliver a fresh card (root + details thread) and point the row at it.
-- A create failure is RETURNED like a patch failure (job retry + visible
-- last_error).
recreateCard :: (?modelContext :: ModelContext) => MattermostConfig -> MattermostPost -> Text -> Aeson.Value -> Text -> Text -> IO (Either Text ())
recreateCard config post rootMessage rootProps detailsMessage renderedStatus = do
    root <- Api.createPost config post.channelId rootMessage Nothing rootProps
    case root of
        Left err -> pure (Left err)
        Right rootPostId -> do
            _ <- Api.createPost config post.channelId detailsMessage (Just rootPostId) (Aeson.object [])
            now <- getCurrentTime
            _ <- post |> set #rootPostId rootPostId |> set #renderedStatus renderedStatus |> set #updatedAt now |> updateRecord
            pure (Right ())

-- | The deleteOnClose terminal-state path: delete the ROOT post from-- Mattermost, then drop the MattermostPost row so no later sync touches the
-- (now gone) post. Deliberately root-only: the bot token is not a channel
-- admin, so posts it did not author (human thread replies) cannot be deleted
-- anyway, and the details reply it did author stays as the audit trail. A
-- delete failure is RETURNED like a patch failure (job retry + visible
-- last_error); a 404 is success (post already gone).
deleteTerminalPost :: (?modelContext :: ModelContext) => MattermostConfig -> MattermostPost -> IO (Either Text ())
deleteTerminalPost config post = do
    result <- Api.deletePost config post.rootPostId
    case result of
        Left err -> pure (Left err)
        Right () -> do
            _ <- deleteRecord post
            pure (Right ())

-- | The first enabled usable mattermost channel config (by channel name) —
-- the sync/purge fallback when a post has no rule to resolve its config
-- through.
firstEnabledMattermostConfig :: (?modelContext :: ModelContext) => IO (Maybe MattermostConfig)
firstEnabledMattermostConfig = do
    channels <-
        query @NotificationChannel
            |> filterWhere (#type_, "mattermost" :: Text)
            |> filterWhere (#enabled, True)
            |> orderByAsc #name
            |> fetch
    configs <- mapM Api.configForChannel channels
    pure (listToMaybe (catMaybes configs))

-- | Admin-purge result counts, flashed on the admin page after a run.
-- pmsTargetsFailed/pmsErrors surface the SILENT skips (rule without a usable
-- config, unresolvable target, failed channel scan) — an all-zeros summary
-- otherwise tells nothing about where the walk short-circuited.
data PurgeMattermostSummary = PurgeMattermostSummary
    { pmsPurged :: Int
    , pmsFailed :: Int
    , pmsUntracked :: Int
    , pmsKeptActive :: Int
    , pmsTargetsFailed :: Int
    , pmsErrors :: [Text]
    }
    deriving (Eq, Show)

instance Semigroup PurgeMattermostSummary where
    PurgeMattermostSummary a1 b1 c1 d1 e1 f1 <> PurgeMattermostSummary a2 b2 c2 d2 e2 f2 =
        PurgeMattermostSummary (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2) (e1 + e2) (f1 <> f2)

instance Monoid PurgeMattermostSummary where
    mempty = PurgeMattermostSummary 0 0 0 0 0 []

-- | One failed/skipped purge target with a human-readable reason.
purgeErr :: Text -> PurgeMattermostSummary
purgeErr err = mempty{pmsTargetsFailed = 1, pmsErrors = [err]}

-- | Which posts a purge deletes. PurgeResolvedPosts deletes only the
-- BOT-AUTHORED ROOT posts whose mapped alert is terminal (resolved/closed) --
-- the retroactive cleanup for channels without deleteOnClose. Everything
-- else (active alerts' posts, other authors' posts, untracked leftovers) is
-- left in place. PurgeUnrelatedPosts keeps ONLY the firing alerts' root
-- posts: every other root post in the channel (terminal or stale
-- acked/stalled cards, and root posts with no mattermost_posts row at all)
-- is deleted. The latter relies on the halemans MM user having CHANNEL
-- ADMIN rights -- it deletes posts regardless of the author.
data PurgeMattermostMode = PurgeResolvedPosts | PurgeUnrelatedPosts
    deriving (Eq, Show)

-- | Danger zone (channel admin UI): per-NOTIFICATION-CHANNEL variant of the
-- purge -- only the rules referencing THIS channel row are walked, resolving
-- each rule's MM target (rule channelConfig or team defaults). The global
-- 'purgeResolvedMattermostPosts' below is the same walk over all mattermost
-- channels.
purgeMattermostPostsForChannel :: (?modelContext :: ModelContext) => PurgeMattermostMode -> NotificationChannel -> IO PurgeMattermostSummary
purgeMattermostPostsForChannel mode channel
    | channel.type_ /= "mattermost" =
        pure (purgeErr ("channel \"" <> channel.name <> "\" is not of type mattermost"))
    | otherwise = do
        rules <-
            query @NotificationRule
                |> filterWhere (#channel, channel.name)
                |> fetch
        if null rules
            then pure (purgeErr ("no notification rules on channel \"" <> channel.name <> "\""))
            else purgeRules mode rules

-- | Global purge (kept for the retroactive-all use and tests): same walk as
-- the per-channel purge over every mattermost channel's rules.
purgeResolvedMattermostPosts :: (?modelContext :: ModelContext) => IO PurgeMattermostSummary
purgeResolvedMattermostPosts = do
    channelNames <-
        map (get #name)
            <$> ( query @NotificationChannel
                    |> filterWhere (#type_, "mattermost" :: Text)
                    |> fetch
                )
    if null channelNames
        then pure (purgeErr "no notification channel of type mattermost")
        else do
            rules <-
                query @NotificationRule
                    |> filterWhereIn (#channel, channelNames)
                    |> fetch
            if null rules
                then pure (purgeErr ("no notification rules on the mattermost channels " <> Text.intercalate ", " channelNames))
                else purgeRules PurgeResolvedPosts rules

-- | CHANNEL-FIRST retroactive cleanup. For every walked rule's target, lists
-- the channel's actual posts (paginated) and deletes the root posts the mode
-- rejects. Walking the channel (not the DB) means deletes are only attempted
-- for posts that still exist. Rules without a usable config or target, and
-- targets whose scan fails, are counted in pmsTargetsFailed with the reason
-- in pmsErrors. A target shared by several rules is scanned once per rule --
-- harmless, the second scan finds nothing left.
purgeRules :: (?modelContext :: ModelContext) => PurgeMattermostMode -> [NotificationRule] -> IO PurgeMattermostSummary
purgeRules mode rules = fmap mconcat $ forM rules \rule -> do
    configOrNothing <- mattermostConfigForRule rule
    targetOrError <- mattermostTargetForRule rule
    case (configOrNothing, targetOrError) of
        (Just config, Right (teamName, channelName)) ->
            purgeChannelTarget mode config teamName channelName
        (Nothing, _) ->
            pure (purgeErr ("rule \"" <> rule.name <> "\": no usable mattermost channel config (channel row missing/disabled, empty base URL or token env unset in the app environment)"))
        (_, Left err) ->
            pure (purgeErr ("rule \"" <> rule.name <> "\": " <> err))

purgeChannelTarget :: (?modelContext :: ModelContext) => PurgeMattermostMode -> MattermostConfig -> Text -> Text -> IO PurgeMattermostSummary
purgeChannelTarget mode config teamName channelName = do
    channelOrError <- Api.resolveChannel config teamName channelName
    case channelOrError of
        Left err -> pure (purgeErr (targetLabel <> " channel resolution: " <> err))
        Right channelId -> do
            botOrError <- case mode of
                PurgeResolvedPosts -> Api.getMe config
                PurgeUnrelatedPosts -> pure (Right "")
            case botOrError of
                Left err -> pure (purgeErr (targetLabel <> " users/me: " <> err))
                Right botUserId -> do
                    outcome <- Api.channelPostsFold config channelId (purgeStep botUserId) mempty
                    case outcome of
                        Left err -> pure (purgeErr (targetLabel <> " channel posts: " <> err))
                        Right summary -> pure summary
  where
    targetLabel = config.mmBaseUrl <> " " <> teamName <> "/" <> channelName
    purgeStep botUserId summary postValue
        | rootIdOf postValue /= Just "" = pure summary
        | PurgeResolvedPosts <- mode
        , userIdOf postValue /= Just botUserId =
            pure summary
        | otherwise = case postIdOf postValue of
            Nothing -> pure summary
            Just postId -> do
                pageSummary <- case mode of
                    PurgeResolvedPosts -> purgeOnePost config postId
                    PurgeUnrelatedPosts -> purgeOneUnrelatedPost config postId
                pure (summary <> pageSummary)
    rootIdOf value = parseMaybe (Aeson.withObject "post" (\o -> o Aeson..: "root_id")) value :: Maybe Text
    userIdOf value = parseMaybe (Aeson.withObject "post" (\o -> o Aeson..: "user_id")) value :: Maybe Text
    postIdOf value = parseMaybe (Aeson.withObject "post" (\o -> o Aeson..: "id")) value :: Maybe Text

purgeOnePost :: (?modelContext :: ModelContext) => MattermostConfig -> Text -> IO PurgeMattermostSummary
purgeOnePost config postId = do
    rows <-
        query @MattermostPost
            |> filterWhere (#rootPostId, postId)
            |> fetch
    case rows of
        [] -> pure mempty{pmsUntracked = 1}
        _ -> do
            alerts <- mapM (fetch . get #alertId) rows
            if any (isTerminalStatus . statusSnapshot) alerts
                then do
                    result <- Api.deletePost config postId
                    case result of
                        Left _ -> pure mempty{pmsFailed = 1}
                        Right () -> do
                            mapM_ deleteRecord rows
                            pure mempty{pmsPurged = 1}
                else pure mempty{pmsKeptActive = 1}

-- | PurgeUnrelatedPosts per-post decision: the post stays ONLY when at least
-- one mapped alert is still firing; everything else (terminal alerts' cards,
-- stale acked/stalled cards) is deleted along with its mattermost_posts
-- rows. A root post with NO rows (untracked leftover) is deleted too.
purgeOneUnrelatedPost :: (?modelContext :: ModelContext) => MattermostConfig -> Text -> IO PurgeMattermostSummary
purgeOneUnrelatedPost config postId = do
    rows <-
        query @MattermostPost
            |> filterWhere (#rootPostId, postId)
            |> fetch
    alerts <- mapM (fetch . get #alertId) rows
    if any (\alert -> statusSnapshot alert == "firing") alerts
        then pure mempty{pmsKeptActive = 1}
        else do
            result <- Api.deletePost config postId
            case result of
                Left _ -> pure mempty{pmsFailed = 1}
                Right () -> do
                    mapM_ deleteRecord rows
                    pure mempty{pmsPurged = 1}

nestedConfigText :: [Text] -> Aeson.Value -> Text
nestedConfigText [] (Aeson.String value) = value
nestedConfigText (key : rest) (Aeson.Object object_) =
    case KeyMap.lookup (Key.fromText key) object_ of
        Just inner -> nestedConfigText rest inner
        Nothing -> ""
nestedConfigText _ _ = ""

configText :: Text -> Aeson.Value -> Text
configText key config = case config of
    Aeson.Object object_ -> case KeyMap.lookup (Key.fromText key) object_ of
        Just (Aeson.String value) -> value
        _ -> ""
    _ -> ""

orElse :: Text -> Text -> Text
orElse value fallback = if Text.null value then fallback else value
