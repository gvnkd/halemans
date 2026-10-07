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
) where

import Application.Service.ActionTokens (ensureActionToken)
import Application.Service.Mattermost.Api (MattermostConfig)
import qualified Application.Service.Mattermost.Api as Api
import Application.Service.Mattermost.Render (MattermostRenderContext (..), mattermostAttachmentTemplateName, mattermostColorTemplateName, mattermostDetailsTemplateName, mattermostFieldsTemplateName, mattermostRootTemplateName, mattermostStatusTemplateName, renderDetailsMessage, renderRootMessage, renderRootProps)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
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
-- the channel post.
deliverNotify :: (?modelContext :: ModelContext) => Alert -> NotificationRule -> IO (Either Text ())
deliverNotify alert rule = do
    existing <-
        query @MattermostPost
            |> filterWhere (#alertId, get #id alert)
            |> filterWhere (#notificationRuleId, Just (get #id rule))
            |> fetchOneOrNothing
    case existing of
        Just _ -> syncAlertPosts alert
        Nothing -> do
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
                        let colorOverrides = Api.mmColorOverrides config
                            context = respectAckFlag rawContext config
                        result <- Api.patchPost config post.rootPostId (renderRootMessage rootTemplate colorOverrides context alert) (renderRootProps statusTemplate colorTemplate fieldsTemplate propsTemplate colorOverrides context alert)
                        case result of
                            Left err -> pure (Left err)
                            Right () -> do
                                now <- getCurrentTime
                                _ <- post |> set #renderedStatus (statusSnapshot alert) |> set #updatedAt now |> updateRecord
                                pure (Right ())
            pure case [err | Left err <- results] of
                [] -> Right ()
                (firstErr : _) -> Left firstErr
  where
    firstEnabledConfig = do
        channels <-
            query @NotificationChannel
                |> filterWhere (#type_, "mattermost" :: Text)
                |> filterWhere (#enabled, True)
                |> orderByAsc #name
                |> fetch
        configs <- mapM Api.configForChannel channels
        pure (listToMaybe (catMaybes configs))

statusSnapshot :: Alert -> Text
statusSnapshot alert = alert.status

isTerminalStatus :: Text -> Bool
isTerminalStatus status = status `elem` (["resolved", "closed"] :: [Text])

-- | The deleteOnClose terminal-state path: delete the ROOT post from
-- Mattermost, then drop the MattermostPost row so no later sync touches the
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
