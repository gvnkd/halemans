module Web.Controller.NotificationChannels where

import Application.Service.Mattermost (PurgeMattermostMode (..), PurgeMattermostSummary (..), purgeMattermostPostsForChannel)
import qualified Application.Service.Mattermost.Api as MattermostApi
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import System.IO (hFlush, stdout)
import Web.Controller.Prelude
import Web.View.NotificationChannels.Edit
import Web.View.NotificationChannels.Index
import Web.View.NotificationChannels.New

-- Admin → Notification channels: notification_rules.channel references a
-- channel row by name; the row carries the delivery type, the server base
-- URL (mattermost) and a config tokenEnv naming the env var that holds the
-- secret — the same credential pattern as zabbix/grafana sources.
instance Controller NotificationChannelsController where
    beforeAction = ensureIsUser

    action NotificationChannelsAction = do
        requirePrivilege "manage_rules"
        channels <-
            query @NotificationChannel
                |> orderByAsc #name
                |> fetch
        referenced <- ruleChannelNames
        render IndexView{channels, referenced}
    action NewNotificationChannelAction = do
        requirePrivilege "manage_rules"
        render NewView
    action CreateNotificationChannelAction = do
        requirePrivilege "manage_rules"
        let name = Text.strip (param @Text "name")
        existing <- query @NotificationChannel |> filterWhere (#name, name) |> fetchOneOrNothing
        case existing of
            Just _ -> do
                setErrorMessage (trp "Notification channel {name} already exists" [("name", name)])
                redirectTo NewNotificationChannelAction
            Nothing -> do
                _ <- createRecord (channelFromForm name (newRecord @NotificationChannel))
                setSuccessMessage (tr "Notification channel created")
                redirectTo NotificationChannelsAction
    action EditNotificationChannelAction{notificationChannelId} = do
        requirePrivilege "manage_rules"
        channel <- fetch notificationChannelId
        ensureNotProtected channel.name (get #protected channel)
        render EditView{channel}
    action UpdateNotificationChannelAction{notificationChannelId} = do
        requirePrivilege "manage_rules"
        channel <- fetch notificationChannelId
        ensureNotProtected channel.name (get #protected channel)
        let name = Text.strip (param @Text "name")
        existing <-
            query @NotificationChannel
                |> filterWhere (#name, name)
                |> fetchOneOrNothing
        case existing of
            Just other | other.id /= channel.id -> do
                setErrorMessage (trp "Notification channel {name} already exists" [("name", name)])
                redirectTo EditNotificationChannelAction{notificationChannelId}
            _ -> do
                _ <- updateRecord (channelFromForm name channel)
                setSuccessMessage (tr "Notification channel updated")
                redirectTo NotificationChannelsAction
    action ToggleNotificationChannelAction{notificationChannelId} = do
        requirePrivilege "manage_rules"
        channel <- fetch notificationChannelId
        ensureNotProtected channel.name (get #protected channel)
        _ <- channel |> set #enabled (not channel.enabled) |> updateRecord
        setSuccessMessage (if channel.enabled then tr "Notification channel disabled" else tr "Notification channel enabled")
        redirectTo NotificationChannelsAction
    action DeleteNotificationChannelAction{notificationChannelId} = do
        requirePrivilege "manage_rules"
        channel <- fetch notificationChannelId
        ensureNotProtected channel.name (get #protected channel)
        referenced <- ruleChannelNames
        if channel.name `elem` referenced
            then do
                setErrorMessage (trp "Cannot delete {name}: notification rules still reference it" [("name", channel.name)])
                redirectTo NotificationChannelsAction
            else do
                deleteRecord channel
                setSuccessMessage (tr "Notification channel deleted")
                redirectTo NotificationChannelsAction
    action TestNotificationChannelAction{notificationChannelId} = do
        requirePrivilege "manage_rules"
        channel <- fetch notificationChannelId
        result <- testChannel channel
        case result of
            Left err -> setErrorMessage (trp "Channel test failed: {error}" [("error", err)])
            Right message -> setSuccessMessage message
        redirectTo NotificationChannelsAction

    -- Danger zone (channel page): per-channel retroactive purge of the bot's
    -- root posts of resolved/closed alerts, walking only THIS channel row's
    -- rules' MM targets. Moved here from the admin page -- the channel admin
    -- runs it per channel, not globally.
    action PurgeResolvedNotificationChannelAction{notificationChannelId} = do
        requirePrivilege "manage_rules"
        channel <- fetch notificationChannelId
        summary <- purgeMattermostPostsForChannel PurgeResolvedPosts channel
        flashPurgeSummary
            summary
            ( trp
                "Mattermost purge: {purged} deleted, {failed} failed, {untracked} untracked left, {kept} active kept, {targets} targets failed"
                [ ("purged", tshow summary.pmsPurged)
                , ("failed", tshow summary.pmsFailed)
                , ("untracked", tshow summary.pmsUntracked)
                , ("kept", tshow summary.pmsKeptActive)
                , ("targets", tshow summary.pmsTargetsFailed)
                ]
            )
        redirectTo EditNotificationChannelAction{notificationChannelId}
    -- Danger zone (channel page): delete ALL root posts in this channel's MM
    -- targets except the firing alerts' posts (terminal/stale cards AND
    -- untracked leftovers go). Requires the halemans MM user to be a channel
    -- admin -- it deletes posts of any author.
    action PurgeUnrelatedNotificationChannelAction{notificationChannelId} = do
        requirePrivilege "manage_rules"
        channel <- fetch notificationChannelId
        summary <- purgeMattermostPostsForChannel PurgeUnrelatedPosts channel
        flashPurgeSummary
            summary
            ( trp
                "Mattermost purge: {purged} deleted, {failed} failed, {kept} firing kept, {targets} targets failed"
                [ ("purged", tshow summary.pmsPurged)
                , ("failed", tshow summary.pmsFailed)
                , ("kept", tshow summary.pmsKeptActive)
                , ("targets", tshow summary.pmsTargetsFailed)
                ]
            )
        redirectTo EditNotificationChannelAction{notificationChannelId}

-- | Flash a purge summary; the first skip reasons ride along in the flash
-- (operators must not need log access to see WHY a target failed), the full
-- list also goes to the app log (stdout is block-buffered under docker,
-- hence the explicit flush).
flashPurgeSummary :: (?context :: ControllerContext, ?request :: Request) => PurgeMattermostSummary -> Text -> IO ()
flashPurgeSummary summary message = do
    let reasons = Text.intercalate " | " (take 2 summary.pmsErrors)
        detail = if Text.null reasons then "" else " -- " <> reasons
    setSuccessMessage (message <> detail)
    mapM_ (\err -> putStrLn ("mattermost purge: " <> err)) (take 10 summary.pmsErrors)
    hFlush stdout

-- | Mattermost connectivity check: resolve the config (base URL + tokenEnv)
-- and hit /api/v4/users/me. Other channel types have nothing to probe yet.
testChannel :: (?modelContext :: ModelContext) => NotificationChannel -> IO (Either Text Text)
testChannel channel = case channel.type_ of
    "mattermost" -> do
        configOrNothing <- MattermostApi.configForChannel channel
        case configOrNothing of
            Nothing -> pure (Left "base URL empty or token env var unset")
            Just config -> do
                result <- MattermostApi.testConnection config
                pure (fmap (\username -> "Connected to Mattermost as " <> username) result)
    _ -> pure (Right "Nothing to test for this channel type")

channelFromForm :: (?request :: Request, ?respond :: Respond) => Text -> NotificationChannel -> NotificationChannel
channelFromForm name channel =
    channel
        |> set #name name
        |> set #type_ (param @Text "type")
        |> set #baseUrl (Text.strip (param @Text "baseUrl"))
        |> set #config (channelConfigJson (configOf channel) (param @Text "tokenEnv") ackEnabled deleteOnClose)
        |> set #enabled (paramOrNothing @Text "enabled" == Just "on")
  where
    -- Unchecked = the key is ABSENT from config (absence means "enabled",
    -- matching ackActionEnabledFromJson's default).
    ackEnabled = paramOrNothing @Text "ackAction" == Just "on"
    -- Same convention: absence means False (deleteOnCloseEnabledFromJson's
    -- default), so the managed key is only written when checked.
    deleteOnClose = paramOrNothing @Text "deleteOnClose" == Just "on"

-- | Form-managed keys overlay the existing config so provisioned/hand-set
-- keys (colors, future keys) survive a UI edit (sourceConfig pattern).
channelConfigJson :: Value -> Text -> Bool -> Bool -> Value
channelConfigJson base tokenEnv ackEnabled deleteOnClose =
    Aeson.Object (extra <> managed)
  where
    managed =
        KeyMap.fromList
            ( ["tokenEnv" .= tokenEnv | tokenEnv /= ""]
                <> ["ackAction" .= False | not ackEnabled]
                <> ["deleteOnClose" .= True | deleteOnClose]
            )
    managedKeys = ["tokenEnv", "ackAction", "deleteOnClose"]
    extra = case base of
        Aeson.Object object_ -> KeyMap.filterWithKey (\key _ -> Key.toText key `notElem` managedKeys) object_
        _ -> mempty

configOf :: NotificationChannel -> Value
configOf = get #config

ruleChannelNames :: (?modelContext :: ModelContext) => IO [Text]
ruleChannelNames = do
    rules <- query @NotificationRule |> fetch
    pure (nub [rule.channel | rule <- rules, rule.channel /= ""])
