module Web.Controller.NotificationChannels where

import qualified Application.Service.Mattermost.Api as MattermostApi
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
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
        |> set #config (channelConfigJson (configOf channel) (param @Text "tokenEnv"))
        |> set #enabled (paramOrNothing @Text "enabled" == Just "on")

-- | Form-managed keys overlay the existing config so provisioned/hand-set
-- keys survive a UI edit (sourceConfig pattern).
channelConfigJson :: Value -> Text -> Value
channelConfigJson base tokenEnv =
    Aeson.Object (extra <> managed)
  where
    managed =
        KeyMap.fromList
            (["tokenEnv" .= tokenEnv | tokenEnv /= ""])
    managedKeys = ["tokenEnv"]
    extra = case base of
        Aeson.Object object_ -> KeyMap.filterWithKey (\key _ -> Key.toText key `notElem` managedKeys) object_
        _ -> mempty

configOf :: NotificationChannel -> Value
configOf = get #config

ruleChannelNames :: (?modelContext :: ModelContext) => IO [Text]
ruleChannelNames = do
    rules <- query @NotificationRule |> fetch
    pure (nub [rule.channel | rule <- rules, rule.channel /= ""])
