module Web.View.NotificationChannels.Index where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Fragments (emptyStateHtml, enabledBadgeHtml, inlinePostFormHtml, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView
    { channels :: [NotificationChannel]
    , referenced :: [Text]
    }

instance View IndexView where
    beforeRender _ = setPageTitle (tr "Notification channels")
    html IndexView{..} =
        [hsx|
        <div>
        {pageHeaderHtml (tr "Notification channels") newButton}
        {tableOrEmpty}
        </div>
    |]
      where
        tableOrEmpty =
            if null channels
                then emptyStateHtml "notification-channels-empty" (tr "No notification channels yet — create one for rules to deliver through.")
                else
                    [hsx|
        <table class="table" data-testid="notification-channels-table">
            <thead>
                <tr>
                    <th>{tr "Name"}</th>
                    <th>{tr "Type"}</th>
                    <th>{tr "Base URL"}</th>
                    <th>{tr "Token env var"}</th>
                    <th>{tr "Enabled"}</th>
                    <th>{tr "Used by rules"}</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach channels (renderChannelRow referenced)}
            </tbody>
        </table>
                |]
        newButton = [hsx|<a href={NewNotificationChannelAction} class="btn btn-brand" data-testid="new-notification-channel">{tr "New channel"}</a>|]

renderChannelRow :: (CurrentUserRecord ~ User, ?request :: Request) => [Text] -> NotificationChannel -> Html
renderChannelRow referenced channel =
    [hsx|
    <tr data-testid="notification-channel-row">
        <td>{channel.name} {protectedBadgeHtml channel.protected}</td>
        <td>{channel.type_}</td>
        <td>{channel.baseUrl}</td>
        <td>{tokenEnvText channel}</td>
        <td>{enabledBadgeHtml channel.enabled}</td>
        <td>{usedBadge}</td>
        <td>
            <a href={EditNotificationChannelAction channel.id} class="btn btn-sm btn-ghost" data-testid="edit-notification-channel">{tr "Edit"}</a>
            {inlinePostFormHtml (pathTo (ToggleNotificationChannelAction channel.id)) toggleLabel "btn btn-sm btn-ghost" (Just "toggle-notification-channel") False}
            {inlinePostFormHtml (pathTo (TestNotificationChannelAction channel.id)) (tr "Test") "btn btn-sm btn-ghost" (Just "test-notification-channel") False}
            {deleteButton}
        </td>
    </tr>
|]
  where
    usedBadge =
        if channel.name `elem` referenced
            then [hsx|<span class="badge bg-secondary">{tr "yes"}</span>|]
            else mempty
    toggleLabel = if channel.enabled then tr "Disable" else tr "Enable"
    deleteButton =
        if channel.protected
            then mempty
            else inlinePostFormHtml (pathTo (DeleteNotificationChannelAction channel.id)) (tr "Delete") "btn btn-sm btn-ghost-critical" (Just "delete-notification-channel") True

tokenEnvText :: NotificationChannel -> Text
tokenEnvText channel = fromMaybe "" (parseMaybe (Aeson.withObject "config" (\o -> o Aeson..:? "tokenEnv" Aeson..!= "")) channel.config)
