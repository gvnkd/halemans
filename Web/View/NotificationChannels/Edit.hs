module Web.View.NotificationChannels.Edit where

import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.NotificationChannels.Form (channelFormFields, valuesFromChannel)
import Web.View.Prelude

data EditView = EditView
    { channel :: NotificationChannel
    }

instance View EditView where
    beforeRender _ = setPageTitle (tr "Notification channels")
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit notification channel") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={UpdateNotificationChannelAction channel.id} data-testid="notification-channel-form">
            {channelFormFields (valuesFromChannel channel)}
            <button type="submit" class="btn btn-brand" data-testid="notification-channel-submit">{tr "Save"}</button>
        </form>
        </div></div>
        {purgeDangerZone}|]
      where
        purgeDangerZone =
            if channel.type_ == "mattermost"
                then
                    [hsx|
                    <div class="card maxw-600 mt-3"><div class="card-body">
                        <form method="POST" action={PurgeResolvedNotificationChannelAction channel.id} data-confirm={tr "Delete the bot's root posts of resolved/closed alerts in this channel's Mattermost targets? Posts of active alerts are kept."}>
                            <button type="submit" class="btn btn-ghost btn-ghost-critical" data-testid="purge-mattermost-resolved">{tr "Purge resolved Mattermost posts"}</button>
                        </form>
                        <form method="POST" action={PurgeUnrelatedNotificationChannelAction channel.id} data-confirm={tr "Delete ALL root posts in this channel's Mattermost targets except the firing alerts' posts? This cannot be undone."} class="mt-2">
                            <button type="submit" class="btn btn-ghost btn-ghost-critical" data-testid="purge-mattermost-unrelated">{tr "Purge all unrelated posts"}</button>
                        </form>
                    </div></div>|]
                else mempty
