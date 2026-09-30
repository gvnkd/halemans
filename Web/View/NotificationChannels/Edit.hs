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
        </div></div>|]
