module Web.View.NotificationChannels.New where

import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.NotificationChannels.Form (channelFormFields, defaultChannelFormValues)
import Web.View.Prelude

data NewView = NewView

instance View NewView where
    beforeRender _ = setPageTitle (tr "Notification channels")
    html NewView =
        [hsx|
        {pageHeaderHtml (tr "New notification channel") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={CreateNotificationChannelAction} data-testid="notification-channel-form">
            {channelFormFields defaultChannelFormValues}
            <button type="submit" class="btn btn-brand" data-testid="notification-channel-submit">{tr "Create"}</button>
        </form>
        </div></div>|]
