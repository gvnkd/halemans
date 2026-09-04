module Web.View.Profile.Show where
import Web.View.Prelude

data ShowView = ShowView
    { subscriptions :: [PushSubscription]
    , pushPublicKey :: Maybe Text
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <h1>Profile</h1>
        <p>{currentUser.email}</p>

        <h2>Push notifications</h2>
        {pushSection}

        <h2>Dashboard</h2>
        <p class="text-muted">Default dashboard selection lands with user dashboards in a later phase.</p>
    |]
        where
            pushSection = case pushPublicKey of
                Nothing -> [hsx|
                    <p class="text-warning" data-testid="push-unavailable">Push is not configured on this server (no VAPID keys).</p>
                |]
                Just publicKey -> [hsx|
                    <div data-testid="push-settings" data-vapid-key={publicKey}>
                        <p>{length subscriptions} subscription(s) registered for this account.</p>
                        <button class="btn btn-sm btn-primary" id="push-subscribe-button" data-testid="push-subscribe">Enable push for this browser</button>
                        <button class="btn btn-sm btn-outline-secondary" id="push-unsubscribe-button" data-testid="push-unsubscribe">Disable</button>
                        <span id="push-status" data-testid="push-status"></span>
                    </div>
                |]
