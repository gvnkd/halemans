module Web.View.Profile.Show where
import Web.View.Prelude
import Application.Helper.Theme (themes)

data ShowView = ShowView
    { subscriptions :: [PushSubscription]
    , pushPublicKey :: Maybe Text
    , currentTheme :: Text
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <h1>Profile</h1>
        <p>{currentUser.email}</p>

        <h2>Theme</h2>
        <div class="theme-picker mb-3" data-testid="theme-picker">
            {forEach themes themeButton}
        </div>

        <h2>Push notifications</h2>
        {pushSection}

        <h2>Dashboards</h2>
        <p><a href={DashboardsAction} data-testid="profile-dashboards-link">Manage my dashboards</a></p>
    |]
        where
            themeButton theme = themeChoiceButton currentTheme theme
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

-- Clicking a choice swaps data-theme live and persists via POST
-- /profile/theme (static/app.js halemansApplyTheme).
themeChoiceButton :: Text -> Text -> Html
themeChoiceButton currentTheme theme = [hsx|
    <button type="button" class={buttonClass} data-theme-choice={theme} data-testid={"theme-choice-" <> theme}>{theme}</button>
|]
    where
        buttonClass :: Text
        buttonClass = "btn btn-sm btn-outline-secondary me-1" <> (if theme == currentTheme then " active" else "")
