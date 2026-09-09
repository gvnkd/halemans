module Web.View.Profile.Show where
import Web.View.Prelude
import Web.View.Fragments (inlinePostFormHtml)
import Application.Helper.Theme (themes)
import Application.Service.Api.Token (allScopes)
import qualified Data.Text as Text

data ShowView = ShowView
    { subscriptions :: [PushSubscription]
    , pushPublicKey :: Maybe Text
    , currentTheme :: Text
    , apiTokens :: [ApiToken]
    , newToken :: Maybe Text
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <h1>Profile</h1>
        <p>{currentUser.email}</p>

        <h2>Theme</h2>
        <div class="theme-picker mb-3" data-testid="theme-picker">
            {forEach themes themeButton}
        </div>

        <h2>API tokens</h2>
        {newTokenBanner}
        <form method="POST" action={CreateApiTokenAction} class="mb-3" data-testid="api-token-create-form">
            <div class="row g-2 align-items-end">
                <div class="col-auto">
                    <label class="form-label">Name</label>
                    <input type="text" name="name" class="form-control" data-testid="api-token-name"/>
                </div>
                <div class="col-auto">
                    {forEach allScopes scopeCheckbox}
                </div>
                <div class="col-auto">
                    <button type="submit" class="btn btn-primary" data-testid="api-token-create">Create token</button>
                </div>
            </div>
        </form>
        <table class="table" data-testid="api-tokens-table">
            <thead>
                <tr>
                    <th>Name</th>
                    <th>Prefix</th>
                    <th>Scopes</th>
                    <th>Last used</th>
                    <th>Expires</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach apiTokens renderApiTokenRow}
            </tbody>
        </table>

        <h2>Push notifications</h2>
        {pushSection}

        <h2>Dashboards</h2>
        <p><a href={DashboardsAction} data-testid="profile-dashboards-link">Manage my dashboards</a></p>
    |]
        where
            themeButton theme = themeChoiceButton currentTheme theme
            newTokenBanner = case newToken of
                Nothing -> mempty
                Just plaintext -> [hsx|
                    <div class="alert alert-success" data-testid="api-token-created">
                        <p class="mb-1">Token created. Copy it now — it is shown exactly once and never stored.</p>
                        <code data-testid="api-token-plaintext">{plaintext}</code>
                    </div>
                |]
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

scopeCheckbox :: Text -> Html
scopeCheckbox scope = [hsx|
    <div class="form-check">
        <input class="form-check-input" type="checkbox" name={scopeParamName scope} id={scopeParamName scope} checked={scope == "alerts:read"} data-testid={scopeParamName scope}/>
        <label class="form-check-label" for={scopeParamName scope}>{scope}</label>
    </div>
|]
    where scopeParamName s = "scope_" <> Text.map (\c -> if c == ':' then '_' else c) s

renderApiTokenRow :: ApiToken -> Html
renderApiTokenRow token =
    let scopes :: Text
        scopes = Text.intercalate ", " token.scopes
        revoked = isJust token.revokedAt
    in [hsx|
    <tr data-testid="api-token-row">
        <td data-testid="api-token-row-name">{token.name}</td>
        <td><code>{token.prefix}</code></td>
        <td>{scopes}</td>
        <td data-testid="api-token-last-used">{utcTimeOrHtml "never" token.lastUsedAt}</td>
        <td>{utcTimeOrHtml "never" token.expiresAt}</td>
        <td>{revokeCell revoked}</td>
    </tr>
|]
    where
        revokeCell revoked
            | revoked = [hsx|<span class="badge bg-secondary" data-testid="api-token-revoked">revoked</span>|]
            | otherwise = inlinePostFormHtml (pathTo (RevokeApiTokenAction (get #id token))) "Revoke" "btn btn-sm btn-outline-danger" (Just "api-token-revoke") False

-- Clicking a choice swaps data-theme live and persists via POST
-- /profile/theme (static/app.js halemansApplyTheme).
themeChoiceButton :: Text -> Text -> Html
themeChoiceButton currentTheme theme = [hsx|
    <button type="button" class={buttonClass} data-theme-choice={theme} data-testid={"theme-choice-" <> theme}>{theme}</button>
|]
    where
        buttonClass :: Text
        buttonClass = "btn btn-sm btn-outline-secondary me-1" <> (if theme == currentTheme then " active" else "")
