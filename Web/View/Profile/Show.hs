module Web.View.Profile.Show where

import Application.Helper.Theme (themes)
import Application.Helper.Timezone (timezones)
import Application.Service.Api.Token (allScopes)
import Application.Service.I18n (Language, languageCode, languages)
import qualified Data.Text as Text
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Fragments (calloutInfoHtml, calloutWarningHtml, emptyStateHtml, inlinePostFormHtml, pageHeaderHtml, sectionHeaderHtml)
import Web.View.Prelude

data ShowView = ShowView
    { subscriptions :: [PushSubscription]
    , pushPublicKey :: Maybe Text
    , currentTheme :: Text
    , currentTimezone :: Maybe Text
    , currentLanguage :: Language
    , apiTokens :: [ApiToken]
    , newToken :: Maybe Text
    }

instance View ShowView where
    html ShowView{..} =
        [hsx|
        {pageHeaderHtml (tr "Profile") mempty}
        <p class="text-muted">{currentUser.email}</p>

        {sectionHeaderHtml (tr "Theme") mempty}
        <div class="seg mb-3" data-testid="theme-picker">
            {forEach themes themeButton}
        </div>

        {sectionHeaderHtml (tr "Timezone") mempty}
        <form method="POST" action={UpdateTimezoneAction} class="mb-3" data-testid="timezone-form">
            <select name="timezone" class="form-select w-auto" data-autosubmit="" data-testid="timezone-select">
                <option value="" selected={isNothing currentTimezone}>{tr "Browser default"}</option>
                {forEach timezones timezoneOption}
            </select>
            <p class="form-text mb-0">{tr "Timestamps render in this timezone; the default follows your browser."}</p>
        </form>

        {sectionHeaderHtml (tr "Language") mempty}
        <form method="POST" action={UpdateLanguageAction} class="mb-3" data-testid="language-form">
            <select name="language" class="form-select w-auto" data-autosubmit="" data-testid="language-select">
                {forEach languages languageOption}
            </select>
            <p class="form-text mb-0">{tr "UI language; also used as the prompt language for LLM analyses you queue."}</p>
        </form>

        {sectionHeaderHtml (tr "API tokens") mempty}
        {newTokenBanner}
        <form method="POST" action={CreateApiTokenAction} class="mb-3" data-testid="api-token-create-form">
            <div class="row g-2 align-items-end">
                <div class="col-auto">
                    <label class="form-label">{tr "Name"}</label>
                    <input type="text" name="name" class="form-control" data-testid="api-token-name"/>
                </div>
                <div class="col-auto">
                    {forEach allScopes scopeCheckbox}
                </div>
                <div class="col-auto">
                    <button type="submit" class="btn btn-brand" data-testid="api-token-create">{tr "Create token"}</button>
                </div>
            </div>
        </form>
        {tokenTable}

        {sectionHeaderHtml (tr "Push notifications") mempty}
        {pushSection}

        {sectionHeaderHtml (tr "Dashboards") dashboardsLink}
    |]
      where
        tokenTable =
            if null apiTokens
                then emptyStateHtml "api-tokens-empty" (tr "No tokens yet — create one to get a hlm_… key")
                else
                    [hsx|
        <table class="table" data-testid="api-tokens-table">
            <thead>
                <tr>
                    <th>{tr "Name"}</th>
                    <th>{tr "Prefix"}</th>
                    <th>{tr "Scopes"}</th>
                    <th>{tr "Last used"}</th>
                    <th>{tr "Expires"}</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach apiTokens renderApiTokenRow}
            </tbody>
        </table>
                    |]
        dashboardsLink = [hsx|<a href={DashboardsAction} data-testid="profile-dashboards-link">{tr "Manage my dashboards"}</a>|]
        themeButton theme = themeChoiceButton currentTheme theme
        timezoneOption timezone =
            [hsx|<option value={timezone} selected={currentTimezone == Just timezone}>{timezone}</option>|]
        languageOption (code, label) =
            [hsx|<option value={code} selected={languageCode currentLanguage == code}>{label}</option>|]
        newTokenBanner = case newToken of
            Nothing -> mempty
            Just plaintext ->
                calloutInfoHtml "api-token-created" [hsx|<p class="mb-1">{tr "Token created. Copy it now — it is shown exactly once and never stored."}</p> <code data-testid="api-token-plaintext">{plaintext}</code>|]
        pushSection = case pushPublicKey of
            Nothing ->
                calloutWarningHtml "push-unavailable" (tr "Push unavailable") [hsx|{tr "Push is not configured on this server (no VAPID keys)."}|]
            Just publicKey ->
                [hsx|
                    <div data-testid="push-settings" data-vapid-key={publicKey}>
                        <p>{trp "Push subscriptions registered for this account: {n}" [("n", tshow (length subscriptions))]}</p>
                        <button class="btn btn-sm btn-ghost" id="push-subscribe-button" data-testid="push-subscribe">{tr "Enable push for this browser"}</button>
                        <button class="btn btn-sm btn-ghost" id="push-unsubscribe-button" data-testid="push-unsubscribe">{tr "Disable"}</button>
                        <span id="push-status" data-testid="push-status"></span>
                    </div>
                |]

scopeCheckbox :: Text -> Html
scopeCheckbox scope =
    [hsx|
    <div class="form-check">
        <input class="form-check-input" type="checkbox" name={scopeParamName scope} id={scopeParamName scope} checked={scope == "alerts:read"} data-testid={scopeParamName scope}/>
        <label class="form-check-label" for={scopeParamName scope}>{scope}</label>
    </div>
|]
  where
    scopeParamName s = "scope_" <> Text.map (\c -> if c == ':' then '_' else c) s

renderApiTokenRow :: (CurrentUserRecord ~ User, ?request :: Request) => ApiToken -> Html
renderApiTokenRow token =
    let scopes :: Text
        scopes = Text.intercalate ", " token.scopes
        revoked = isJust token.revokedAt
     in [hsx|
    <tr data-testid="api-token-row">
        <td data-testid="api-token-row-name">{token.name}</td>
        <td><code>{token.prefix}</code></td>
        <td>{scopes}</td>
        <td data-testid="api-token-last-used">{utcTimeOrHtml (tr "never") token.lastUsedAt}</td>
        <td>{utcTimeOrHtml (tr "never") token.expiresAt}</td>
        <td>{revokeCell revoked}</td>
    </tr>
|]
  where
    revokeCell revoked
        | revoked = [hsx|<span class="badge bg-secondary" data-testid="api-token-revoked">{tr "revoked"}</span>|]
        | otherwise = inlinePostFormHtml (pathTo (RevokeApiTokenAction (get #id token))) (tr "Revoke") "btn btn-sm btn-ghost btn-ghost-critical" (Just "api-token-revoke") True

-- Clicking a choice swaps data-theme live and persists via POST
-- /profile/theme (static/app.js halemansApplyTheme).
themeChoiceButton :: Text -> Text -> Html
themeChoiceButton currentTheme theme =
    [hsx|
    <button type="button" class={buttonClass} data-theme-choice={theme} data-testid={"theme-choice-" <> theme}>{theme}</button>
|]
  where
    buttonClass :: Text
    buttonClass = "seg-item" <> (if theme == currentTheme then " active" else "")
