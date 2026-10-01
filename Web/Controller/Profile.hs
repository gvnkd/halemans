module Web.Controller.Profile where

import Application.Helper.Theme (isValidTheme, themeFromSettings, themes)
import Application.Helper.Timezone (isValidTimezone, timezoneFromSettings)
import Application.Service.AlertScope (alertScopeBypassFromSettings)
import Application.Service.Mattermost (mattermostUsernameFromSettings)
import Application.Service.Api.Token (allScopes, newApiToken)
import Application.Service.I18n (isValidLanguage, languageFromSettings, languages)
import Application.Service.Push (vapidPublicKey)
import Control.Monad (void)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime)
import IHP.ControllerSupport (respondAndExit)
import Network.HTTP.Types (status403)
import Network.Wai (responseLBS)
import Web.Controller.Prelude
import Web.View.Profile.Show

instance Controller ProfileController where
    beforeAction = ensureIsUser

    action ProfileAction = renderProfile Nothing
    -- Persisted theme preference (design_docs/milestone_3.md §7); the page
    -- itself swaps data-theme without reload, this just stores the choice.
    action UpdateThemeAction = do
        let theme = paramOrNothing @Text "theme" |> fromMaybe ""
        if isValidTheme theme
            then do
                let merged = case currentUser.settings of
                        Aeson.Object o -> Aeson.Object (KeyMap.insert "theme" (Aeson.String theme) o)
                        _ -> object ["theme" .= theme]
                _ <-
                    currentUser
                        |> set #settings merged
                        |> updateRecord
                renderJson (object ["ok" .= True])
            else renderJson (object ["ok" .= False, "error" .= ("unknown theme" :: Text)])

    -- Timezone preference for timestamp rendering (IANA name); empty value
    -- removes the key, meaning "browser default". Plain form POST, so this
    -- redirects back to the profile page.
    action UpdateTimezoneAction = do
        let timezone = paramOrNothing @Text "timezone" |> fromMaybe "" |> Text.strip
        if Text.null timezone || isValidTimezone timezone
            then do
                let merged = case currentUser.settings of
                        Aeson.Object o
                            | Text.null timezone -> Aeson.Object (KeyMap.delete "timezone" o)
                            | otherwise -> Aeson.Object (KeyMap.insert "timezone" (Aeson.String timezone) o)
                        _ -> object ["timezone" .= timezone]
                _ <-
                    currentUser
                        |> set #settings merged
                        |> updateRecord
                redirectTo ProfileAction
            else do
                setErrorMessage (trp "Unknown timezone: {timezone}" [("timezone", timezone)])
                redirectTo ProfileAction

    -- UI language (users.settings.language): drives tr/trp in views and is
    -- stamped onto manually queued LLM analyses as their prompt language.
    action UpdateLanguageAction = do
        let language = paramOrNothing @Text "language" |> fromMaybe "" |> Text.strip
        if isValidLanguage language
            then do
                let merged = case currentUser.settings of
                        Aeson.Object o -> Aeson.Object (KeyMap.insert "language" (Aeson.String language) o)
                        _ -> object ["language" .= language]
                _ <-
                    currentUser
                        |> set #settings merged
                        |> updateRecord
                redirectTo ProfileAction
            else do
                setErrorMessage (trp "Unknown language: {language}" [("language", language)])
                redirectTo ProfileAction

    -- Alert visibility bypass (users.settings.alertScopeBypass): when on,
    -- the user sees every alert regardless of team zabbix host groups.
    -- Plain form POST (checkbox), so this redirects back to the profile page.
    action UpdateAlertScopeAction = do
        let bypass = isJust (paramOrNothing @Text "bypass")
            merged = case currentUser.settings of
                Aeson.Object o -> Aeson.Object (KeyMap.insert "alertScopeBypass" (Aeson.Bool bypass) o)
                _ -> object ["alertScopeBypass" .= bypass]
        _ <-
            currentUser
                |> set #settings merged
                |> updateRecord
        redirectTo ProfileAction

    -- Mattermost username (users.settings.mattermostUsername): the MM Ack
    -- action matches the clicker to a Halemans user by this value. Empty
    -- input removes the key (the legacy displayName fallback still applies).
    action UpdateMattermostUserAction = do
        let raw = paramOrNothing @Text "mattermost_username" |> fromMaybe "" |> Text.strip
            username = fromMaybe raw (Text.stripPrefix "@" raw)
            merged = case currentUser.settings of
                Aeson.Object o
                    | Text.null username -> Aeson.Object (KeyMap.delete "mattermostUsername" o)
                    | otherwise -> Aeson.Object (KeyMap.insert "mattermostUsername" (Aeson.String username) o)
                _ -> object ["mattermostUsername" .= username]
        _ <-
            currentUser
                |> set #settings merged
                |> updateRecord
        redirectTo ProfileAction

    -- API token management (design_docs/milestone_6.md §4): the plaintext is
    -- rendered exactly once, straight from the POST (no redirect, no flash).
    action CreateApiTokenAction = do
        let name = paramOrNothing @Text "name" |> fromMaybe "" |> Text.strip
            scopes = [scope | scope <- allScopes, isJust (paramOrNothing @Text (cs (scopeParamName scope)))]
        if Text.null name || null scopes
            then do
                setErrorMessage (tr "API token needs a name and at least one scope.")
                redirectTo ProfileAction
            else do
                (_, plaintext) <- newApiToken currentUserId name scopes Nothing
                renderProfile (Just plaintext)
    action RevokeApiTokenAction{apiTokenId} = do
        token <- fetch apiTokenId
        if token.userId /= currentUserId
            then respondAndExit $ responseLBS status403 [("Content-Type", "text/html; charset=utf-8")] "<h1>403 — Forbidden</h1>"
            else do
                now <- getCurrentTime
                when (isNothing token.revokedAt) do
                    void (token |> set #revokedAt (Just now) |> updateRecord)
                redirectTo ProfileAction

scopeParamName :: Text -> Text
scopeParamName scope = "scope_" <> Text.map (\c -> if c == ':' then '_' else c) scope

renderProfile :: (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext) => Maybe Text -> IO ResponseReceived
renderProfile newToken = do
    subscriptions <-
        query @PushSubscription
            |> filterWhere (#userId, currentUserId)
            |> orderByDesc #createdAt
            |> fetch
    apiTokens <-
        query @ApiToken
            |> filterWhere (#userId, currentUserId)
            |> orderByDesc #createdAt
            |> fetch
    dashboardCount <-
        query @Dashboard
            |> filterWhere (#userId, currentUserId)
            |> fetchCount
    pushPublicKey <- vapidPublicKey
    let currentTheme = themeFromSettings currentUser.settings
        currentTimezone = timezoneFromSettings currentUser.settings
        currentLanguage = languageFromSettings currentUser.settings
        alertScopeBypass = alertScopeBypassFromSettings currentUser.settings
        mattermostUsername = mattermostUsernameFromSettings currentUser.settings
    render ShowView{..}
