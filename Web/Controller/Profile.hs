module Web.Controller.Profile where

import Web.Controller.Prelude
import Web.View.Profile.Show
import Application.Service.Push (vapidPublicKey)
import Application.Helper.Theme (themes, isValidTheme, themeFromSettings)
import Application.Service.Api.Token (allScopes, newApiToken)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Control.Monad (void)
import Network.HTTP.Types (status403)
import Network.Wai (responseLBS)
import IHP.ControllerSupport (respondAndExit)

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
                _ <- currentUser
                    |> set #settings merged
                    |> updateRecord
                renderJson (object ["ok" .= True])
            else renderJson (object ["ok" .= False, "error" .= ("unknown theme" :: Text)])

    -- API token management (design_docs/milestone_6.md §4): the plaintext is
    -- rendered exactly once, straight from the POST (no redirect, no flash).
    action CreateApiTokenAction = do
        let name = paramOrNothing @Text "name" |> fromMaybe "" |> Text.strip
            scopes = [scope | scope <- allScopes, isJust (paramOrNothing @Text (cs (scopeParamName scope)))]
        if Text.null name || null scopes
            then do
                setErrorMessage "API token needs a name and at least one scope."
                redirectTo ProfileAction
            else do
                (_, plaintext) <- newApiToken currentUserId name scopes Nothing
                renderProfile (Just plaintext)

    action RevokeApiTokenAction { apiTokenId } = do
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
    subscriptions <- query @PushSubscription
        |> filterWhere (#userId, currentUserId)
        |> orderByDesc #createdAt
        |> fetch
    apiTokens <- query @ApiToken
        |> filterWhere (#userId, currentUserId)
        |> orderByDesc #createdAt
        |> fetch
    pushPublicKey <- vapidPublicKey
    let currentTheme = themeFromSettings currentUser.settings
    render ShowView { .. }
