module Web.Controller.Profile where

import Web.Controller.Prelude
import Web.View.Profile.Show
import Application.Service.Push (vapidPublicKey)
import Application.Helper.Theme (themes, isValidTheme, themeFromSettings)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap

instance Controller ProfileController where
    beforeAction = ensureIsUser

    action ProfileAction = do
        subscriptions <- query @PushSubscription
            |> filterWhere (#userId, currentUserId)
            |> orderByDesc #createdAt
            |> fetch
        pushPublicKey <- vapidPublicKey
        let currentTheme = themeFromSettings currentUser.settings
        render ShowView { .. }

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
