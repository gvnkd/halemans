module Application.Helper.I18n (
    currentLanguage,
    tr,
    trp,
) where

import Application.Helper.Controller ()
import Application.Service.I18n (Language (..), languageFromSettings, translate, translateParams)
import Generated.Types
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord, currentUserOrNothing)
import IHP.Prelude
import Network.Wai (Request)

-- Request-scoped translation helpers: the active language comes from the
-- current user's settings (users.settings.language), English for anonymous
-- requests. Available in every view via Web.View.Prelude and in controllers
-- via Web.Controller.Prelude.

currentLanguage :: (CurrentUserRecord ~ User, ?request :: Request) => Language
currentLanguage = maybe LangEn (languageFromSettings . (.settings)) currentUserOrNothing

tr :: (CurrentUserRecord ~ User, ?request :: Request) => Text -> Text
tr key = translate currentLanguage key

trp :: (CurrentUserRecord ~ User, ?request :: Request) => Text -> [(Text, Text)] -> Text
trp key = translateParams currentLanguage key
