module Application.Helper.Theme (
    themes,
    isValidTheme,
    themeFromSettings,
    bsTheme,
) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import IHP.Prelude

-- Theme packs (design_docs/milestone_3.md §7): CSS-variable packs switched
-- via data-theme on <html>; the chosen pack persists in users.settings.theme.

themes :: [Text]
themes = ["latte", "frappe", "macchiato", "dracula", "light", "dark", "halemans-dark", "halemans-light"]

isValidTheme :: Text -> Bool
isValidTheme theme = theme `elem` themes

themeFromSettings :: Value -> Text
themeFromSettings settings =
    case parseMaybe (Aeson.withObject "settings" (\o -> o Aeson..: "theme")) settings of
        Just theme | isValidTheme theme -> theme
        _ -> "dark"

-- Bootstrap 5.3 color mode mapped from each pack: without data-bs-theme
-- Bootstrap renders light placeholders/muted text on our dark surfaces.
bsTheme :: Text -> Text
bsTheme theme = if theme `elem` ["latte", "light", "halemans-light"] then "light" else "dark"
