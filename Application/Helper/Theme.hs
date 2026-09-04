module Application.Helper.Theme
( themes
, isValidTheme
, themeFromSettings
) where

import IHP.Prelude
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)

-- Theme packs (design_docs/milestone_3.md §7): CSS-variable packs switched
-- via data-theme on <html>; the chosen pack persists in users.settings.theme.

themes :: [Text]
themes = ["latte", "frappe", "macchiato", "dracula", "light", "dark"]

isValidTheme :: Text -> Bool
isValidTheme theme = theme `elem` themes

themeFromSettings :: Value -> Text
themeFromSettings settings =
    case parseMaybe (Aeson.withObject "settings" (\o -> o Aeson..: "theme")) settings of
        Just theme | isValidTheme theme -> theme
        _ -> "dark"
