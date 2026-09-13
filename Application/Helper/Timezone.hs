module Application.Helper.Timezone (
    timezones,
    isValidTimezone,
    timezoneFromSettings,
) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import IHP.Prelude

-- Per-user timezone for timestamp rendering: stored in
-- users.settings.timezone as a fixed-offset label ("UTC", "UTC+3", "UTC-4");
-- Nothing means "browser default" (static/app.js falls back to Date's local
-- zone). Rendering happens client-side; app.js maps the label onto the
-- matching Etc/GMT zone (sign inverted by IANA convention).
timezones :: [Text]
timezones =
    ["UTC-" <> tshow n | n <- reverse [1 .. 12 :: Int]]
        ++ ["UTC"]
        ++ ["UTC+" <> tshow n | n <- [1 .. 14 :: Int]]

isValidTimezone :: Text -> Bool
isValidTimezone timezone = timezone `elem` timezones

timezoneFromSettings :: Value -> Maybe Text
timezoneFromSettings settings =
    case parseMaybe (Aeson.withObject "settings" (\o -> o Aeson..: "timezone")) settings of
        Just timezone | isValidTimezone timezone -> Just timezone
        _ -> Nothing
