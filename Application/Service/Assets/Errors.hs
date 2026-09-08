module Application.Service.Assets.Errors
( AssetsError (..)
, describeError
, notFoundPrefix
, classifyResponse
) where

import IHP.Prelude
import Data.Aeson (Value)
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text

-- Error model + classification per design_docs/assets-api.md §3: the plugin
-- answers business errors as JSON (shape A, Russian text), unregistered
-- routes as Jira framework XML (shape B), and unauth/unknown paths 302 to a
-- login page (shape C). Detection is by HTTP code + structure, never by the
-- localized message text.

data AssetsError
    = AuthFailed
    | NotFound Int64
    | Redirected
    | Upstream Int Text
    | InvalidResponse Int Text
    deriving (Eq, Show)

describeError :: AssetsError -> Text
describeError = \case
    AuthFailed -> "assets auth failed (401)"
    NotFound objectId -> "assets object not found: " <> tshow objectId
    Redirected -> "assets endpoint redirected (login page fallback)"
    Upstream code body -> "assets upstream error " <> tshow code <> ": " <> Text.take 200 body
    InvalidResponse code body -> "assets invalid response for status " <> tshow code <> ": " <> Text.take 200 body

-- Stable machine prefix inside shape-A errorMessages (§3). The remainder is
-- localized (ru_RU on the verified instance) and must never be matched.
notFoundPrefix :: Text
notFoundPrefix = "NotFoundInsightException:"

-- Classify a non-2xx response. requestObjectId is the object id from the
-- request path when the route carries one (for NotFound).
classifyResponse :: Maybe Int64 -> Int -> LByteString -> AssetsError
classifyResponse requestObjectId code body
    | code == 401 = AuthFailed
    | code >= 300 && code < 400 = Redirected
    | code == 404 && hasNotFoundMarker = NotFound (fromMaybe 0 requestObjectId)
    | otherwise = Upstream code (cs (BL.take 200 body))
    where
        hasNotFoundMarker = case Aeson.decode body of
            Nothing -> False
            Just value -> fromMaybe False (parseMaybe parser value)
        parser = Aeson.withObject "error" \o -> do
            messages <- o Aeson..: "errorMessages" :: Parser [Text]
            pure (any (notFoundPrefix `Text.isPrefixOf`) messages)
