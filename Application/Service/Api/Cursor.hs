module Application.Service.Api.Cursor
    ( Cursor (..)
    , encodeCursor
    , decodeCursor
    ) where

import IHP.Prelude
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.ByteString.Base64.URL as Base64Url
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)

-- Keyset cursor for GET /api/v1/alerts (design_docs/milestone_6.md §3):
-- the (last_seen_at, id) key of the last row of a page, base64url-encoded.
data Cursor = Cursor
    { cursorLastSeenAt :: !UTCTime
    , cursorAlertId :: !UUID
    } deriving (Eq, Show)

timeFormat :: String
timeFormat = "%Y-%m-%dT%H:%M:%S%Q"

encodeCursor :: Cursor -> Text
encodeCursor Cursor { .. } =
    let raw = cs (formatTime defaultTimeLocale timeFormat cursorLastSeenAt) <> "|" <> UUID.toText cursorAlertId :: Text
    in Text.Encoding.decodeUtf8 (Base64Url.encodeUnpadded (Text.Encoding.encodeUtf8 raw))

decodeCursor :: Text -> Maybe Cursor
decodeCursor encoded = do
    bytes <- either (const Nothing) Just (Base64Url.decodeUnpadded (Text.Encoding.encodeUtf8 encoded))
    raw <- either (const Nothing) Just (Text.Encoding.decodeUtf8' bytes)
    let (timePart, rest) = Text.breakOn "|" raw
    cursorAlertId <- UUID.fromText (Text.drop 1 rest)
    cursorLastSeenAt <- parseTimeM True defaultTimeLocale timeFormat (cs timePart)
    pure Cursor { .. }
