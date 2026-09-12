module Application.Service.Api.Token (
    allScopes,
    generateToken,
    hashToken,
    newApiToken,
    resolveToken,
) where

import Control.Monad (void)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString.Base64.URL as Base64Url
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Generated.Types
import IHP.Fetch (fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, query)
import "cryptonite" Crypto.Hash (SHA256 (..), hashWith)
import "cryptonite" Crypto.Random (getRandomBytes)

allScopes :: [Text]
allScopes = ["alerts:read", "metrics"]

-- 32 random bytes, base64url without padding (design_docs/milestone_6.md §4).
generateToken :: IO Text
generateToken = do
    bytes <- getRandomBytes 32
    pure (Text.Encoding.decodeUtf8 (Base64Url.encodeUnpadded bytes))

-- sha256 hex of the bearer secret; plaintext is never stored.
hashToken :: Text -> Text
hashToken plaintext = cs (convertToBase Base16 digest :: ByteString)
  where
    digest = hashWith SHA256 (Text.Encoding.encodeUtf8 plaintext)

-- Returns the created record and the plaintext secret (shown exactly once).
newApiToken :: (?modelContext :: ModelContext) => Id User -> Text -> [Text] -> Maybe UTCTime -> IO (ApiToken, Text)
newApiToken userId name scopes expiresAt = do
    plaintext <- generateToken
    record <-
        newRecord @ApiToken
            |> set #userId userId
            |> set #name name
            |> set #tokenHash (hashToken plaintext)
            |> set #prefix (Text.take 8 plaintext)
            |> set #scopes scopes
            |> set #expiresAt expiresAt
            |> createRecord
    pure (record, plaintext)

-- Hash lookup; rejects revoked/expired tokens and touches last_used_at at
-- most once per minute so hot polling loops don't write on every request.
resolveToken :: (?modelContext :: ModelContext) => Text -> IO (Maybe ApiToken)
resolveToken plaintext = do
    candidate <-
        query @ApiToken
            |> filterWhere (#tokenHash, hashToken plaintext)
            |> fetchOneOrNothing
    now <- getCurrentTime
    case candidate of
        Just token
            | isNothing token.revokedAt
            , maybe True (> now) token.expiresAt -> do
                touchLastUsedAt now token
                pure (Just token)
        _ -> pure Nothing

touchLastUsedAt :: (?modelContext :: ModelContext) => UTCTime -> ApiToken -> IO ()
touchLastUsedAt now token = case token.lastUsedAt of
    Just lastUsed | diffUTCTime now lastUsed < 60 -> pure ()
    _ -> void (token |> set #lastUsedAt (Just now) |> updateRecord)
