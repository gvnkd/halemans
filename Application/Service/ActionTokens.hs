module Application.Service.ActionTokens (
    ensureActionToken,
    consumeActionToken,
    actionTokenExpiryHours,
) where

import Control.Monad (void)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString.Base64.URL as Base64Url
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
import "cryptonite" Crypto.Hash (SHA256 (..), hashWith)
import "cryptonite" Crypto.Random (getRandomBytes)

-- One-time capability tokens for external interactions (Sergey 2026-10-01:
-- replay-safe links, "any other external interaction" extensible via the
-- action column — 'ack' today, unack/close later). Only the SHA-256 hash is
-- stored; the plaintext lives solely in the rendered link. Consumption is
-- an atomic UPDATE ... WHERE used_at IS NULL RETURNING, so concurrent or
-- repeated use of the same link succeeds at most once.

-- | How long an unused token stays valid.
actionTokenExpiryHours :: NominalDiffTime
actionTokenExpiryHours = 24 * 7

-- | Plaintext token for (alert, action), creating it when the firing alert
-- is re-rendered: any previous UNUSED tokens for the pair are expired
-- (rotation — only the newest rendered link works; consumed ones stay as
-- audit), then a fresh random token is inserted and returned.
ensureActionToken :: (?modelContext :: ModelContext) => Id Alert -> Text -> IO Text
ensureActionToken alertId action = do
    now <- getCurrentTime
    _ <-
        sqlExecTyped
            [typedSql|
        UPDATE action_tokens SET used_at = ${now}, expires_at = ${now}
        WHERE alert_id = ${alertId} AND action = ${action} AND used_at IS NULL
    |]
    tokenBytes <- getRandomBytes 24
    let token = cs (Base64Url.encodeUnpadded tokenBytes)
        tokenHash = hashToken token
        expiresAt = addUTCTime actionTokenExpiryHours now
    void $
        sqlExecTyped
            [typedSql|
        INSERT INTO action_tokens (alert_id, action, token_hash, expires_at)
        VALUES (${alertId}, ${action}, ${tokenHash}, ${expiresAt})
    |]
    pure token

-- | Atomically claim a token: succeeds (and marks used) exactly once per
-- token, only for the expected action and alert, only before expiry.
consumeActionToken :: (?modelContext :: ModelContext) => Text -> Id Alert -> Text -> IO Bool
consumeActionToken action alertId token = do
    now <- getCurrentTime
    let tokenHash = hashToken token
    rows <-
        sqlQueryTyped
            [typedSql|
        UPDATE action_tokens SET used_at = ${now}
        WHERE token_hash = ${tokenHash}
          AND action = ${action}
          AND alert_id = ${alertId}
          AND used_at IS NULL
          AND expires_at > ${now}
        RETURNING token_hash
    |]
    pure (not (null rows))

hashToken :: Text -> Text
hashToken token =
    cs (convertToBase Base16 (hashWith SHA256 (cs token :: ByteString)) :: ByteString)
