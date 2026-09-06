module Application.Script.GenPassword where

import Application.Script.Prelude
import Crypto.PasswordStore (makePassword)
import System.Exit (exitFailure)

-- Prints the pwstore-fast pbkdf1 hash of a plaintext password, for use in a
-- provision config's users.items[].passwordHash (design_docs/milestone_7.md
-- §4). Container deployments without nix:
--   docker run --rm <image> /bin/GenPassword '<plaintext>'
run :: Script
run = do
    args <- getArgs
    case args of
        [password] -> do
            passwordHash <- makePassword (cs password) 17
            putStrLn (cs passwordHash)
        _ -> do
            putStrLn ("usage: GenPassword <plaintext-password>" :: Text)
            exitFailure
