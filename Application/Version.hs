{-# LANGUAGE OverloadedStrings #-}
module Application.Version (appVersion) where

import IHP.Prelude

-- Single runtime-visible copy of the app version. The nix build filters the
-- app source down to .hs files + Makefile (IHP NixSupport appSrcInclude), so
-- Halemans.cabal is not readable at compile time. Keep in sync with the
-- `version:` field in Halemans.cabal (guarded by Test/Main.hs).
appVersion :: Text
appVersion = "1.15.1"
