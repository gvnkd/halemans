module Test.Helpers (
    atTime,
) where

import IHP.Prelude
import Text.Read (readMaybe)

-- Parse a "2026-09-04 10:00:00 UTC" literal; fails the spec on a bad literal.
atTime :: Text -> UTCTime
atTime raw = fromMaybe (error "bad utc literal") (readMaybe (cs raw))
