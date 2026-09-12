module Application.Helper.Json
( stringList
) where

import IHP.Prelude
import Data.Aeson (Value)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson

-- Decode a jsonb column holding a JSON array of strings; anything else
-- (null, object, wrong element type) decodes to [].
stringList :: Value -> [Text]
stringList value = fromMaybe [] (parseMaybe Aeson.parseJSON value)
