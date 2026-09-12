module Application.Helper.Json (
    stringList,
) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import IHP.Prelude

-- Decode a jsonb column holding a JSON array of strings; anything else
-- (null, object, wrong element type) decodes to [].
stringList :: Value -> [Text]
stringList value = fromMaybe [] (parseMaybe Aeson.parseJSON value)
