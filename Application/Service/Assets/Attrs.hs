module Application.Service.Assets.Attrs
( objectAttributes
, configuredAttrNames
) where

import IHP.Prelude
import Generated.Types
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text

-- Flattened-attribute readers shared by the card fragment and the LLM
-- prompt excerpt (milestone_8.md §5/§6).

objectAttributes :: AssetsObject -> [(Text, Text)]
objectAttributes object = fromMaybe [] (parseMaybe parser object.attributes)
    where
        parser = Aeson.withObject "attributes" \o ->
            forM (KeyMap.toList o) \(key, value) -> case value of
                Aeson.String text -> pure (Key.toText key, text)
                other -> pure (Key.toText key, cs (Aeson.encode other))

configuredAttrNames :: AssetsConfig -> [Text]
configuredAttrNames config =
    [name | name <- map Text.strip (Text.splitOn "," config.attributeNames), not (Text.null name)]
