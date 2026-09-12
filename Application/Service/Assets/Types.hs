module Application.Service.Assets.Types (
    ObjectSchema (..),
    ObjectType (..),
    ObjectTypeAttribute (..),
    AssetObject (..),
    ObjectAttribute (..),
    ObjectAttributeValue (..),
    ObjectHistory (..),
    ObjectListResult (..),
    Ticket (..),
    StatusType (..),
    Icon (..),
    Avatar (..),
    envelopeParser,
    flattenAttributes,
    hasMorePages,
) where

import Data.Aeson (Value, (.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import qualified Data.Text as Text
import IHP.Prelude

-- Jira Assets (Insight) read model (design_docs/assets-api.md). IDs are JSON
-- numbers on the wire (§8.2) — Int64 everywhere. Response envelopes are
-- mixed (§8.1): bare arrays and wrapped objects both occur, envelopeParser
-- tolerates either.

data ObjectSchema = ObjectSchema
    { schemaId :: Int64
    , schemaName :: Text
    , schemaKey :: Text
    , schemaObjectCount :: Maybe Int
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ObjectSchema where
    parseJSON = Aeson.withObject "ObjectSchema" \o -> do
        schemaId <- o .: "id"
        schemaName <- o .: "name"
        schemaKey <- o .:? "objectSchemaKey" .!= ""
        schemaObjectCount <- o .:? "objectCount"
        pure ObjectSchema{..}

data ObjectType = ObjectType
    { typeId :: Int64
    , typeName :: Text
    , typeSchemaId :: Maybe Int64
    , typeParentId :: Maybe Int64
    , typeIconUrl :: Maybe Text
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ObjectType where
    parseJSON = Aeson.withObject "ObjectType" \o -> do
        typeId <- o .: "id"
        typeName <- o .: "name"
        typeSchemaId <- o .:? "objectSchemaId"
        typeParentId <- o .:? "parentObjectTypeId"
        icon <- o .:? "icon"
        typeIconUrl <- case icon of
            Nothing -> pure Nothing
            Just i -> i .:? "url16"
        pure ObjectType{..}

data ObjectTypeAttribute = ObjectTypeAttribute
    { typeAttrId :: Int64
    , typeAttrName :: Text
    , typeAttrType :: Maybe Int
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ObjectTypeAttribute where
    parseJSON = Aeson.withObject "ObjectTypeAttribute" \o -> do
        typeAttrId <- o .: "id"
        typeAttrName <- o .: "name"
        typeAttrType <- o .:? "type"
        pure ObjectTypeAttribute{..}

data Avatar = Avatar
    { avatarUrl16 :: Text
    , avatarUrl48 :: Text
    }
    deriving (Eq, Show)

instance Aeson.FromJSON Avatar where
    parseJSON = Aeson.withObject "Avatar" \o -> do
        avatarUrl16 <- o .:? "url16" .!= ""
        avatarUrl48 <- o .:? "url48" .!= ""
        pure Avatar{..}

data AssetObject = AssetObject
    { objectId :: Int64
    , objectLabel :: Text
    , objectKey :: Text
    , objectAvatar :: Maybe Avatar
    , objectTypeName :: Text
    , objectAttributes :: [ObjectAttribute]
    }
    deriving (Eq, Show)

instance Aeson.FromJSON AssetObject where
    parseJSON = Aeson.withObject "AssetObject" \o -> do
        objectId <- o .: "id"
        objectLabel <- o .:? "label" .!= ""
        objectKey <- o .:? "objectKey" .!= ""
        objectAvatar <- o .:? "avatar"
        objectType <- o .:? "objectType"
        objectTypeName <- case objectType of
            Nothing -> pure ""
            Just t -> t .:? "name" .!= ""
        objectAttributes <- o .:? "attributes" .!= []
        pure AssetObject{..}

data ObjectAttribute = ObjectAttribute
    { attrId :: Int64
    , attrName :: Text
    , attrValues :: [ObjectAttributeValue]
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ObjectAttribute where
    parseJSON = Aeson.withObject "ObjectAttribute" \o -> do
        attrId <- o .:? "id" .!= 0
        typeAttr <- o .:? "objectTypeAttribute"
        attrName <- case typeAttr of
            Nothing -> o .:? "name" .!= ""
            Just t -> t .:? "name" .!= ""
        attrValues <- o .:? "objectAttributeValues" .!= []
        pure ObjectAttribute{..}

-- One of the sub-fields is populated depending on the attribute kind
-- (assets-api.md §4.3): raw value, formatted displayValue, referenced object
-- label, user/group display name, or status name.
data ObjectAttributeValue = ObjectAttributeValue
    { valueDisplay :: Text
    , valueStatusName :: Maybe Text
    , valueStatusId :: Maybe Int64
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ObjectAttributeValue where
    parseJSON = Aeson.withObject "ObjectAttributeValue" \o -> do
        display <- o .:? "displayValue"
        raw <- o .:? "value"
        referenced <- o .:? "referencedObject"
        referencedLabel <- case referenced of
            Nothing -> pure Nothing
            Just r -> r .:? "label"
        user <- o .:? "user"
        userName <- case user of
            Nothing -> pure Nothing
            Just u -> u .:? "displayName"
        grp <- o .:? "group"
        groupName <- case grp of
            Nothing -> pure Nothing
            Just g -> g .:? "name"
        status <- o .:? "status"
        valueStatusName <- case status of
            Nothing -> pure Nothing
            Just s -> s .:? "name"
        valueStatusId <- case status of
            Nothing -> pure Nothing
            Just s -> s .:? "id"
        let asText :: Maybe Value -> Maybe Text
            asText = \case
                Just (Aeson.String t) -> Just t
                Just (Aeson.Number n) -> Just (tshow n)
                _ -> Nothing
            valueDisplay =
                fromMaybe
                    ""
                    (asText display <|> asText raw <|> referencedLabel <|> userName <|> groupName <|> valueStatusName)
        pure ObjectAttributeValue{..}

-- Flattened name → display value map for the cache row (joined with ", "
-- when an attribute has several values). Status-only attributes keep their
-- status name.
flattenAttributes :: [ObjectAttribute] -> [(Text, Text)]
flattenAttributes attrs =
    [ (attr.attrName, Text.intercalate ", " (map (.valueDisplay) (nonEmpty attr.attrValues)))
    | attr <- attrs
    , not (Text.null attr.attrName)
    ]
  where
    nonEmpty = filter (not . Text.null . (.valueDisplay))

data ObjectHistory = ObjectHistory
    { historyId :: Int64
    , historyActor :: Text
    , historyCreated :: Text
    , historyType :: Text
    , historyAffectedAttribute :: Maybe Text
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ObjectHistory where
    parseJSON = Aeson.withObject "ObjectHistory" \o -> do
        historyId <- o .:? "id" .!= 0
        actor <- o .:? "actor"
        historyActor <- case actor of
            Nothing -> pure ""
            Just a -> a .:? "displayName" .!= ""
        historyCreated <- o .:? "created" .!= ""
        historyType <- o .:? "type" .!= ""
        historyAffectedAttribute <- o .:? "affectedAttribute"
        pure ObjectHistory{..}

data ObjectListResult = ObjectListResult
    { listEntries :: [AssetObject]
    , listTotalFilterCount :: Int
    , listStartIndex :: Int
    , listToIndex :: Int
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ObjectListResult where
    parseJSON = Aeson.withObject "ObjectListResult" \o -> do
        listEntries <- o .:? "objectEntries" .!= []
        listTotalFilterCount <- o .:? "totalFilterCount" .!= 0
        listStartIndex <- o .:? "startIndex" .!= 0
        listToIndex <- o .:? "toIndex" .!= 0
        pure ObjectListResult{..}

-- GET pagination model (assets-api.md §4.4): iterate while toIndex <
-- totalFilterCount.
hasMorePages :: ObjectListResult -> Bool
hasMorePages result = result.listToIndex < result.listTotalFilterCount

data Ticket = Ticket
    { ticketKey :: Text
    , ticketSummary :: Text
    , ticketStatus :: Text
    }
    deriving (Eq, Show)

instance Aeson.FromJSON Ticket where
    parseJSON = Aeson.withObject "Ticket" \o -> do
        ticketKey <- o .:? "key" .!= ""
        ticketSummary <- o .:? "summary" .!= ""
        status <- o .:? "status"
        ticketStatus <- case status of
            Nothing -> pure ""
            Just s -> s .:? "name" .!= ""
        pure Ticket{..}

data StatusType = StatusType
    { statusTypeId :: Int64
    , statusTypeName :: Text
    , statusTypeCategory :: Int
    }
    deriving (Eq, Show)

instance Aeson.FromJSON StatusType where
    parseJSON = Aeson.withObject "StatusType" \o -> do
        statusTypeId <- o .: "id"
        statusTypeName <- o .: "name"
        statusTypeCategory <- o .:? "category" .!= 1
        pure StatusType{..}

data Icon = Icon
    { iconId :: Int64
    , iconName :: Text
    , iconUrl16 :: Text
    , iconUrl48 :: Text
    }
    deriving (Eq, Show)

instance Aeson.FromJSON Icon where
    parseJSON = Aeson.withObject "Icon" \o -> do
        iconId <- o .: "id"
        iconName <- o .: "name"
        iconUrl16 <- o .:? "url16" .!= ""
        iconUrl48 <- o .:? "url48" .!= ""
        pure Icon{..}

-- Tolerant collection decoder (assets-api.md §8.1): accepts a bare array or
-- a wrapper object whose first matching key (in order) holds the array
-- (objectschemas / objectEntries / values / objects / icons / tickets / ...).
envelopeParser :: (Aeson.FromJSON a) => [Text] -> Value -> Parser [a]
envelopeParser keys value = case value of
    Aeson.Array _ -> Aeson.parseJSON value
    Aeson.Object o -> case mapMaybe (\k -> KeyMap.lookup (Key.fromText k) o) keys of
        (inner : _) -> Aeson.parseJSON inner
        [] -> fail ("no envelope key found (tried: " <> cs (Text.intercalate ", " keys) <> ")")
    _ -> fail "expected array or wrapper object"
