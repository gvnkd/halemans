module Application.Service.Assets.Aql
( Aql (..)
, escapeAql
, quoteAql
, schemaEq
, attrLike
, attrEq
, andAql
, typeAndChildren
, fillHostTemplate
) where

import IHP.Prelude
import qualified Data.Text as Text

-- Opaque AQL builder (design_docs/assets-api.md §5.4): user input only ever
-- enters queries through escapeAql/quoteAql; no raw string concatenation of
-- unescaped values into queries.

newtype Aql = Aql { aqlText :: Text } deriving (Eq, Show)

-- \ and " escaping inside double-quoted AQL string literals (§5.4).
escapeAql :: Text -> Text
escapeAql = Text.concatMap \case
    '\\' -> "\\\\"
    '"' -> "\\\""
    c -> Text.singleton c

quoteAql :: Text -> Text
quoteAql value = "\"" <> escapeAql value <> "\""

-- objectSchema = "<display name>" (the name, NOT the key — §5.3).
schemaEq :: Text -> Aql
schemaEq name = Aql ("objectSchema = " <> quoteAql name)

-- Attribute names may contain spaces — always double-quoted.
attrLike :: Text -> Text -> Aql
attrLike attr value = Aql (quoteAql attr <> " like " <> quoteAql value)

attrEq :: Text -> Text -> Aql
attrEq attr value = Aql (quoteAql attr <> " = " <> quoteAql value)

andAql :: [Aql] -> Aql
andAql parts = Aql (Text.intercalate " AND " (map (.aqlText) parts))

-- objectType in objectTypeAndChildren("<name>") — type plus descendants
-- (§5.3: plain objectType = misses child types).
typeAndChildren :: Text -> Aql
typeAndChildren name = Aql ("objectType in objectTypeAndChildren(" <> quoteAql name <> ")")

-- Config host_query_template carries a {host} placeholder filled here
-- through the escaper (milestone_8.md §2/§3).
fillHostTemplate :: Text -> Text -> Aql
fillHostTemplate template host = Aql (Text.replace "{host}" (escapeAql host) template)
