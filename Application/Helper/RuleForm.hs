module Application.Helper.RuleForm (
    parseMatchForm,
    parseMatchFormFacets,
    matchFieldsText,
    matchLabelsText,
    matchFacetsText,
) where

import Application.Pipeline.Grouping (MatchExpr (..), alertFieldName, matchExprFromJSON, parseAlertField)
import Control.Monad (guard)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import IHP.Prelude

-- Match-editor form helpers (milestone_2.md §9): the admin UI edits match
-- expressions as three comma-separated inputs — field clauses ("env=dev,
-- severity=critical, status in (firing, closed), severity not in (info)"),
-- label globs ("component=db-*") and facet globs ("DB Cluster=ib-*").
-- Conjunctions only per §13. Commas inside `in (...)` parens do not split
-- chunks, so list values may contain commas.

-- | One parsed field clause from the fields input.
data FieldClause = FieldEq Text | FieldIn [Text] | FieldNotIn [Text]

-- | Build the rule `match` jsonb from the three form fields. Unknown field
-- names are dropped (the view labels the accepted set).
parseMatchForm :: Text -> Text -> Value
parseMatchForm fieldsInput labelsInput = parseMatchFormFacets fieldsInput labelsInput ""

-- | Like parseMatchForm plus a third comma-separated facet-glob input
-- ("DB Cluster=ib-*"). Used by the grouping-rule form; notification rules
-- use the same three-input variant.
parseMatchFormFacets :: Text -> Text -> Text -> Value
parseMatchFormFacets fieldsInput labelsInput facetsInput =
    object
        [ "fields" .= object [(Key.fromText name, fieldValue clause) | (name, clause) <- fieldClauses]
        , "labels" .= object [(Key.fromText name, Aeson.toJSON value) | (name, value) <- pairs labelsInput]
        , "facets" .= object [(Key.fromText name, Aeson.toJSON value) | (name, value) <- pairs facetsInput]
        ]
  where
    fieldClauses =
        [ (name, clause)
        | chunk <- splitTopLevel fieldsInput
        , Just (name, clause) <- [parseFieldChunk chunk]
        , isJust (parseAlertField name)
        ]
    fieldValue (FieldEq value) = Aeson.toJSON value
    fieldValue (FieldIn values) = object ["in" .= values]
    fieldValue (FieldNotIn values) = object ["not-in" .= values]

-- | One fields-input chunk: `name=value`, `name in (a, b)` or
-- `name not in (a, b)`. Chunks with neither `=` nor `in` are dropped.
parseFieldChunk :: Text -> Maybe (Text, FieldClause)
parseFieldChunk chunk =
    let stripped = Text.strip chunk
     in case Text.breakOn "=" stripped of
            (name, eqRest)
                | not (Text.null eqRest) ->
                    Just (Text.strip name, FieldEq (Text.strip (Text.drop 1 eqRest)))
            _ -> parseListChunk stripped
  where
    parseListChunk stripped = do
        let (nameRaw, rest) = Text.breakOn " in " stripped
        guard (not (Text.null rest))
        let nameStripped = Text.strip nameRaw
        -- `status not in (...)` leaves " not" on the name side; a field
        -- name merely ending in "not" (e.g. "knot") must not count, so
        -- require a separating space.
        (name, isNot) <- case Text.stripSuffix " not" nameStripped of
            Just base | not (Text.null (Text.strip base)) -> Just (Text.strip base, True)
            _ -> Just (nameStripped, False)
        let valuesText = Text.dropAround (\c -> c == '(' || c == ')') (Text.strip (Text.drop 4 rest))
            values = [Text.strip value | value <- Text.splitOn "," valuesText, not (Text.null (Text.strip value))]
        pure (name, if isNot then FieldNotIn values else FieldIn values)

-- | Split on commas at paren depth 0 so `in (a, b)` stays a single chunk.
splitTopLevel :: Text -> [Text]
splitTopLevel input = go input "" 0
  where
    go remaining acc depth = case Text.uncons remaining of
        Nothing -> [acc | not (Text.null (Text.strip acc))]
        Just (',', rest)
            | depth == (0 :: Int) -> acc : go rest "" 0
            | otherwise -> go rest (acc <> ",") depth
        Just ('(', rest) -> go rest (acc <> "(") (depth + 1)
        Just (')', rest) -> go rest (acc <> ")") (max 0 (depth - 1))
        Just (c, rest) -> go rest (acc <> Text.singleton c) depth

-- | Comma-separated `name=value` pairs (labels/facets inputs).
pairs :: Text -> [(Text, Text)]
pairs input =
    [ (name, value)
    | chunk <- Text.splitOn "," input
    , let (name, rawValue) = Text.breakOn "=" (Text.strip chunk)
    , not (Text.null name)
    , not (Text.null rawValue)
    , let value = Text.strip (Text.drop 1 rawValue)
    ]

matchFieldsText :: Value -> Text
matchFieldsText matchJson =
    let expr = matchExprFromJSON matchJson
        parts =
            [alertFieldName field <> "=" <> value | (field, value) <- expr.meFieldEquals]
                <> [alertFieldName field <> " in (" <> Text.intercalate ", " values <> ")" | (field, values) <- expr.meFieldIn]
                <> [alertFieldName field <> " not in (" <> Text.intercalate ", " values <> ")" | (field, values) <- expr.meFieldNotIn]
     in Text.intercalate ", " parts

matchLabelsText :: Value -> Text
matchLabelsText matchJson =
    let expr = matchExprFromJSON matchJson
     in Text.intercalate ", " [name <> "=" <> glob | (name, glob) <- expr.meLabelGlobs]

matchFacetsText :: Value -> Text
matchFacetsText matchJson =
    let expr = matchExprFromJSON matchJson
     in Text.intercalate ", " [name <> "=" <> glob | (name, glob) <- expr.meFacetGlobs]
