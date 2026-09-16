module Application.Helper.RuleForm (
    parseMatchForm,
    parseMatchFormFacets,
    matchFieldsText,
    matchLabelsText,
    matchFacetsText,
) where

import Application.Pipeline.Grouping (MatchExpr (..), alertFieldName, matchExprFromJSON, parseAlertField)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import IHP.Prelude

-- Match-editor form helpers (milestone_2.md §9): the admin UI edits match
-- expressions as two comma-separated inputs — field equals ("env=dev,
-- severity=critical") and label globs ("component=db-*"). Conjunctions only
-- per §13.

-- | Build the rule `match` jsonb from the two form fields. Unknown field
-- names are dropped (the view labels the accepted set).
parseMatchForm :: Text -> Text -> Value
parseMatchForm fieldsInput labelsInput = parseMatchFormFacets fieldsInput labelsInput ""

-- | Like parseMatchForm plus a third comma-separated facet-glob input
-- ("DB Cluster=ib-*"). Used by the grouping-rule form; notification rules
-- keep the two-input variant.
parseMatchFormFacets :: Text -> Text -> Text -> Value
parseMatchFormFacets fieldsInput labelsInput facetsInput =
    object
        [ "fields" .= object [(Key.fromText name, Aeson.toJSON value) | (name, value) <- fields]
        , "labels" .= object [(Key.fromText name, Aeson.toJSON value) | (name, value) <- labels]
        , "facets" .= object [(Key.fromText name, Aeson.toJSON value) | (name, value) <- pairs facetsInput]
        ]
  where
    fields = [(name, value) | (name, value) <- pairs fieldsInput, isJust (parseAlertField name)]
    labels = pairs labelsInput
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
     in Text.intercalate ", " [alertFieldName field <> "=" <> value | (field, value) <- expr.meFieldEquals]

matchLabelsText :: Value -> Text
matchLabelsText matchJson =
    let expr = matchExprFromJSON matchJson
     in Text.intercalate ", " [name <> "=" <> glob | (name, glob) <- expr.meLabelGlobs]

matchFacetsText :: Value -> Text
matchFacetsText matchJson =
    let expr = matchExprFromJSON matchJson
     in Text.intercalate ", " [name <> "=" <> glob | (name, glob) <- expr.meFacetGlobs]
