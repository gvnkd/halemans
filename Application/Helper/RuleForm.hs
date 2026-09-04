module Application.Helper.RuleForm
( parseMatchForm
, matchFieldsText
, matchLabelsText
) where

import IHP.Prelude
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import Application.Pipeline.Grouping (MatchExpr (..), matchExprFromJSON, parseAlertField, alertFieldName)

-- Match-editor form helpers (milestone_2.md §9): the admin UI edits match
-- expressions as two comma-separated inputs — field equals ("env=dev,
-- severity=critical") and label globs ("component=db-*"). Conjunctions only
-- per §13.

-- | Build the rule `match` jsonb from the two form fields. Unknown field
-- names are dropped (the view labels the accepted set).
parseMatchForm :: Text -> Text -> Value
parseMatchForm fieldsInput labelsInput = object
    [ "fields" .= object [ (Key.fromText name, Aeson.toJSON value) | (name, value) <- fields ]
    , "labels" .= object [ (Key.fromText name, Aeson.toJSON value) | (name, value) <- labels ]
    ]
    where
        fields = [ (name, value) | (name, value) <- pairs fieldsInput, isJust (parseAlertField name) ]
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
    in Text.intercalate ", " [ alertFieldName field <> "=" <> value | (field, value) <- expr.meFieldEquals ]

matchLabelsText :: Value -> Text
matchLabelsText matchJson =
    let expr = matchExprFromJSON matchJson
    in Text.intercalate ", " [ name <> "=" <> glob | (name, glob) <- expr.meLabelGlobs ]
