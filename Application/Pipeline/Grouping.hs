module Application.Pipeline.Grouping
( AlertField (..)
, MatchExpr (..)
, emptyMatch
, matchExprFromJSON
, matchAlert
, alertFieldText
, alertFieldName
, parseAlertField
, renderTemplate
, groupKeyForRule
, severityAtLeast
, severityRank
, globMatch
) where

import IHP.Prelude
import Generated.Types
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text

-- Pure grouping-rule core (design_docs/milestone_2.md §4). Rules are rows;
-- this module never touches the DB so it stays unit-testable.

data AlertField = FieldEnv | FieldHost | FieldService | FieldCheck | FieldSeverity | FieldStatus
    deriving (Eq, Show)

data MatchExpr = MatchExpr
    { meFieldEquals :: [(AlertField, Text)]
    , meLabelGlobs :: [(Text, Text)]
    }
    deriving (Eq, Show)

emptyMatch :: MatchExpr
emptyMatch = MatchExpr [] []

-- | Rule `match` jsonb shape: {"fields": {"env": "dev", ...},
-- "labels": {"component": "db-*", ...}}. Unknown fields/keys are ignored so
-- old rows keep parsing after new match kinds land.
matchExprFromJSON :: Value -> MatchExpr
matchExprFromJSON value = MatchExpr
    { meFieldEquals = fields
    , meLabelGlobs = labels
    }
    where
        fields = case lookupKey "fields" value of
            Just (Object o) -> foldr collectField [] (KeyMap.toList o)
            _ -> []
        collectField (key, String v) acc = case parseAlertField (Key.toText key) of
            Just field -> (field, v) : acc
            Nothing -> acc
        collectField _ acc = acc
        labels = case lookupKey "labels" value of
            Just (Object o) -> foldr collectLabel [] (KeyMap.toList o)
            _ -> []
        collectLabel (key, String v) acc = (Key.toText key, v) : acc
        collectLabel _ acc = acc

lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupKey _ _ = Nothing

alertFieldName :: AlertField -> Text
alertFieldName = \case
    FieldEnv -> "env"
    FieldHost -> "host"
    FieldService -> "service"
    FieldCheck -> "check"
    FieldSeverity -> "severity"
    FieldStatus -> "status"

parseAlertField :: Text -> Maybe AlertField
parseAlertField = \case
    "env" -> Just FieldEnv
    "host" -> Just FieldHost
    "service" -> Just FieldService
    "check" -> Just FieldCheck
    "severity" -> Just FieldSeverity
    "status" -> Just FieldStatus
    _ -> Nothing

alertFieldText :: AlertField -> Alert -> Maybe Text
alertFieldText = \case
    FieldEnv -> (.env)
    FieldHost -> (.host)
    FieldService -> (.service)
    FieldCheck -> (.checkName)
    FieldSeverity -> Just . (.severity)
    FieldStatus -> Just . (.status)

-- | Conjunction only (§13 decision): every field-equals and every label
-- glob must hold. An empty MatchExpr matches everything.
matchAlert :: MatchExpr -> Alert -> Bool
matchAlert expr alert = fieldsHold && labelsHold
    where
        fieldsHold = all (\(field, expected) -> alertFieldText field alert == Just expected) expr.meFieldEquals
        labelsHold = all (labelHolds alert) expr.meLabelGlobs

labelHolds :: Alert -> (Text, Text) -> Bool
labelHolds alert (name, glob) = case labelValue alert name of
    Just value -> globMatch glob value
    Nothing -> False

labelValue :: Alert -> Text -> Maybe Text
labelValue alert name = case alert.labels of
    Object o -> case KeyMap.lookup (Key.fromText name) o of
        Just (String value) -> Just value
        _ -> Nothing
    _ -> Nothing

-- | Shell-style glob: `*` any run, `?` single char, everything else literal.
globMatch :: Text -> Text -> Bool
globMatch pattern value = go (Text.unpack pattern) (Text.unpack value)
    where
        go [] [] = True
        go ('*':rest) chars = any (go rest) (dropNTails chars)
        go ('?':rest) (_:chars) = go rest chars
        go (p:rest) (c:chars) = p == c && go rest chars
        go _ _ = False
        dropNTails chars = [drop n chars | n <- [0 .. length chars]]

-- | Group-key template (§4): placeholders {env} {host} {service} {check}
-- {severity} and {label:name}; a missing subject renders as "-".
renderTemplate :: Text -> Alert -> Text
renderTemplate template alert = mconcat (map renderSegment (parseTemplate template))
    where
        renderSegment = \case
            Literal text -> text
            Placeholder name -> fromMaybe "-" (placeholderValue name alert)
        placeholderValue name alert
            | Just field <- parseAlertField name = alertFieldText field alert
            | Just labelName <- Text.stripPrefix "label:" name = labelValue alert labelName
            | otherwise = Nothing

data TemplateSegment = Literal Text | Placeholder Text

parseTemplate :: Text -> [TemplateSegment]
parseTemplate template
    | Text.null template = []
    | otherwise = case Text.breakOn "{" template of
        (before, rest)
            | Text.null rest -> [Literal before | not (Text.null before)]
            | otherwise ->
                let afterBrace = Text.drop 1 rest
                    (name, afterName) = Text.breakOn "}" afterBrace
                in case Text.uncons afterName of
                    Just ('}', remainder) ->
                        [Literal before | not (Text.null before)]
                            ++ [Placeholder name]
                            ++ parseTemplate remainder
                    _ -> [Literal (before <> rest)]

-- | What the pipeline stores on the alert/group for a matched rule: the
-- rendered group_key and the rule version at match time.
groupKeyForRule :: GroupingRule -> Alert -> Text
groupKeyForRule rule = renderTemplate rule.groupKeyTemplate

-- | Normalized severity ordering (§5): critical > high > warning > info.
severityRank :: Text -> Int
severityRank = \case
    "critical" -> 3
    "high" -> 2
    "warning" -> 1
    _ -> 0

severityAtLeast :: Text -> Text -> Bool
severityAtLeast threshold severity = severityRank severity >= severityRank threshold
