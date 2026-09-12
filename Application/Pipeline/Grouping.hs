module Application.Pipeline.Grouping (
    AlertField (..),
    MatchExpr (..),
    emptyMatch,
    matchExprFromJSON,
    matchAlert,
    alertFieldText,
    alertFieldName,
    effectiveFieldText,
    effectiveFieldSql,
    fieldOverridable,
    parseAlertField,
    labelValue,
    facetValue,
    ruleReferencesFacets,
    renderTemplate,
    groupKeyForRule,
    severityAtLeast,
    severityRank,
    globMatch,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import Generated.Types
import IHP.Prelude

-- Pure grouping-rule core (design_docs/milestone_2.md §4). Rules are rows;
-- this module never touches the DB so it stays unit-testable.

data AlertField = FieldEnv | FieldHost | FieldService | FieldCheck | FieldSeverity | FieldStatus
    deriving (Eq, Show)

data MatchExpr = MatchExpr
    { meFieldEquals :: [(AlertField, Text)]
    , meLabelGlobs :: [(Text, Text)]
    , meFacetGlobs :: [(Text, Text)]
    }
    deriving (Eq, Show)

emptyMatch :: MatchExpr
emptyMatch = MatchExpr [] [] []

-- | Rule `match` jsonb shape: {"fields": {"env": "dev", ...},
-- "labels": {"component": "db-*", ...}, "facets": {"DB Cluster": "ib*",
-- ...}}. Unknown fields/keys are ignored so old rows keep parsing after new
-- match kinds land. Facet globs read the materialized alerts.facets map
-- (milestone_9.md §7).
matchExprFromJSON :: Value -> MatchExpr
matchExprFromJSON value =
    MatchExpr
        { meFieldEquals = fields
        , meLabelGlobs = labels
        , meFacetGlobs = facets
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
    facets = case lookupKey "facets" value of
        Just (Object o) -> foldr collectLabel [] (KeyMap.toList o)
        _ -> []

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

-- | A materialized facet named exactly like an overridable field becomes the
-- effective value of that field everywhere (display, filters, dashboards,
-- grouping). Only env/host/service are overridable: check/severity/status
-- stay raw (status is lifecycle-managed; severity/check drive rollups and
-- source correlation).
fieldOverridable :: AlertField -> Bool
fieldOverridable = \case
    FieldEnv -> True
    FieldHost -> True
    FieldService -> True
    _ -> False

-- | Effective field value: facet override wins, raw column is the fallback.
effectiveFieldText :: AlertField -> Alert -> Maybe Text
effectiveFieldText field alert
    | fieldOverridable field = facetValue alert (alertFieldName field) <|> alertFieldText field alert
    | otherwise = alertFieldText field alert

-- | SQL expression for the effective value of an overridable field, e.g.
-- @coalesce(nullif(alerts.facets ->> 'env', ''), alerts.env)@. Facet values
-- are non-empty by construction (resolveFacets drops empties); nullif guards
-- against hand-written rows. Nothing for non-overridable fields.
effectiveFieldSql :: Text -> AlertField -> Maybe Text
effectiveFieldSql alias field
    | fieldOverridable field = Just ("coalesce(nullif(" <> alias <> ".facets ->> '" <> alertFieldName field <> "', ''), " <> alias <> "." <> column <> ")")
    | otherwise = Nothing
  where
    column = case field of
        FieldEnv -> "env"
        FieldHost -> "host"
        FieldService -> "service"
        FieldCheck -> "check_name"
        FieldSeverity -> "severity"
        FieldStatus -> "status"

-- | Conjunction only (§13 decision): every field-equals, every label glob
-- and every facet glob must hold. An empty MatchExpr matches everything.
matchAlert :: MatchExpr -> Alert -> Bool
matchAlert expr alert = fieldsHold && labelsHold && facetsHold
  where
    fieldsHold = all (\(field, expected) -> effectiveFieldText field alert == Just expected) expr.meFieldEquals
    labelsHold = all (labelHolds alert) expr.meLabelGlobs
    facetsHold = all (\(name, glob) -> maybe False (globMatch glob) (facetValue alert name)) expr.meFacetGlobs

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

-- | Read one resolved facet from the materialized alerts.facets map
-- (milestone_9.md §3).
facetValue :: Alert -> Text -> Maybe Text
facetValue alert name = case alert.facets of
    Object o -> case KeyMap.lookup (Key.fromText name) o of
        Just (String value) -> Just value
        _ -> Nothing
    _ -> Nothing

-- | Rules referencing facets (facet globs in match or {facet:name} in the
-- group-key template) are re-evaluated after enrichment materializes attr
-- facets (milestone_9.md §7).
ruleReferencesFacets :: GroupingRule -> Bool
ruleReferencesFacets rule =
    not (null (meFacetGlobs (matchExprFromJSON rule.match)))
        || Text.isInfixOf "{facet:" rule.groupKeyTemplate

-- | Shell-style glob: `*` any run, `?` single char, everything else literal.
globMatch :: Text -> Text -> Bool
globMatch pat value = go (Text.unpack pat) (Text.unpack value)
  where
    go [] [] = True
    go ('*' : rest) chars = any (go rest) (dropNTails chars)
    go ('?' : rest) (_ : chars) = go rest chars
    go (p : rest) (c : chars) = p == c && go rest chars
    go _ _ = False
    dropNTails chars = [drop n chars | n <- [0 .. length chars]]

-- | Group-key template (§4): placeholders {env} {host} {service} {check}
-- {severity}, {label:name} and {facet:name} (milestone_9.md §7); a missing
-- subject renders as "-".
renderTemplate :: Text -> Alert -> Text
renderTemplate template alert = mconcat (map renderSegment (parseTemplate template))
  where
    renderSegment = \case
        Literal text -> text
        Placeholder name -> fromMaybe "-" (placeholderValue name alert)
    placeholderValue name alert
        | Just field <- parseAlertField name = effectiveFieldText field alert
        | Just labelName <- Text.stripPrefix "label:" name = labelValue alert labelName
        | Just facetName <- Text.stripPrefix "facet:" name = facetValue alert facetName
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
