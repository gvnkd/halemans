module Application.Helper.DashboardConfig
( FacetRef (..)
, MatchOp (..)
, MatchClause (..)
, DashboardCard (..)
, decodeDashboardConfig
, encodeDashboardConfig
, renderDashboardConfig
, parseFacetRef
, facetRefText
, clauseValue
, matchClauseAlert
, matchCardAlert
, globToLike
, quoteSqlText
) where

import IHP.Prelude
import Generated.Types (Alert)
import Application.Pipeline.Grouping (AlertField (..), alertFieldName, alertFieldText, parseAlertField, labelValue, globMatch)
import Application.Service.Facets (facetValue)
import Data.Aeson (Value (..), object, (.=), (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Types (parseEither)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Control.Monad (guard)

-- Dashboard card JSON schema v2 (design_docs/milestone_9.md §4). JSON is the
-- only dashboard definition format; decoders are forward-tolerant (unknown
-- keys preserved on round-trip via cardExtras). Legacy M3 {env, filters}
-- cards decode into the same AST and re-encode to the legacy shape so old
-- rows round-trip unchanged.

data FacetRef = FacetField AlertField | FacetLabel Text | FacetAttr Text
    deriving (Eq, Show)

data MatchOp = OpEq | OpNe | OpGlob | OpIn
    deriving (Eq, Show)

data MatchClause = MatchClause
    { mcFacet :: FacetRef
    , mcOp :: MatchOp
    , mcValue :: Text
    , mcValues :: [Text]
    } deriving (Eq, Show)

data DashboardCard = DashboardCard
    { cardTitle :: Maybe Text
    , cardMatch :: [MatchClause]
    , cardGroupBy :: Maybe FacetRef
    , cardLimit :: Int
    , cardLegacy :: Bool
    , cardExtras :: KeyMap Value
    } deriving (Eq, Show)

parseFacetRef :: Text -> Maybe FacetRef
parseFacetRef raw
    | Just name <- Text.stripPrefix "field:" raw = FacetField <$> parseAlertField name
    | Just name <- Text.stripPrefix "label:" raw = nonEmpty FacetLabel name
    | Just name <- Text.stripPrefix "attr:" raw = nonEmpty FacetAttr name
    | otherwise = Nothing
    where
        nonEmpty f name
            | Text.null name = Nothing
            | otherwise = Just (f name)

facetRefText :: FacetRef -> Text
facetRefText = \case
    FacetField field -> "field:" <> alertFieldName field
    FacetLabel name -> "label:" <> name
    FacetAttr name -> "attr:" <> name

instance Aeson.FromJSON MatchClause where
    parseJSON = Aeson.withObject "MatchClause" \o -> do
        facetText <- o .: "facet"
        mcFacet <- maybe (fail ("invalid facet reference: " <> cs facetText)) pure (parseFacetRef facetText)
        opText <- o .:? "op" .!= ("=" :: Text)
        mcOp <- case opText of
            "=" -> pure OpEq
            "!=" -> pure OpNe
            "~" -> pure OpGlob
            "in" -> pure OpIn
            other -> fail ("unknown match op: " <> cs other)
        (mcValue, mcValues) <- case mcOp of
            OpIn -> do
                values <- o .: "values"
                when (null values) (fail "values must be a non-empty array for op \"in\"")
                pure ("", values)
            _ -> do
                value <- o .: "value"
                pure (value, [])
        pure MatchClause { .. }

instance Aeson.ToJSON MatchClause where
    toJSON clause = object $
        [ "facet" .= facetRefText clause.mcFacet
        , "op" .= opText clause.mcOp
        ] ++ case clause.mcOp of
            OpIn -> [ "values" .= clause.mcValues ]
            _ -> [ "value" .= clause.mcValue ]

opText :: MatchOp -> Text
opText = \case
    OpEq -> "="
    OpNe -> "!="
    OpGlob -> "~"
    OpIn -> "in"

instance Aeson.FromJSON DashboardCard where
    parseJSON = Aeson.withObject "DashboardCard" \o ->
        case KeyMap.lookup "env" o of
            Just (Aeson.String envValue) | not (KeyMap.member "match" o) -> do
                filtersValue <- o .:? "filters"
                let filters = case filtersValue of
                        Just (Aeson.Object f) -> f
                        _ -> KeyMap.empty
                statuses <- filters .:? "status" .!= []
                severities <- filters .:? "severity" .!= []
                let cardMatch = [MatchClause (FacetField FieldEnv) OpEq envValue []]
                        ++ [MatchClause (FacetField FieldStatus) OpIn "" statuses | not (null statuses)]
                        ++ [MatchClause (FacetField FieldSeverity) OpIn "" severities | not (null severities)]
                    cardExtras = KeyMap.delete "filters" (KeyMap.delete "env" o)
                pure DashboardCard
                    { cardTitle = Nothing
                    , cardMatch
                    , cardGroupBy = Nothing
                    , cardLimit = 100
                    , cardLegacy = True
                    , cardExtras
                    }
            _ -> do
                cardTitle <- o .:? "title"
                cardMatch <- o .:? "match" .!= []
                groupByText <- o .:? "groupBy"
                cardGroupBy <- case groupByText of
                    Nothing -> pure Nothing
                    Just text -> maybe (fail ("invalid groupBy facet reference: " <> cs text)) (pure . Just) (parseFacetRef text)
                cardLimit <- o .:? "limit" .!= 100
                when (cardLimit < 1) (fail "limit must be >= 1")
                let cardExtras = foldr KeyMap.delete o ["title", "match", "groupBy", "limit"]
                pure DashboardCard { cardLegacy = False, .. }

instance Aeson.ToJSON DashboardCard where
    toJSON card = case legacyCardValue card of
        Just value -> value
        Nothing -> Aeson.Object (KeyMap.union known card.cardExtras)
            where
                known = KeyMap.fromList $
                    [ "match" .= card.cardMatch
                    , "limit" .= card.cardLimit
                    ] ++ [ "title" .= title | Just title <- [card.cardTitle] ]
                    ++ [ "groupBy" .= facetRefText groupBy | Just groupBy <- [card.cardGroupBy] ]

-- | Legacy {env, filters} re-encode (milestone_9.md §4): only cards decoded
-- from the legacy shape whose clauses still fit it encode this way, so old
-- rows round-trip unchanged.
legacyCardValue :: DashboardCard -> Maybe Value
legacyCardValue card = do
    guard card.cardLegacy
    guard (isNothing card.cardTitle && isNothing card.cardGroupBy && card.cardLimit == 100)
    let envValues = [value | MatchClause (FacetField FieldEnv) OpEq value _ <- card.cardMatch]
        statusLists = [values | MatchClause (FacetField FieldStatus) OpIn _ values <- card.cardMatch]
        severityLists = [values | MatchClause (FacetField FieldSeverity) OpIn _ values <- card.cardMatch]
    case (envValues, statusLists, severityLists) of
        ([envValue], statuses, severities)
            | length card.cardMatch == 1 + length statuses + length severities
            , length statuses <= 1
            , length severities <= 1 ->
                Just (Aeson.Object (KeyMap.union known card.cardExtras))
                where
                    known = KeyMap.fromList
                        [ "env" .= envValue
                        , "filters" .= object
                            [ "status" .= fromMaybe [] (head statuses)
                            , "severity" .= fromMaybe [] (head severities)
                            ]
                        ]
        _ -> Nothing

decodeDashboardConfig :: Value -> Either Text [DashboardCard]
decodeDashboardConfig value = case value of
    Aeson.Array items -> forM (zip [0 ..] (Vector.toList items)) \(index, item) ->
        case parseEither Aeson.parseJSON item of
            Left err -> Left ("card " <> tshow (index :: Int) <> ": " <> cs err)
            Right card -> Right card
    _ -> Left "dashboard config must be a JSON array of cards"

encodeDashboardConfig :: [DashboardCard] -> Value
encodeDashboardConfig = Aeson.toJSON

-- | Pretty-printed JSON for the edit form textarea.
renderDashboardConfig :: [DashboardCard] -> Text
renderDashboardConfig = cs . Aeson.encode

-- | Value of a facet reference on an alert row: attr: reads the materialized
-- facets map (resolved override chain), field: the raw column, label:
-- alerts.labels.
clauseValue :: FacetRef -> Alert -> Maybe Text
clauseValue (FacetField field) alert = alertFieldText field alert
clauseValue (FacetLabel name) alert = labelValue alert name
clauseValue (FacetAttr name) alert = facetValue alert name

matchClauseAlert :: MatchClause -> Alert -> Bool
matchClauseAlert clause alert = case (clause.mcOp, clauseValue clause.mcFacet alert) of
    (OpEq, value) -> value == Just clause.mcValue
    (OpNe, Just value) -> value /= clause.mcValue
    (OpNe, Nothing) -> False
    (OpGlob, Just value) -> globMatch clause.mcValue value
    (OpGlob, Nothing) -> False
    (OpIn, Just value) -> value `elem` clause.mcValues
    (OpIn, Nothing) -> False

-- | Conjunction only, consistent with grouping rules (milestone_2.md §13).
matchCardAlert :: DashboardCard -> Alert -> Bool
matchCardAlert card alert = all (`matchClauseAlert` alert) card.cardMatch

-- | globMatch patterns translate to LIKE exactly: `*` ↔ `%`, `?` ↔ `_`,
-- everything else literal with the LIKE specials escaped (backslash is the
-- default LIKE escape in Postgres).
globToLike :: Text -> Text
globToLike = Text.concatMap translate
    where
        translate '*' = "%"
        translate '?' = "_"
        translate c
            | c `elem` ("%_\\" :: String) = Text.pack ['\\', c]
            | otherwise = Text.singleton c

-- | SQL string literal quoting (standard_conforming_strings=on: only single
-- quotes need doubling). Dashboard configs are user-authored JSON, so facet
-- names/values never interpolate unquoted.
quoteSqlText :: Text -> Text
quoteSqlText value = "'" <> Text.replace "'" "''" value <> "'"
