module Application.Helper.DashboardConfig (
    FacetRef (..),
    MatchOp (..),
    MatchClause (..),
    HideWhen (..),
    SortKey (..),
    SortTarget (..),
    AlertSortKey (..),
    validAlertSortColumns,
    parseAlertSortKey,
    alertSortKeyText,
    alertSortNaturalDir,
    alertSortDisplayDir,
    CardSize (..),
    defaultCardSize,
    DashboardCard (..),
    decodeDashboardConfig,
    encodeDashboardConfig,
    renderDashboardConfig,
    parseFacetRef,
    parseSortKey,
    sortKeyText,
    facetRefText,
    clauseValue,
    matchClauseAlert,
    matchCardAlert,
    globToLike,
    quoteSqlText,
) where

import Application.Pipeline.Grouping (AlertField (..), alertFieldName, effectiveFieldText, globMatch, labelValue, parseAlertField)
import Application.Service.Facets (facetValue)
import Control.Monad (guard)
import Data.Aeson (Value (..), object, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Generated.Types (Alert)
import IHP.Prelude

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
    }
    deriving (Eq, Show)

-- | Conditional visibility: hide the card when the number of (non-closed)
-- alerts matching hwMatch ON TOP of the card's own match is <= hwMaxCount.
data HideWhen = HideWhen
    { hwMatch :: [MatchClause]
    , hwMaxCount :: Int
    }
    deriving (Eq, Show)

-- | Summary-card dimensions in px. A card without "size" keeps the CSS
-- defaults (defaultCardSize); "size" overrides per card and may itself omit
-- either dimension.
data CardSize = CardSize
    { csWidth :: Int
    , csHeight :: Int
    }
    deriving (Eq, Show)

-- | Sane defaults, mirroring the CSS (.env-card height, summary flex-basis).
defaultCardSize :: CardSize
defaultCardSize = CardSize 360 168

instance Aeson.FromJSON CardSize where
    parseJSON = Aeson.withObject "CardSize" \o -> do
        csWidth <- o .:? "width" .!= csWidth defaultCardSize
        csHeight <- o .:? "height" .!= csHeight defaultCardSize
        when (csWidth < 80) (fail "size.width must be >= 80")
        when (csHeight < 60) (fail "size.height must be >= 60")
        pure CardSize{..}

instance Aeson.ToJSON CardSize where
    toJSON size = object ["width" .= size.csWidth, "height" .= size.csHeight]

data DashboardCard = DashboardCard
    { cardTitle :: Maybe Text
    , cardMatch :: [MatchClause]
    , cardGroupBy :: Maybe FacetRef
    , cardLimit :: Int
    , cardLegacy :: Bool
    , cardForEach :: Maybe FacetRef
    , cardHideWhen :: Maybe HideWhen
    , cardSummary :: Bool
    , cardSortBy :: [SortKey]
    , cardAlertSortBy :: [AlertSortKey]
    , cardSize :: Maybe CardSize
    , cardExtras :: KeyMap Value
    }
    deriving (Eq, Show)

-- | Ordering of the cards a template expands into. Builtins are "key:"
-- prefixed (severity = worst severity of the card's alerts, count = matching
-- alert count, title); anything else is a facet reference (the pinned value
-- for forEach cards). A leading "-" flips the direction; defaults:
-- severity/count descending (worst/most first), title/facet ascending.
data SortKey = SortKey
    { skDesc :: Bool
    , skTarget :: SortTarget
    }
    deriving (Eq, Show)

data SortTarget = SortBuiltin Text | SortFacet FacetRef
    deriving (Eq, Show)

parseSortKey :: Text -> Maybe SortKey
parseSortKey raw = do
    let (skDesc, body) = case Text.uncons raw of
            Just ('-', rest) -> (True, rest)
            _ -> (False, raw)
    skTarget <- case Text.stripPrefix "key:" body of
        Just name
            | name `elem` ["severity", "count", "title"] -> Just (SortBuiltin name)
            | otherwise -> Nothing
        Nothing -> SortFacet <$> parseFacetRef body
    pure SortKey{..}

sortKeyText :: SortKey -> Text
sortKeyText key = (if key.skDesc then "-" else "") <> targetText
  where
    targetText = case key.skTarget of
        SortBuiltin name -> "key:" <> name
        SortFacet ref -> facetRefText ref

instance Aeson.FromJSON SortKey where
    parseJSON = Aeson.withText "SortKey" \text ->
        maybe (fail ("invalid sortBy key: " <> cs text)) pure (parseSortKey text)

instance Aeson.ToJSON SortKey where
    toJSON = Aeson.toJSON . sortKeyText

-- | Ordering of the alerts inside one card (flat list + detail table).
-- Columns are the /alerts sortable columns; the natural direction is
-- worst/newest first (severity: critical first, last_seen_at: newest first,
-- everything else ascending), a leading "-" flips it.
data AlertSortKey = AlertSortKey
    { askDesc :: Bool
    , askColumn :: Text
    }
    deriving (Eq, Show)

-- | Canonical list of alert-sortable columns; Application.Service.AlertList
-- re-exports it as validSortColumns so the widget, the list query and the
-- card config never drift apart.
validAlertSortColumns :: [Text]
validAlertSortColumns = ["status", "severity", "title", "env", "host", "occurrences", "last_seen_at"]

parseAlertSortKey :: Text -> Maybe AlertSortKey
parseAlertSortKey raw = do
    let (askDesc, body) = case Text.uncons raw of
            Just ('-', rest) -> (True, rest)
            _ -> (False, raw)
    guard (body `elem` validAlertSortColumns)
    pure AlertSortKey{askColumn = body, ..}

alertSortKeyText :: AlertSortKey -> Text
alertSortKeyText key = (if key.askDesc then "-" else "") <> key.askColumn

-- | Direction a fresh header click sorts by (matches /alerts): newest-first
-- for last_seen_at, ascending otherwise.
alertSortNaturalDir :: Text -> Text
alertSortNaturalDir "last_seen_at" = "desc"
alertSortNaturalDir _ = "asc"

-- | asc/desc as shown by the widget indicator for a configured key.
alertSortDisplayDir :: AlertSortKey -> Text
alertSortDisplayDir key = case (key.askDesc, alertSortNaturalDir key.askColumn) of
    (False, dir) -> dir
    (True, "asc") -> "desc"
    (True, _) -> "asc"

instance Aeson.FromJSON AlertSortKey where
    parseJSON = Aeson.withText "AlertSortKey" \text ->
        maybe (fail ("invalid alertSortBy key: " <> cs text)) pure (parseAlertSortKey text)

instance Aeson.ToJSON AlertSortKey where
    toJSON = Aeson.toJSON . alertSortKeyText

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
        pure MatchClause{..}

instance Aeson.ToJSON MatchClause where
    toJSON clause =
        object $
            [ "facet" .= facetRefText clause.mcFacet
            , "op" .= opText clause.mcOp
            ]
                ++ case clause.mcOp of
                    OpIn -> ["values" .= clause.mcValues]
                    _ -> ["value" .= clause.mcValue]

opText :: MatchOp -> Text
opText = \case
    OpEq -> "="
    OpNe -> "!="
    OpGlob -> "~"
    OpIn -> "in"

instance Aeson.FromJSON HideWhen where
    parseJSON = Aeson.withObject "HideWhen" \o -> do
        hwMatch <- o .:? "match" .!= []
        hwMaxCount <- o .:? "maxCount" .!= 0
        when (hwMaxCount < 0) (fail "maxCount must be >= 0")
        pure HideWhen{..}

instance Aeson.ToJSON HideWhen where
    toJSON hw = object ["match" .= hw.hwMatch, "maxCount" .= hw.hwMaxCount]

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
                let cardMatch =
                        [MatchClause (FacetField FieldEnv) OpEq envValue []]
                            ++ [MatchClause (FacetField FieldStatus) OpIn "" statuses | not (null statuses)]
                            ++ [MatchClause (FacetField FieldSeverity) OpIn "" severities | not (null severities)]
                    cardExtras = KeyMap.delete "filters" (KeyMap.delete "env" o)
                pure
                    DashboardCard
                        { cardTitle = Nothing
                        , cardMatch
                        , cardGroupBy = Nothing
                        , cardLimit = 100
                        , cardLegacy = True
                        , cardForEach = Nothing
                        , cardHideWhen = Nothing
                        , cardSummary = False
                        , cardSortBy = []
                        , cardAlertSortBy = []
                        , cardSize = Nothing
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
                forEachText <- o .:? "forEach"
                cardForEach <- case forEachText of
                    Nothing -> pure Nothing
                    Just text -> maybe (fail ("invalid forEach facet reference: " <> cs text)) (pure . Just) (parseFacetRef text)
                cardHideWhen <- o .:? "hideWhen"
                cardSummary <- o .:? "summary" .!= False
                cardSortBy <- o .:? "sortBy" .!= []
                cardAlertSortBy <- o .:? "alertSortBy" .!= []
                cardSize <- o .:? "size"
                let cardExtras = foldr KeyMap.delete o ["title", "match", "groupBy", "limit", "forEach", "hideWhen", "summary", "sortBy", "alertSortBy", "size"]
                pure DashboardCard{cardLegacy = False, ..}

instance Aeson.ToJSON DashboardCard where
    toJSON card = case legacyCardValue card of
        Just value -> value
        Nothing -> Aeson.Object (KeyMap.union known card.cardExtras)
          where
            known =
                KeyMap.fromList $
                    [ "match" .= card.cardMatch
                    , "limit" .= card.cardLimit
                    ]
                        ++ ["title" .= title | Just title <- [card.cardTitle]]
                        ++ ["groupBy" .= facetRefText groupBy | Just groupBy <- [card.cardGroupBy]]
                        ++ ["forEach" .= facetRefText forEach | Just forEach <- [card.cardForEach]]
                        ++ ["hideWhen" .= hideWhen | Just hideWhen <- [card.cardHideWhen]]
                        ++ ["summary" .= True | card.cardSummary]
                        ++ ["sortBy" .= card.cardSortBy | not (null card.cardSortBy)]
                        ++ ["alertSortBy" .= card.cardAlertSortBy | not (null card.cardAlertSortBy)]
                        ++ ["size" .= size | Just size <- [card.cardSize]]

-- | Legacy {env, filters} re-encode (milestone_9.md §4): only cards decoded
-- from the legacy shape whose clauses still fit it encode this way, so old
-- rows round-trip unchanged.
legacyCardValue :: DashboardCard -> Maybe Value
legacyCardValue card = do
    guard card.cardLegacy
    guard (isNothing card.cardTitle && isNothing card.cardGroupBy && card.cardLimit == 100)
    guard (isNothing card.cardForEach && isNothing card.cardHideWhen && not card.cardSummary && null card.cardSortBy && null card.cardAlertSortBy && isNothing card.cardSize)
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
            known =
                KeyMap.fromList
                    [ "env" .= envValue
                    , "filters"
                        .= object
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
renderDashboardConfig = cs . Pretty.encodePretty

-- | Value of a facet reference on an alert row: attr: reads the materialized
-- facets map (resolved override chain), field: the EFFECTIVE field value
-- (facet named like an overridable field — env/host/service — wins over the
-- raw column), label: alerts.labels.
clauseValue :: FacetRef -> Alert -> Maybe Text
clauseValue (FacetField field) alert = effectiveFieldText field alert
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
