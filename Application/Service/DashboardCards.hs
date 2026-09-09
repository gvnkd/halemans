module Application.Service.DashboardCards
( CardGroup (..)
, ExpandedCard (..)
, runCardQuery
, runCardQueryGroups
, expandDashboardCards
, legacyCardDomKey
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.Fetch (fetch)
import IHP.QueryBuilder
import Generated.Types
import Application.Helper.DashboardConfig
import Application.Pipeline.Grouping (AlertField (..), severityRank)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.List (sortOn, nub, sort)
import Data.Ord (Down (..))

-- Card query engine (design_docs/milestone_9.md §5): single-table queries
-- over alerts; facet references compile to columns or jsonb accessors on the
-- materialized facets map, so dashboards never join asset_alert_links.

data CardGroup = CardGroup
    { cgValue :: Text
    , cgTotal :: Int
    , cgWorstSeverity :: Text
    , cgAlerts :: [Alert]
    } deriving (Eq, Show)

-- | Flat card: global limit, newest first.
runCardQuery :: (?modelContext :: ModelContext) => DashboardCard -> IO [Alert]
runCardQuery card =
    cardBaseQuery card
        |> orderByDesc #lastSeenAt
        |> limit (fromIntegral card.cardLimit)
        |> fetch

-- | Grouped card: group by the groupBy facet (absent lands in "-"), sections
-- ordered by worst severity then count desc, limit per group.
runCardQueryGroups :: (?modelContext :: ModelContext) => DashboardCard -> FacetRef -> IO [CardGroup]
runCardQueryGroups card groupBy = do
    alerts <- cardBaseQuery card
        |> orderByDesc #lastSeenAt
        |> fetch
    let byGroup = Map.fromListWith (\new old -> old ++ new)
            [(fromMaybe "-" (clauseValue groupBy alert), [alert]) | alert <- alerts]
        groups =
            [ CardGroup
                { cgValue = value
                , cgTotal = length groupAlerts
                , cgWorstSeverity = groupWorst groupAlerts
                , cgAlerts = take card.cardLimit groupAlerts
                }
            | (value, groupAlerts) <- Map.toList byGroup
            ]
    pure (sortOn (\group -> Down (severityRank group.cgWorstSeverity, group.cgTotal)) groups)

groupWorst :: [Alert] -> Text
groupWorst = foldl' (\worst alert -> if severityRank alert.severity > severityRank worst then alert.severity else worst) "info"

-- | A template card expanded to a concrete renderable card: forEach pins the
-- facet value into the match, the title gets {value} substituted, domId is
-- stable across renders, ecHidden reflects the hideWhen count at expansion
-- time.
data ExpandedCard = ExpandedCard
    { ecDomId :: Text
    , ecCard :: DashboardCard
    , ecHidden :: Bool
    } deriving (Eq, Show)

-- | DOM key convention: legacy cards keep the M3 dashboard-card-<env> id;
-- v2 cards key by config index, forEach expansions append the facet value.
legacyCardDomKey :: DashboardCard -> Maybe Text
legacyCardDomKey card
    | card.cardLegacy = head [value | MatchClause (FacetField FieldEnv) OpEq value _ <- card.cardMatch]
    | otherwise = Nothing

-- | Expand forEach templates into concrete cards (one per distinct facet
-- value among the template's matching non-closed alerts, sorted) and
-- evaluate hideWhen counts. Cards without forEach pass through unchanged.
expandDashboardCards :: (?modelContext :: ModelContext) => [DashboardCard] -> IO [ExpandedCard]
expandDashboardCards cards = concat <$> forM (zip [0 ..] cards) \(index, card) -> do
    let baseKey = fromMaybe (tshow (index :: Int)) (legacyCardDomKey card)
    expanded <- case card.cardForEach of
        Nothing -> pure [card]
        Just facetRef -> do
            alerts <- cardBaseQuery card |> fetch
            let values = sort (nub (mapMaybe (clauseValue facetRef) alerts))
            pure [pinCard card facetRef value | value <- values]
    forM expanded \expandedCard -> do
        hidden <- evaluateHideWhen expandedCard
        pure ExpandedCard
            { ecDomId = "dashboard-card-" <> baseKey <> forEachSuffix card expandedCard
            , ecCard = expandedCard
            , ecHidden = hidden
            }

pinCard :: DashboardCard -> FacetRef -> Text -> DashboardCard
pinCard card facetRef value = card
    { cardMatch = card.cardMatch ++ [MatchClause facetRef OpEq value []]
    , cardTitle = Just (Text.replace "{value}" value (fromMaybe value card.cardTitle))
    }

-- | Recover the pinned value from the clause pinCard appended, so the domId
-- is stable across expansion sites (HTTP render and WS broadcast).
forEachSuffix :: DashboardCard -> DashboardCard -> Text
forEachSuffix template expanded = case template.cardForEach of
    Just facetRef -> case [value | MatchClause f OpEq value _ <- drop (length template.cardMatch) expanded.cardMatch, f == facetRef] of
        (value : _) -> "-" <> Text.replace " " "_" value
        [] -> ""
    Nothing -> ""

evaluateHideWhen :: (?modelContext :: ModelContext) => DashboardCard -> IO Bool
evaluateHideWhen card = case card.cardHideWhen of
    Nothing -> pure False
    Just hw -> do
        let scoped = card { cardMatch = card.cardMatch ++ hw.hwMatch }
        matches <- cardBaseQuery scoped |> fetch
        pure (length matches <= hw.hwMaxCount)

cardBaseQuery :: DashboardCard -> QueryBuilder "alerts"
cardBaseQuery card = foldl' apply (query @Alert |> filterWhereNot (#status, "closed" :: Text)) card.cardMatch
    where
        apply builder clause = applyClause clause builder

applyClause :: MatchClause -> QueryBuilder "alerts" -> QueryBuilder "alerts"
applyClause clause builder = case clause.mcFacet of
    FacetField field -> case field of
        FieldEnv -> builder |> filterWhereSql (#env, fragment "" "alerts.env")
        FieldHost -> builder |> filterWhereSql (#host, fragment "" "alerts.host")
        FieldService -> builder |> filterWhereSql (#service, fragment "" "alerts.service")
        FieldCheck -> builder |> filterWhereSql (#checkName, fragment "" "alerts.check_name")
        FieldSeverity -> builder |> filterWhereSql (#severity, fragment "" "alerts.severity")
        FieldStatus -> builder |> filterWhereSql (#status, fragment "" "alerts.status")
    FacetLabel name -> builder |> filterWhereSql (#labels, fragment accessor ("alerts.labels " <> accessor))
        where accessor = "->> " <> quoteSqlText name
    FacetAttr name -> builder |> filterWhereSql (#facets, fragment accessor ("alerts.facets " <> accessor))
        where accessor = "->> " <> quoteSqlText name
    where
        -- filterWhereSql appends the fragment after the qualified proxy
        -- column; `accessor` extends the column to the value expression and
        -- `valueExpr` repeats it in full for the != null guard.
        fragment accessor valueExpr = case clause.mcOp of
            OpEq -> accessor <> " = " <> quoteSqlText clause.mcValue
            OpNe -> accessor <> " IS NOT NULL AND " <> valueExpr <> " <> " <> quoteSqlText clause.mcValue
            OpGlob -> accessor <> " LIKE " <> quoteSqlText (globToLike clause.mcValue)
            OpIn -> accessor <> " IN (" <> Text.intercalate ", " (map quoteSqlText clause.mcValues) <> ")"
