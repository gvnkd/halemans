module Application.Service.DashboardCards (
    CardGroup (..),
    CardSummary (..),
    ExpandedCard (..),
    runCardQuery,
    runCardQueryGroups,
    runCardSummary,
    sortCardAlerts,
    expandDashboardCards,
    legacyCardDomKey,
    expandedDomId,
    pinCard,
) where

import Application.Helper.DashboardConfig
import Application.Pipeline.Grouping (AlertField (..), effectiveFieldSql, effectiveFieldText, severityRank)
import Data.List (nub, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Generated.Types
import IHP.Fetch (fetch)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

-- Card query engine (design_docs/milestone_9.md §5): single-table queries
-- over alerts; facet references compile to columns or jsonb accessors on the
-- materialized facets map, so dashboards never join asset_alert_links.

data CardGroup = CardGroup
    { cgValue :: Text
    , cgTotal :: Int
    , cgWorstSeverity :: Text
    , cgAlerts :: [Alert]
    }
    deriving (Eq, Show)

-- | Flat card: global limit. alertSortBy orders the list (stable over the
-- newest-first base order, so ties stay newest-first); without it the SQL
-- limit applies directly on newest-first.
runCardQuery :: (?modelContext :: ModelContext) => DashboardCard -> IO [Alert]
runCardQuery card
    | null card.cardAlertSortBy =
        cardBaseQuery card
            |> orderByDesc #lastSeenAt
            |> limit (fromIntegral card.cardLimit)
            |> fetch
    | otherwise = do
        alerts <-
            cardBaseQuery card
                |> orderByDesc #lastSeenAt
                |> fetch
        pure (take card.cardLimit (sortCardAlerts card.cardAlertSortBy alerts))

-- | alertSortBy ordering of a card's alerts: stable sort over the
-- lastSeenAt-descending fetch order. Each column's natural direction
-- (severity worst-first, last_seen_at newest-first, others ascending) is
-- baked into the payload; askDesc flips it.
sortCardAlerts :: [AlertSortKey] -> [Alert] -> [Alert]
sortCardAlerts keys = sortOn (\alert -> map (alertSortPayload alert) keys)

alertSortPayload :: Alert -> AlertSortKey -> AlertSortPayload
alertSortPayload alert key = AlertSortPayload key.askDesc $ case key.askColumn of
    "status" -> APInt (statusRank alert.status)
    "severity" -> APDownInt (Down (severityRank alert.severity))
    "title" -> APText (Text.toLower alert.title)
    "env" -> APText (fromMaybe "" (effectiveFieldText FieldEnv alert))
    "host" -> APText (fromMaybe "" (effectiveFieldText FieldHost alert))
    "occurrences" -> APInt alert.occurrences
    _ -> APDownTime (Down alert.lastSeenAt)

statusRank :: Text -> Int
statusRank = \case
    "firing" -> 0
    "ack" -> 1
    "resolved" -> 2
    "stalled" -> 3
    _ -> 4

data AlertSortPayload = AlertSortPayload Bool AlertSortValue
data AlertSortValue = APInt Int | APDownInt (Down Int) | APText Text | APDownTime (Down UTCTime)

instance Eq AlertSortPayload where
    a == b = compare a b == EQ

instance Ord AlertSortPayload where
    compare (AlertSortPayload desc a) (AlertSortPayload _ b) = applyDir desc (compareAlertSortValue a b)
      where
        applyDir False o = o
        applyDir True EQ = EQ
        applyDir True LT = GT
        applyDir True GT = LT

compareAlertSortValue :: AlertSortValue -> AlertSortValue -> Ordering
compareAlertSortValue (APInt a) (APInt b) = compare a b
compareAlertSortValue (APDownInt a) (APDownInt b) = compare a b
compareAlertSortValue (APText a) (APText b) = compare a b
compareAlertSortValue (APDownTime a) (APDownTime b) = compare a b
compareAlertSortValue _ _ = EQ

-- | Grouped card: group by the groupBy facet (absent lands in "-"), sections
-- ordered by worst severity then count desc, limit per group.
runCardQueryGroups :: (?modelContext :: ModelContext) => DashboardCard -> FacetRef -> IO [CardGroup]
runCardQueryGroups card groupBy = do
    alerts <-
        cardBaseQuery card
            |> orderByDesc #lastSeenAt
            |> fetch
    let byGroup =
            Map.fromListWith
                (\new old -> old ++ new)
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

-- | Rollup for "summary": true cards — the same numbers the overview env
-- cards show (counts by status, suppressed, worst severity, hourly buckets
-- over the last 24h), scoped to the card's match.
data CardSummary = CardSummary
    { csFiring :: Int
    , csAcked :: Int
    , csResolved :: Int
    , csStalled :: Int
    , csSuppressed :: Int
    , csWorstSeverity :: Maybe Text
    , csHourly :: [(UTCTime, Int)]
    }
    deriving (Eq, Show)

runCardSummary :: (?modelContext :: ModelContext) => DashboardCard -> IO CardSummary
runCardSummary card = do
    alerts <- cardBaseQuery card |> fetch
    now <- getCurrentTime
    let countFor status = length (filter (\alert -> alert.status == status) alerts)
        cutoff = addUTCTime (-24 * 3600) now
        hourOf alert = posixSecondsToUTCTime (fromIntegral (seconds - seconds `mod` 3600))
          where
            seconds = floor (utcTimeToPOSIXSeconds alert.lastSeenAt) :: Int
        hourly =
            Map.toAscList
                ( Map.fromListWith
                    (+)
                    [(hourOf alert, 1) | alert <- alerts, alert.lastSeenAt > cutoff]
                )
    pure
        CardSummary
            { csFiring = countFor "firing"
            , csAcked = countFor "ack"
            , csResolved = countFor "resolved"
            , csStalled = countFor "stalled"
            , csSuppressed = length (filter (.suppressed) alerts)
            , csWorstSeverity = if null alerts then Nothing else Just (groupWorst alerts)
            , csHourly = hourly
            }

-- | A template card expanded to a concrete renderable card: forEach pins the
-- facet value into the match, the title gets {value} substituted, domId is
-- stable across renders, ecHidden reflects the hideWhen count at expansion
-- time.
data ExpandedCard = ExpandedCard
    { ecDomId :: Text
    , ecCard :: DashboardCard
    , ecHidden :: Bool
    , ecIndex :: Int
    , ecValue :: Maybe Text
    }
    deriving (Eq, Show)

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
expandDashboardCards cards =
    concat <$> forM (zip [0 ..] cards) \(index, card) -> do
        pinned <- case card.cardForEach of
            Nothing -> pure [(card, Nothing)]
            Just facetRef -> do
                alerts <- cardBaseQuery card |> fetch
                let values = sort (nub (mapMaybe (clauseValue facetRef) alerts))
                sortPinned card [(pinCard card facetRef value, Just value) | value <- values]
        forM pinned \(expandedCard, pinnedValue) -> do
            hidden <- evaluateHideWhen expandedCard
            pure
                ExpandedCard
                    { ecDomId = expandedDomId index card pinnedValue
                    , ecCard = expandedCard
                    , ecHidden = hidden
                    , ecIndex = index
                    , ecValue = pinnedValue
                    }

-- | Stable section dom id for a (possibly hypothetical) expanded card: config
-- index (or legacy env key) plus the pinned forEach value when present.
expandedDomId :: Int -> DashboardCard -> Maybe Text -> Text
expandedDomId index card pinnedValue =
    "dashboard-card-" <> fromMaybe (tshow index) (legacyCardDomKey card) <> valueSuffix
  where
    valueSuffix = maybe "" ("-" <>) (Text.replace " " "_" <$> pinnedValue)

pinCard :: DashboardCard -> FacetRef -> Text -> DashboardCard
pinCard card facetRef value =
    card
        { cardMatch = card.cardMatch ++ [MatchClause facetRef OpEq value []]
        , cardTitle = Just (Text.replace "{value}" value (fromMaybe value card.cardTitle))
        }

-- | sortBy ordering of one template's expanded cards: stable, per template,
-- so config order between templates is preserved. Metrics (worst severity,
-- count) are fetched per expanded card only when the keys need them.
data SortMetrics = SortMetrics
    { smWorst :: Maybe Text
    , smCount :: Int
    , smTitle :: Text
    }

sortPinned :: (?modelContext :: ModelContext) => DashboardCard -> [(DashboardCard, Maybe Text)] -> IO [(DashboardCard, Maybe Text)]
sortPinned card pinned
    | null card.cardSortBy = pure pinned
    | otherwise = do
        entries <- forM pinned \(pinnedCard, value) -> do
            metrics <- cardSortMetrics pinnedCard
            pure (metrics, pinnedCard, value)
        pure (map (\(_, pinnedCard, value) -> (pinnedCard, value)) (sortOn (sortEntryKey card.cardSortBy) entries))

cardSortMetrics :: (?modelContext :: ModelContext) => DashboardCard -> IO SortMetrics
cardSortMetrics card = do
    alerts <- cardBaseQuery card |> fetch
    pure
        SortMetrics
            { smWorst = if null alerts then Nothing else Just (groupWorst alerts)
            , smCount = length alerts
            , smTitle = fromMaybe "" card.cardTitle
            }

-- | Mapped into a sortable key: each SortKey contributes its comparison
-- payload in order; severityRank higher = more severe, so worst-first is
-- Just-rank descending with alert-less cards last.
sortEntryKey :: [SortKey] -> (SortMetrics, DashboardCard, Maybe Text) -> [SortKeyPayload]
sortEntryKey keys (metrics, card, value) = map payload keys
  where
    payload key = SortKeyPayload (key.skDesc) $ case key.skTarget of
        SortBuiltin "severity" -> PInt (Down (maybe (-1) severityRank metrics.smWorst))
        SortBuiltin "count" -> PInt (Down metrics.smCount)
        SortBuiltin "title" -> PText metrics.smTitle
        SortBuiltin _ -> PText ""
        SortFacet ref -> case (card.cardForEach, value) of
            (Just forEachRef, Just pinned) | ref == forEachRef -> PMaybeText (Just pinned)
            _ -> PMaybeText Nothing

data SortKeyPayload = SortKeyPayload Bool Payload
data Payload = PInt (Down Int) | PText Text | PMaybeText (Maybe Text)

instance Eq SortKeyPayload where
    a == b = compare a b == EQ

instance Ord SortKeyPayload where
    compare (SortKeyPayload desc a) (SortKeyPayload _ b) = applyDir desc (comparePayload a b)
      where
        applyDir False o = o
        applyDir True EQ = EQ
        applyDir True LT = GT
        applyDir True GT = LT

comparePayload :: Payload -> Payload -> Ordering
comparePayload (PInt a) (PInt b) = compare a b
comparePayload (PText a) (PText b) = compare a b
comparePayload (PMaybeText a) (PMaybeText b) = compare (Down (isJust a), a) (Down (isJust b), b)
comparePayload _ _ = EQ

evaluateHideWhen :: (?modelContext :: ModelContext) => DashboardCard -> IO Bool
evaluateHideWhen card = case card.cardHideWhen of
    Nothing -> pure False
    Just hw -> do
        let scoped = card{cardMatch = card.cardMatch ++ hw.hwMatch}
        matches <- cardBaseQuery scoped |> fetch
        pure (length matches <= hw.hwMaxCount)

cardBaseQuery :: DashboardCard -> QueryBuilder "alerts"
cardBaseQuery card = foldl' apply (query @Alert |> filterWhereNot (#status, "closed" :: Text)) card.cardMatch
  where
    apply builder clause = applyClause clause builder

applyClause :: MatchClause -> QueryBuilder "alerts" -> QueryBuilder "alerts"
applyClause clause builder = case clause.mcFacet of
    FacetField field -> case effectiveFieldSql "alerts" field of
        -- Overridable fields match on the effective value (facet override
        -- wins over the raw column). filterWhereSql only APPENDS the fragment
        -- after the qualified proxy column, so the condition is spliced
        -- behind `alerts.facets IS NOT NULL AND` — facets is NOT NULL by
        -- schema, making the prefix a no-op.
        Just expr -> builder |> filterWhereSql (#facets, effectiveCondition expr)
        Nothing -> case field of
            FieldCheck -> builder |> filterWhereSql (#checkName, fragment "" "alerts.check_name")
            FieldSeverity -> builder |> filterWhereSql (#severity, fragment "" "alerts.severity")
            FieldStatus -> builder |> filterWhereSql (#status, fragment "" "alerts.status")
            _ -> builder
    FacetLabel name -> builder |> filterWhereSql (#labels, fragment accessor ("alerts.labels " <> accessor))
      where
        accessor = "->> " <> quoteSqlText name
    FacetAttr name -> builder |> filterWhereSql (#facets, fragment accessor ("alerts.facets " <> accessor))
      where
        accessor = "->> " <> quoteSqlText name
  where
    -- A NULL effective value (facet and raw column both absent) never
    -- matches, mirroring matchClauseAlert on Nothing.
    effectiveCondition expr =
        "IS NOT NULL AND " <> case clause.mcOp of
            OpEq -> expr <> " = " <> quoteSqlText clause.mcValue
            OpNe -> expr <> " IS NOT NULL AND " <> expr <> " <> " <> quoteSqlText clause.mcValue
            OpGlob -> expr <> " LIKE " <> quoteSqlText (globToLike clause.mcValue)
            OpIn -> expr <> " IN (" <> Text.intercalate ", " (map quoteSqlText clause.mcValues) <> ")"
    -- filterWhereSql appends the fragment after the qualified proxy
    -- column; `accessor` extends the column to the value expression and
    -- `valueExpr` repeats it in full for the != null guard.
    fragment accessor valueExpr = case clause.mcOp of
        OpEq -> accessor <> " = " <> quoteSqlText clause.mcValue
        OpNe -> accessor <> " IS NOT NULL AND " <> valueExpr <> " <> " <> quoteSqlText clause.mcValue
        OpGlob -> accessor <> " LIKE " <> quoteSqlText (globToLike clause.mcValue)
        OpIn -> accessor <> " IN (" <> Text.intercalate ", " (map quoteSqlText clause.mcValues) <> ")"
