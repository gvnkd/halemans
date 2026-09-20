module Web.View.Dashboards.Show where

import Application.Helper.DashboardConfig
import Application.Pipeline.Grouping (AlertField (..))
import Application.Service.DashboardCards (CardGroup (..), CardSummary (..), ExpandedCard (..), runCardQuery, runCardQueryGroups, runCardSummary)
import Application.Service.DynTable
import qualified Data.Text as Text
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.HTTP.Types (urlEncode)
import Web.View.DynTable (DynTable (..), dynTableHtml)
import Web.View.Fragments (RollupCard (..), alertRowHtmlCols, alertStaticColumns, pageHeaderTestIdHtml, rollupCardHtml, severityBadgeHtml, statusBadgeHtml)
import Web.View.Prelude

data ShowView = ShowView
    { dashboard :: Dashboard
    , cardSections :: [(ExpandedCard, CardData)]
    }

data CardData = FlatCard [Alert] | GroupedCard [CardGroup] | SummaryCard CardSummary | HiddenCard

-- | Query the concrete card's data unless its hideWhen condition fired.
-- "summary": true wins over groupBy (a rollup has no per-group breakdown).
fetchCardData :: (?modelContext :: ModelContext) => ExpandedCard -> IO CardData
fetchCardData expandedCard
    | expandedCard.ecHidden = pure HiddenCard
    | expandedCard.ecCard.cardSummary = SummaryCard <$> runCardSummary expandedCard.ecCard
    | otherwise = case expandedCard.ecCard.cardGroupBy of
        Nothing -> FlatCard <$> runCardQuery expandedCard.ecCard
        Just groupBy -> GroupedCard <$> runCardQueryGroups expandedCard.ecCard groupBy

instance View ShowView where
    html ShowView{..} =
        [hsx|
        <div data-live-scope={liveScope}>
            {pageHeaderTestIdHtml dashboard.name "dashboard-title" headerActions}
            <div id="dashboard-cards">
                {forEach cardSections (renderCardSection dashboard.id)}
            </div>
        </div>
    |]
      where
        headerActions =
            [hsx|
                <a href={EditDashboardAction dashboard.id} class="btn btn-sm btn-outline-primary">{tr "Edit"}</a>
                <a href={DashboardsAction} class="btn btn-sm btn-outline-secondary">{tr "All dashboards"}</a>
            |]
        liveScope :: Text
        liveScope = "dash:" <> tshow dashboard.id

renderCardSection :: Id Dashboard -> (ExpandedCard, CardData) -> Html
renderCardSection dashboardId (expanded, result) = case result of
    HiddenCard -> [hsx|<section class={sectionClass <> " d-none"} id={domId} data-testid={domId} style={widthStyle}></section>|]
    _ ->
        [hsx|
        <section class={sectionClass} id={domId} data-testid={domId} style={widthStyle}>
            <h2>
                {cardTitleText expanded.ecCard}
                {filterChips}
            </h2>
            {cardBody}
        </section>
    |]
  where
    card = expanded.ecCard
    domId = expanded.ecDomId
    sectionClass :: Text
    sectionClass = "dashboard-card" <> if card.cardSummary then " dashboard-card-summary mb-3" else " mb-4"
    -- Summary cards are fixed-width flex items; "size".width overrides
    -- the CSS flex-basis, flat/grouped cards always span full width.
    widthStyle :: Maybe Text
    widthStyle = case (card.cardSummary, card.cardSize) of
        (True, Just size) -> Just ("flex: 0 0 " <> tshow size.csWidth <> "px")
        _ -> Nothing
    cardBody = case result of
        FlatCard alerts -> staticAlertsTable tbodyId alerts
        GroupedCard groups ->
            [hsx|
                {forEach groups (renderGroup domId)}
            |]
        SummaryCard summary -> renderSummary dashboardId expanded summary
        HiddenCard -> [hsx||]
    tbodyId :: Text
    tbodyId = domId <> "-tbody"
    filterChips =
        [hsx|
            <span>
                {forEach (legacyList FieldStatus) statusBadgeHtml}
                {forEach (legacyList FieldSeverity) severityChip}
            </span>
        |]
    legacyList field
        | card.cardLegacy = concat [values | MatchClause (FacetField f) OpIn _ values <- card.cardMatch, f == field]
        | otherwise = []

-- | Link to the card-alerts detail page; forEach expansions pass their value
-- as a query param.
cardAlertsLink :: Id Dashboard -> ExpandedCard -> Text
cardAlertsLink dashboardId expanded =
    pathTo (ShowDashboardCardAction dashboardId expanded.ecIndex) <> valueQuery
  where
    valueQuery = case expanded.ecValue of
        Nothing -> ""
        Just value -> "?value=" <> cs (urlEncode True (cs value))

cardTitleText :: (CurrentUserRecord ~ User, ?request :: Request) => DashboardCard -> Text
cardTitleText card = case (card.cardTitle, legacyEnv card, card.cardGroupBy) of
    (Just title, _, _) -> title
    (Nothing, Just env, _) -> env
    (Nothing, Nothing, Just groupBy) -> trp "by {field}" [("field", facetRefText groupBy)]
    _ -> tr "card"

legacyEnv :: DashboardCard -> Maybe Text
legacyEnv card
    | card.cardLegacy = head [value | MatchClause (FacetField FieldEnv) OpEq value _ <- card.cardMatch]
    | otherwise = Nothing

-- | Overview-style rollup body via the shared widget (same renderer and
-- tooltips as the overview env cards); the whole card links to the
-- card-alerts detail page.
renderSummary :: Id Dashboard -> ExpandedCard -> CardSummary -> Html
renderSummary dashboardId expanded summary =
    rollupCardHtml
        RollupCard
            { rcTitle = mempty
            , rcWorstSeverity = summary.csWorstSeverity
            , rcFiring = summary.csFiring
            , rcAcked = summary.csAcked
            , rcResolved = summary.csResolved
            , rcStalled = summary.csStalled
            , rcSuppressed = summary.csSuppressed
            , rcHourly = summary.csHourly
            , rcLink = Just (cardAlertsLink dashboardId expanded)
            , rcSize = expanded.ecCard.cardSize
            }

renderGroup :: Text -> CardGroup -> Html
renderGroup cardId group =
    [hsx|
    <div class="dashboard-group mb-3" id={groupId} data-testid={groupId}>
        <h3>
            {group.cgValue}
            {severityBadgeHtml group.cgWorstSeverity Nothing}
            <span class="badge bg-secondary" data-testid="dashboard-group-count">{group.cgTotal}</span>
        </h3>
        {membersTable}
    </div>
|]
  where
    groupId = cardId <> "-group-" <> Text.replace " " "_" group.cgValue
    membersTable = staticAlertsTable (groupId <> "-tbody") group.cgAlerts

severityChip :: Text -> Html
severityChip severity = severityBadgeHtml severity Nothing

-- Embedded card tables are presentational only: no picker/pager (the card
-- limit already caps the rows), plain headers, no wrapping form.
staticAlertsTable :: Text -> [Alert] -> Html
staticAlertsTable tbodyId alerts =
    dynTableHtml
        DynTable
            { dtTestId = Nothing
            , dtTbodyId = tbodyId
            , dtLiveScope = Nothing
            , dtLiveFilters = Nothing
            , dtTableClass = "table table-sm"
            , dtConfig =
                TableConfig
                    { cfgName = tbodyId
                    , cfgColumns = alertStaticColumns
                    , cfgDefaultVisible = defaultAlertListColumns
                    , cfgDefaultSort = "last_seen_at"
                    , cfgDefaultDir = "desc"
                    , cfgPageSizes = []
                    , cfgDefaultPageSize = defaultAlertListPageSize
                    , cfgColumnPicker = False
                    , cfgPager = False
                    }
            , dtState =
                TableState
                    { tsSort = "last_seen_at"
                    , tsDir = "desc"
                    , tsPage = 1
                    , tsPageSize = max 1 (length alerts)
                    , tsVisible = defaultAlertListColumns
                    , tsFilters = []
                    }
            , dtBasePath = ""
            , dtResetUrl = Nothing
            , dtExtraItems = []
            , dtTotal = fromIntegral (length alerts)
            , dtRows = alerts
            , dtRowHtml = \visible alert -> alertRowHtmlCols Nothing (map colKey visible) alert
            }
