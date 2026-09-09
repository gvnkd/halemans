module Web.View.Dashboards.Show where
import Web.View.Prelude
import Web.View.Fragments (alertRowHtml, RollupCard (..), rollupCardHtml, statusBadgeHtml, severityBadgeHtml)
import Application.Helper.DashboardConfig
import Application.Pipeline.Grouping (AlertField (..))
import Application.Service.DashboardCards (CardGroup (..), CardSummary (..), ExpandedCard (..), runCardQuery, runCardQueryGroups, runCardSummary)
import Network.HTTP.Types (urlEncode)
import qualified Data.Text as Text

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
    html ShowView { .. } = [hsx|
        <div data-live-scope={liveScope}>
            <h1 data-testid="dashboard-title">{dashboard.name}</h1>
            <p>
                <a href={EditDashboardAction dashboard.id} class="btn btn-sm btn-outline-primary">Edit</a>
                <a href={DashboardsAction} class="btn btn-sm btn-outline-secondary">All dashboards</a>
            </p>
            <div id="dashboard-cards">
                {forEach cardSections (renderCardSection dashboard.id)}
            </div>
        </div>
    |]
        where
            liveScope :: Text
            liveScope = "dash:" <> tshow dashboard.id

renderCardSection :: Id Dashboard -> (ExpandedCard, CardData) -> Html
renderCardSection dashboardId (expanded, result) = case result of
    HiddenCard -> [hsx|<section class={sectionClass <> " d-none"} id={domId} data-testid={domId} style={widthStyle}></section>|]
    _ -> [hsx|
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
            FlatCard alerts -> [hsx|
                <table class="table table-sm">
                    <tbody id={tbodyId}>
                        {forEach alerts alertRowHtml}
                    </tbody>
                </table>
            |]
            GroupedCard groups -> [hsx|
                {forEach groups (renderGroup domId)}
            |]
            SummaryCard summary -> renderSummary dashboardId expanded summary
            HiddenCard -> [hsx||]
        tbodyId :: Text
        tbodyId = domId <> "-tbody"
        filterChips = [hsx|
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

cardTitleText :: DashboardCard -> Text
cardTitleText card = case (card.cardTitle, legacyEnv card, card.cardGroupBy) of
    (Just title, _, _) -> title
    (Nothing, Just env, _) -> env
    (Nothing, Nothing, Just groupBy) -> "by " <> facetRefText groupBy
    _ -> "card"

legacyEnv :: DashboardCard -> Maybe Text
legacyEnv card
    | card.cardLegacy = head [value | MatchClause (FacetField FieldEnv) OpEq value _ <- card.cardMatch]
    | otherwise = Nothing

-- | Overview-style rollup body via the shared widget (same renderer and
-- tooltips as the overview env cards); the whole card links to the
-- card-alerts detail page.
renderSummary :: Id Dashboard -> ExpandedCard -> CardSummary -> Html
renderSummary dashboardId expanded summary = rollupCardHtml RollupCard
    { rcTitle = mempty
    , rcWorstSeverity = summary.csWorstSeverity
    , rcFiring = summary.csFiring
    , rcAcked = summary.csAcked
    , rcResolved = summary.csResolved
    , rcSuppressed = summary.csSuppressed
    , rcHourly = summary.csHourly
    , rcLink = Just (cardAlertsLink dashboardId expanded)
    , rcSize = expanded.ecCard.cardSize
    }

renderGroup :: Text -> CardGroup -> Html
renderGroup cardId group = [hsx|
    <div class="dashboard-group mb-3" id={groupId} data-testid={groupId}>
        <h3>
            {group.cgValue}
            {severityBadgeHtml group.cgWorstSeverity Nothing}
            <span class="badge bg-secondary" data-testid="dashboard-group-count">{group.cgTotal}</span>
        </h3>
        <table class="table table-sm">
            <tbody>
                {forEach group.cgAlerts alertRowHtml}
            </tbody>
        </table>
    </div>
|]
    where
        groupId = cardId <> "-group-" <> Text.replace " " "_" group.cgValue

severityChip :: Text -> Html
severityChip severity = severityBadgeHtml severity Nothing
