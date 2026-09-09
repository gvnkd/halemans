module Web.View.Dashboards.Show where
import Web.View.Prelude
import Web.View.Fragments (alertRowHtml)
import Application.Helper.DashboardConfig
import Application.Pipeline.Grouping (AlertField (..))
import Application.Service.DashboardCards (CardGroup (..), ExpandedCard (..))
import qualified Data.Text as Text

data ShowView = ShowView
    { dashboard :: Dashboard
    , cardSections :: [(ExpandedCard, CardData)]
    }

data CardData = FlatCard [Alert] | GroupedCard [CardGroup] | HiddenCard

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div data-live-scope={liveScope}>
            <h1 data-testid="dashboard-title">{dashboard.name}</h1>
            <p>
                <a href={EditDashboardAction dashboard.id} class="btn btn-sm btn-outline-primary">Edit</a>
                <a href={DashboardsAction} class="btn btn-sm btn-outline-secondary">All dashboards</a>
            </p>
            <div id="dashboard-cards">
                {forEach cardSections renderCardSection}
            </div>
        </div>
    |]
        where
            liveScope :: Text
            liveScope = "dash:" <> tshow dashboard.id

renderCardSection :: (ExpandedCard, CardData) -> Html
renderCardSection (expanded, result) = case result of
    HiddenCard -> [hsx|<section class="dashboard-card d-none" id={domId} data-testid={domId}></section>|]
    _ -> [hsx|
        <section class="dashboard-card mb-4" id={domId} data-testid={domId}>
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
            HiddenCard -> [hsx||]
        tbodyId :: Text
        tbodyId = domId <> "-tbody"
        filterChips = [hsx|
            <span>
                {forEach (legacyList FieldStatus) statusChip}
                {forEach (legacyList FieldSeverity) severityChip}
            </span>
        |]
        legacyList field
            | card.cardLegacy = concat [values | MatchClause (FacetField f) OpIn _ values <- card.cardMatch, f == field]
            | otherwise = []

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

renderGroup :: Text -> CardGroup -> Html
renderGroup cardId group = [hsx|
    <div class="dashboard-group mb-3" id={groupId} data-testid={groupId}>
        <h3>
            {group.cgValue}
            <span class={"badge severity-badge severity-" <> group.cgWorstSeverity}>{group.cgWorstSeverity}</span>
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

statusChip :: Text -> Html
statusChip status = [hsx|<span class={"badge status-" <> status}>{status}</span>|]

severityChip :: Text -> Html
severityChip severity = [hsx|<span class={"badge severity-badge severity-" <> severity}>{severity}</span>|]
