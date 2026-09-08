module Web.View.Dashboards.Show where
import Web.View.Prelude
import Web.View.Fragments (alertRowHtml)
import Application.Helper.DashboardConfig
import Application.Pipeline.Grouping (AlertField (..))
import Application.Service.DashboardCards (CardGroup (..))
import qualified Data.Text as Text

data ShowView = ShowView
    { dashboard :: Dashboard
    , cardSections :: [(Int, DashboardCard, CardData)]
    }

data CardData = FlatCard [Alert] | GroupedCard [CardGroup]

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div data-live-scope={liveScope}>
            <h1 data-testid="dashboard-title">{dashboard.name}</h1>
            <p>
                <a href={EditDashboardAction dashboard.id} class="btn btn-sm btn-outline-primary">Edit</a>
                <a href={DashboardsAction} class="btn btn-sm btn-outline-secondary">All dashboards</a>
            </p>
            {forEach cardSections renderCardSection}
        </div>
    |]
        where
            liveScope :: Text
            liveScope = "dash:" <> tshow dashboard.id

-- | Stable DOM key: legacy cards keep the M3 convention
-- (dashboard-card-<env>); v2 cards key by position.
cardSectionDomId :: Int -> DashboardCard -> Text
cardSectionDomId index card = "dashboard-card-" <> fromMaybe (tshow index) (legacyEnv card)

legacyEnv :: DashboardCard -> Maybe Text
legacyEnv card
    | card.cardLegacy = head [value | MatchClause (FacetField FieldEnv) OpEq value _ <- card.cardMatch]
    | otherwise = Nothing

cardTitleText :: Int -> DashboardCard -> Text
cardTitleText index card = case (card.cardTitle, legacyEnv card, card.cardGroupBy) of
    (Just title, _, _) -> title
    (Nothing, Just env, _) -> env
    (Nothing, Nothing, Just groupBy) -> "by " <> facetRefText groupBy
    _ -> "card " <> tshow index

renderCardSection :: (Int, DashboardCard, CardData) -> Html
renderCardSection (index, card, result) = [hsx|
    <section class="dashboard-card mb-4" id={domId} data-testid={domId}>
        <h2>
            {cardTitleText index card}
            {filterChips}
        </h2>
        {cardBody}
    </section>
|]
    where
        domId = cardSectionDomId index card
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
        tbodyId :: Text
        tbodyId = "dashboard-card-tbody-" <> fromMaybe (tshow index) (legacyEnv card)
        filterChips = [hsx|
            <span>
                {forEach (legacyList FieldStatus) statusChip}
                {forEach (legacyList FieldSeverity) severityChip}
            </span>
        |]
        legacyList field
            | card.cardLegacy = concat [values | MatchClause (FacetField f) OpIn _ values <- card.cardMatch, f == field]
            | otherwise = []

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
