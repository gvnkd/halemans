module Web.View.Dashboards.Show where
import Web.View.Prelude
import Web.View.Fragments (alertRowHtml)
import Application.Helper.DashboardConfig (DashboardCard (..))
import qualified Data.Text as Text

data ShowView = ShowView
    { dashboard :: Dashboard
    , cardAlerts :: [(DashboardCard, [Alert])]
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div data-live-scope={liveScope}>
            <h1 data-testid="dashboard-title">{dashboard.name}</h1>
            <p>
                <a href={EditDashboardAction dashboard.id} class="btn btn-sm btn-outline-primary">Edit</a>
                <a href={DashboardsAction} class="btn btn-sm btn-outline-secondary">All dashboards</a>
            </p>
            {forEach cardAlerts renderCardSection}
        </div>
    |]
        where
            liveScope :: Text
            liveScope = Text.intercalate "," ["env:" <> card.cardEnv | (card, _) <- cardAlerts]

renderCardSection :: (DashboardCard, [Alert]) -> Html
renderCardSection (card, alerts) = [hsx|
    <section class="dashboard-card mb-4" data-testid={"dashboard-card-" <> card.cardEnv}>
        <h2>
            {card.cardEnv}
            {filterChips}
        </h2>
        <table class="table table-sm">
            <tbody id={tbodyId}>
                {forEach alerts alertRowHtml}
            </tbody>
        </table>
    </section>
|]
    where
        tbodyId :: Text
        tbodyId = "dashboard-card-tbody-" <> card.cardEnv
        filterChips = [hsx|
            <span>
                {forEach card.cardStatuses statusChip}
                {forEach card.cardSeverities severityChip}
            </span>
        |]

statusChip :: Text -> Html
statusChip status = [hsx|<span class={"badge status-" <> status}>{status}</span>|]

severityChip :: Text -> Html
severityChip severity = [hsx|<span class={"badge severity-badge severity-" <> severity}>{severity}</span>|]
