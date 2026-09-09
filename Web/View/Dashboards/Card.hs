module Web.View.Dashboards.Card where
import Web.View.Prelude
import Web.View.Fragments (alertRowHtml)
import Web.View.Dashboards.Show (cardTitleText)
import Application.Service.DashboardCards (ExpandedCard (..))

data CardView = CardView
    { dashboard :: Dashboard
    , expandedCard :: ExpandedCard
    , alerts :: [Alert]
    }

instance View CardView where
    html CardView { .. } = [hsx|
        <h1>{dashboard.name}</h1>
        <h2 data-testid="dashboard-card-title">{cardTitleText expandedCard.ecCard}</h2>
        <p>
            <a href={ShowDashboardAction dashboard.id} class="btn btn-sm btn-outline-secondary">Back to dashboard</a>
        </p>
        <table class="table table-sm" data-testid="dashboard-card-alerts">
            <tbody>
                {forEach alerts alertRowHtml}
            </tbody>
        </table>
        {emptyNote}
    |]
        where
            emptyNote = if null alerts
                then [hsx|<p class="text-secondary" data-testid="dashboard-card-empty">No matching alerts.</p>|]
                else mempty
