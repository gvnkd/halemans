module Web.View.Dashboards.Card where
import Web.View.Prelude
import Web.View.Fragments (AlertsTable (..), AlertsTableSorting (..), AlertsTableContent (..), alertsTableHtml, nextSortDir)
import Web.View.Dashboards.Show (cardTitleText)
import Application.Service.DashboardCards (ExpandedCard (..))
import Network.HTTP.Types.URI (renderQuery)

data CardView = CardView
    { dashboard :: Dashboard
    , expandedCard :: ExpandedCard
    , alerts :: [Alert]
    , sortColumn :: Text
    , sortDir :: Text
    }

instance View CardView where
    html CardView { .. } = [hsx|
        <h1>{dashboard.name}</h1>
        <h2 data-testid="dashboard-card-title">{cardTitleText expandedCard.ecCard}</h2>
        <p>
            <a href={ShowDashboardAction dashboard.id} class="btn btn-sm btn-outline-secondary">Back to dashboard</a>
        </p>
        {table}
        {emptyNote}
    |]
        where
            table = alertsTableHtml AlertsTable
                { atTestId = Just "dashboard-card-alerts"
                , atTbodyId = "dashboard-card-alerts-tbody"
                , atLiveScope = Nothing
                , atLiveFilters = Nothing
                , atTableClass = "table"
                , atSorting = Just AlertsTableSorting
                    { atsSort = sortColumn
                    , atsDir = sortDir
                    , atsUrl = sortUrl
                    }
                , atContent = FlatAlerts alerts
                }
            emptyNote = if null alerts
                then [hsx|<p class="text-secondary" data-testid="dashboard-card-empty">No matching alerts.</p>|]
                else mempty
            sortUrl :: Text -> Text
            sortUrl column = pathTo (ShowDashboardCardAction dashboard.id expandedCard.ecIndex) <> cs (renderQuery True (queryItems column))
                where
                    queryItems col = valueItem ++
                        [ ("sort", Just (cs col))
                        , ("dir", Just (cs (nextSortDir sortColumn sortDir col)))
                        ]
                    valueItem = maybe [] (\value -> [("value", Just (cs value))]) expandedCard.ecValue
