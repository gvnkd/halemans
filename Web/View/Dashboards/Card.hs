module Web.View.Dashboards.Card where

import Application.Helper.DashboardConfig (defaultAlertListColumns, defaultAlertListPageSize)
import Application.Service.DashboardCards (ExpandedCard (..))
import Application.Service.DynTable
import Web.View.Dashboards.Show (cardTitleText)
import Web.View.DynTable (DynTable (..), dynTableHtml)
import Web.View.Fragments (alertBaseColumns, alertRowHtmlCols)
import Web.View.Prelude

data CardView = CardView
    { dashboard :: Dashboard
    , expandedCard :: ExpandedCard
    , alerts :: [Alert]
    , sortColumn :: Text
    , sortDir :: Text
    , visibleCols :: [Text]
    }

instance View CardView where
    html CardView{..} =
        [hsx|
        <h1>{dashboard.name}</h1>
        <h2 data-testid="dashboard-card-title">{cardTitleText expandedCard.ecCard}</h2>
        <p>
            <a href={ShowDashboardAction dashboard.id} class="btn btn-sm btn-outline-secondary">Back to dashboard</a>
        </p>
        {table}
        {emptyNote}
    |]
      where
        table =
            dynTableHtml
                DynTable
                    { dtTestId = Just "dashboard-card-alerts"
                    , dtTbodyId = "dashboard-card-alerts-tbody"
                    , dtLiveScope = Nothing
                    , dtLiveFilters = Nothing
                    , dtTableClass = "table"
                    , dtConfig = tableConfig
                    , dtState = tableState
                    , dtBasePath = pathTo (ShowDashboardCardAction dashboard.id expandedCard.ecIndex)
                    , dtResetUrl = Nothing
                    , -- The pinned forEach value rides along on every widget
                      -- link/form submission.
                      dtExtraItems = valueItem
                    , dtTotal = fromIntegral (length alerts)
                    , dtRows = alerts
                    , dtRowHtml = \visible alert -> alertRowHtmlCols Nothing (map colKey visible) alert
                    }
        tableConfig =
            TableConfig
                { cfgName = "dashboard-card"
                , cfgColumns = alertBaseColumns
                , cfgDefaultVisible = defaultAlertListColumns
                , cfgDefaultSort = "last_seen_at"
                , cfgDefaultDir = "desc"
                , cfgPageSizes = []
                , cfgDefaultPageSize = defaultAlertListPageSize
                , cfgColumnPicker = True
                , cfgPager = False
                }
        tableState =
            TableState
                { tsSort = sortColumn
                , tsDir = sortDir
                , tsPage = 1
                , tsPageSize = max 1 (length alerts)
                , tsVisible = visibleCols
                , tsFilters = []
                }
        valueItem = maybe [] (\value -> [("value", Just (cs value))]) expandedCard.ecValue
        emptyNote =
            if null alerts
                then [hsx|<p class="text-secondary" data-testid="dashboard-card-empty">No matching alerts.</p>|]
                else mempty
