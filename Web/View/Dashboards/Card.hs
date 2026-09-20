module Web.View.Dashboards.Card where

import Application.Helper.DashboardConfig (defaultAlertListColumns, defaultAlertListPageSize)
import Application.Service.DashboardCards (ExpandedCard (..))
import Application.Service.DynTable
import Web.View.Dashboards.Show (cardTitleText)
import Web.View.DynTable (DynTable (..), dynTableHtml)
import Web.View.Fragments (alertBaseColumns, alertRowHtmlCols, emptyStateHtml, pageHeaderHtml)
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
    <div>
        {pageHeaderHtml dashboard.name backLink}
        <h2 data-testid="dashboard-card-title">{cardTitleText expandedCard.ecCard}</h2>
        {table}
        {emptyNote}
    </div>
    |]
      where
        backLink = [hsx|<a href={ShowDashboardAction dashboard.id} class="btn btn-sm btn-ghost">{tr "Back to dashboard"}</a>|]
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
                    , dtEmptyText = tr "No matching alerts."
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
                then emptyStateHtml "dashboard-card-empty" (tr "No matching alerts.")
                else mempty
