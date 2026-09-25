module Web.View.Alerts.Index where

import Application.Helper.DashboardConfig (alertListPageSizes, defaultAlertListColumns, defaultAlertListPageSize)
import Application.Service.AlertList (AlertListFilters (..), alertFiltersToValue)
import Application.Service.DynTable
import qualified Data.Aeson as Aeson
import Web.View.DynTable (DynTable (..), dynTableHtml)
import Web.View.Fragments (alertListColumns, alertRowHtmlCols, alertSeverityOptions, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView
    { alerts :: [Alert]
    , filters :: AlertListFilters
    , counts :: [(Text, Int64)]
    , envNames :: [Text]
    , total :: Int64
    , groupKeys :: [(Id AlertGroup, Text)]
    -- ^ Group keys for the visible page, fetched only when the group
    -- column is shown.
    }

instance View IndexView where
    beforeRender _ = setPageTitle (tr "Alerts")
    html IndexView{..} =
        [hsx|
    <div>
        {pageHeaderHtml (tr "Alerts") mempty}
        {severityCounts}
        {table}
    </div>
    |]
      where
        table =
            dynTableHtml
                DynTable
                    { dtTestId = Just "alerts-table"
                    , dtTbodyId = "alerts-tbody"
                    , dtLiveScope = Just "alerts"
                    , -- Canonical filter state for the WS subscription: the URL can
                      -- be stale after turbolinks followed the prefs redirect
                      -- without a pushState, so the client reads THESE, not
                      -- location.search.
                      dtLiveFilters = Just liveFilters
                    , dtTableClass = "table"
                    , dtConfig = tableConfig
                    , dtState = tableState
                    , dtBasePath = pathTo AlertsAction
                    , dtResetUrl = Just resetUrl
                    , dtExtraItems = []
                    , dtTotal = total
                    , dtRows = alerts
                    , dtRowHtml = rowHtml
                    , dtEmptyText = tr "No alerts match the current filters."
                    }
        rowHtml visible alert =
            alertRowHtmlCols (alert.groupId >>= (`lookup` groupKeys)) (map colKey visible) alert
        liveFilters :: Text
        liveFilters = cs (Aeson.encode (alertFiltersToValue filters))
        resetUrl :: Text
        resetUrl = pathTo AlertsAction <> "?reset=1"

        tableConfig =
            TableConfig
                { cfgName = "alerts"
                , cfgColumns = alertListColumns (Just envNames) alerts
                , cfgDefaultVisible = defaultAlertListColumns
                , cfgDefaultSort = "last_seen_at"
                , cfgDefaultDir = "desc"
                , cfgPageSizes = alertListPageSizes
                , cfgDefaultPageSize = defaultAlertListPageSize
                , cfgColumnPicker = True
                , cfgPager = True
                }

        tableState =
            TableState
                { tsSort = filters.alfSort
                , tsDir = filters.alfDir
                , tsPage = filters.alfPage
                , tsPageSize = filters.alfPageSize
                , tsVisible = filters.alfColumns
                , tsFilters =
                    [("severity", filters.alfSeverities), ("status", filters.alfStatuses), ("env", filters.alfEnvs)]
                        ++ single "host" filters.alfHost
                        ++ single "service" filters.alfService
                        ++ single "q" filters.alfTitle
                        ++ single "group" filters.alfGroup
                        ++ [("muted", filters.alfMuted) | not (null filters.alfMuted)]
                        ++ single "occ_min" (tshow <$> filters.alfMinOccurrences)
                        ++ single "seen" filters.alfSeenWithin
                }
        single param = maybe [] (\value -> [(param, [value])])

        severityCounts =
            [hsx|
                <div class="mb-2" data-testid="severity-counts">
                    {forEach alertSeverityOptions countBadge}
                </div>
            |]
        countBadge severity =
            [hsx|
                <span class={"badge severity-badge severity-" <> severity <> " me-1"} data-testid={"count-" <> severity}>{severity} {countFor severity}</span>
            |]
        countFor severity = fromMaybe 0 (lookup severity counts)

baseItems :: AlertListFilters -> [(ByteString, Maybe ByteString)]
baseItems f =
    map (\value -> ("severity", Just (cs value))) f.alfSeverities
        ++ map (\value -> ("status", Just (cs value))) f.alfStatuses
        ++ map (\value -> ("env", Just (cs value))) f.alfEnvs
        ++ maybe [] (\value -> [("host", Just (cs value))]) f.alfHost
        ++ maybe [] (\value -> [("service", Just (cs value))]) f.alfService
        ++ maybe [] (\value -> [("q", Just (cs value))]) f.alfTitle
        ++ maybe [] (\value -> [("group", Just (cs value))]) f.alfGroup
        ++ map (\value -> ("muted", Just (cs value))) f.alfMuted
        ++ [("sort", Just (cs f.alfSort)), ("dir", Just (cs f.alfDir))]
        ++ map (\col -> ("cols", Just (cs col))) f.alfColumns
        ++ [("pageSize", Just (cs (tshow f.alfPageSize)))]
        ++ maybe [] (\value -> [("occ_min", Just (cs (tshow value)))]) f.alfMinOccurrences
        ++ maybe [] (\value -> [("seen", Just (cs value))]) f.alfSeenWithin
