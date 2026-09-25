module Web.View.Groups.Show where

import Application.Helper.DashboardConfig (defaultAlertListColumns, defaultAlertListPageSize)
import Application.Service.DynTable
import Web.View.DynTable (DynTable (..), dynTableHtml)
import Web.View.Fragments (alertBaseColumns, alertRowHtmlCols, groupHeaderHtml)
import Web.View.Prelude

data ShowView = ShowView
    { group :: AlertGroup
    , members :: [Alert]
    , canAck :: Bool
    , sortColumn :: Text
    , sortDir :: Text
    }

instance View ShowView where
    beforeRender ShowView{..} = setPageTitle group.groupKey
    html ShowView{..} =
        [hsx|
        <div data-live-scope={"group:" <> tshow group.id}>
            {groupHeaderHtml group}
            {ackButton}
            {membersTable}
        </div>
    |]
      where
        membersTable =
            dynTableHtml
                DynTable
                    { dtTestId = Just "group-members-table"
                    , dtTbodyId = "group-members-tbody"
                    , dtLiveScope = Nothing
                    , dtLiveFilters = Nothing
                    , dtTableClass = "table"
                    , dtConfig = tableConfig
                    , dtState = tableState
                    , dtBasePath = pathTo (ShowGroupAction group.id)
                    , dtResetUrl = Nothing
                    , dtExtraItems = []
                    , dtTotal = fromIntegral (length members)
                    , dtRows = members
                    , dtRowHtml = \visible alert -> alertRowHtmlCols Nothing (map colKey visible) alert
                    , dtEmptyText = tr "No alerts in this group."
                    }
        tableConfig =
            TableConfig
                { cfgName = "group-members"
                , cfgColumns = alertBaseColumns
                , cfgDefaultVisible = defaultAlertListColumns
                , cfgDefaultSort = "last_seen_at"
                , cfgDefaultDir = "desc"
                , cfgPageSizes = []
                , cfgDefaultPageSize = defaultAlertListPageSize
                , cfgColumnPicker = False
                , cfgPager = False
                }
        tableState =
            TableState
                { tsSort = sortColumn
                , tsDir = sortDir
                , tsPage = 1
                , tsPageSize = max 1 (length members)
                , tsVisible = defaultAlertListColumns
                , tsFilters = []
                }
        hasFiring = any (\alert -> alert.status == "firing") members
        ackButton =
            if canAck && hasFiring
                then
                    [hsx|
                    <form method="POST" action={AckGroupAction group.id} class="mb-3">
                        <button type="submit" class="btn btn-brand" data-testid="ack-group">{tr "Ack all firing"}</button>
                    </form>
                |]
                else mempty
