module Web.View.Groups.Show where

import Web.View.Fragments (AlertsTable (..), AlertsTableContent (..), alertsTableHtml, groupHeaderHtml)
import Web.View.Prelude

data ShowView = ShowView
    { group :: AlertGroup
    , members :: [Alert]
    , canAck :: Bool
    }

instance View ShowView where
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
            alertsTableHtml
                AlertsTable
                    { atTestId = Just "group-members-table"
                    , atTbodyId = "group-members-tbody"
                    , atLiveScope = Nothing
                    , atLiveFilters = Nothing
                    , atTableClass = "table"
                    , atSorting = Nothing
                    , atContent = FlatAlerts members
                    }
        hasFiring = any (\alert -> alert.status == "firing") members
        ackButton =
            if canAck && hasFiring
                then
                    [hsx|
                    <form method="POST" action={AckGroupAction group.id} class="mb-3">
                        <button type="submit" class="btn btn-sm btn-warning" data-testid="ack-group">Ack all firing</button>
                    </form>
                |]
                else mempty
