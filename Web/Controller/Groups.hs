module Web.Controller.Groups where

import Application.Helper.DashboardConfig (AlertSortKey (..), alertSortNaturalDir, validAlertSortColumns)
import Application.Pipeline.Actions (ackAlert)
import Application.Service.DashboardCards (sortCardAlerts)
import Web.Controller.Prelude
import Web.View.Groups.Show

instance Controller GroupsController where
    beforeAction = ensureIsUser

    action ShowGroupAction{groupId} = do
        group <- fetch groupId
        members <-
            query @Alert
                |> filterWhere (#groupId, Just groupId)
                |> orderByDesc #lastSeenAt
                |> fetch
        -- Server-side sort via the shared card sorter: stable over the
        -- newest-first fetch order, so ties stay newest-first.
        let sortColumn = case nonEmptyParam "sort" of
                Just col | col `elem` validAlertSortColumns -> col
                _ -> "last_seen_at"
            sortDir = if nonEmptyParam "dir" == Just "asc" then "asc" else "desc"
            sortedMembers = sortCardAlerts [AlertSortKey{askDesc = sortDir /= alertSortNaturalDir sortColumn, askColumn = sortColumn}] members
        canAck <- currentUserHasPrivilege "ack"
        render ShowView{members = sortedMembers, ..}

    -- Group ack acts on all firing members in one action (milestone_2.md §9).
    action AckGroupAction{groupId} = do
        requirePrivilege "ack"
        group <- fetch groupId
        firingMembers <-
            query @Alert
                |> filterWhere (#groupId, Just groupId)
                |> filterWhere (#status, "firing" :: Text)
                |> fetch
        forM_ firingMembers \alert ->
            ackAlert currentUser alert (Just "group ack") Nothing
        setSuccessMessage "Group acknowledged"
        redirectTo (ShowGroupAction groupId)
