module Web.Controller.Groups where

import Web.Controller.Prelude
import Web.View.Groups.Show
import Application.Pipeline.Actions (ackAlert)

instance Controller GroupsController where
    beforeAction = ensureIsUser

    action ShowGroupAction { groupId } = do
        group <- fetch groupId
        members <- query @Alert
            |> filterWhere (#groupId, Just groupId)
            |> orderByDesc #lastSeenAt
            |> fetch
        canAck <- currentUserHasPrivilege "ack"
        render ShowView { .. }

    -- Group ack acts on all firing members in one action (milestone_2.md §9).
    action AckGroupAction { groupId } = do
        requirePrivilege "ack"
        group <- fetch groupId
        firingMembers <- query @Alert
            |> filterWhere (#groupId, Just groupId)
            |> filterWhere (#status, "firing" :: Text)
            |> fetch
        forM_ firingMembers \alert ->
            ackAlert currentUser alert (Just "group ack") Nothing
        setSuccessMessage "Group acknowledged"
        redirectTo (ShowGroupAction groupId)
