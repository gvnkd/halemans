module Web.Controller.Alerts where

import Web.Controller.Prelude
import Web.View.Alerts.Index
import Web.View.Alerts.Show
import Application.Pipeline.Actions (ackAlert, unackAlert, closeAlert, addComment)
import IHP.TypedSql (sqlQueryTyped, typedSql)

instance Controller AlertsController where
    beforeAction = ensureIsUser

    action AlertsAction = do
        alerts <- query @Alert
            |> filterWhereNot (#status, "closed" :: Text)
            |> orderByDesc #lastSeenAt
            |> limit 200
            |> fetch
        render IndexView { .. }

    action ShowAlertAction { alertId } = do
        alert <- fetch alertId
        events <- query @AlertEvent
            |> filterWhere (#alertId, alertId)
            |> orderByAsc #createdAt
            |> fetch
        comments <- query @Comment
            |> filterWhere (#alertId, alertId)
            |> orderByAsc #createdAt
            |> fetch
        commentAuthors <- forM comments \comment -> fetch comment.userId
        eventActors <- forM (mapMaybe (.userId) events) \userId -> fetch userId
        canAck <- currentUserHasPrivilege "ack"
        canClose <- currentUserHasPrivilege "close"
        render ShowView { .. }

    action AckAlertAction { alertId } = do
        requirePrivilege "ack"
        alert <- fetch alertId
        let comment = paramOrNothing @Text "comment"
            timeoutMinutes = paramOrNothing @Int "timeoutMinutes"
        _ <- ackAlert currentUser alert comment timeoutMinutes
        redirectTo ShowAlertAction { alertId }

    action UnackAlertAction { alertId } = do
        requirePrivilege "ack"
        alert <- fetch alertId
        _ <- unackAlert (Just currentUser) alert "manual unack"
        redirectTo ShowAlertAction { alertId }

    action CloseAlertAction { alertId } = do
        requirePrivilege "close"
        alert <- fetch alertId
        let reason = paramOrNothing @Text "reason"
        _ <- closeAlert (Just currentUser) alert reason
        redirectTo ShowAlertAction { alertId }

    action CreateCommentAction { alertId } = do
        requirePrivilege "view"
        alert <- fetch alertId
        let body = param @Text "body"
        unless (null body) do
            _ <- addComment currentUser alert body
            pure ()
        redirectTo ShowAlertAction { alertId }
