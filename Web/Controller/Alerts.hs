module Web.Controller.Alerts where

import Web.Controller.Prelude
import Web.View.Alerts.Index
import Web.View.Alerts.Show
import Application.Pipeline.Actions (ackAlert, unackAlert, closeAlert, addComment)
import qualified Application.Service.Cmdb as Cmdb
import qualified Application.Service.Jira as Jira
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Control.Monad (void)

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
        cmdbEntry <- case (alert.hostId, alert.serviceId) of
            (Just hostId, _) -> query @CmdbEntry
                |> filterWhere (#hostId, Just hostId)
                |> fetchOneOrNothing
            (Nothing, Just serviceId) -> query @CmdbEntry
                |> filterWhere (#serviceId, Just serviceId)
                |> fetchOneOrNothing
            (Nothing, Nothing) -> pure Nothing
        jiraLinks <- query @JiraLink
            |> filterWhere (#alertId, alertId)
            |> orderByAsc #createdAt
            |> fetch
        writeBackAttempts <- query @WriteBackAttempt
            |> filterWhere (#alertId, alertId)
            |> orderByDesc #createdAt
            |> limit 5
            |> fetch
        analyses <- query @LlmAnalysis
            |> filterWhere (#alertId, alertId)
            |> orderByDesc #createdAt
            |> limit 10
            |> fetch
        feedback <- case analyses of
            [] -> pure []
            _ -> query @LlmFeedback
                |> filterWhereIn (#analysisId, map (get #id) analyses)
                |> filterWhere (#userId, get #id currentUser)
                |> fetch
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

    action RefreshCmdbAction { alertId } = do
        requirePrivilege "view"
        alert <- fetch alertId
        forM_ alert.sourceId \sourceId -> do
            source <- fetch sourceId
            result <- Cmdb.refreshForAlert source alert
            case result of
                Left err -> setErrorMessage ("CMDB refresh failed: " <> err)
                Right _ -> setSuccessMessage "CMDB cache refreshed"
        redirectTo ShowAlertAction { alertId }

    action CreateJiraTicketAction { alertId } = do
        requirePrivilege "ack"
        alert <- fetch alertId
        case alert.sourceId of
            Nothing -> setErrorMessage "Alert has no source; cannot create ticket"
            Just sourceId -> do
                source <- fetch sourceId
                let issueType = paramOrNothing @Text "issueType" |> fromMaybe "Task"
                    defaultSummary = alert.title
                    defaultBody = alert.description <> "\n\nSource: " <> fromMaybe "-" alert.sourceUrl
                    summary = paramOrNothing @Text "summary" |> fromMaybe defaultSummary
                    body = paramOrNothing @Text "body" |> fromMaybe defaultBody
                result <- Jira.createTicketForAlert source alert issueType summary body
                case result of
                    Left err -> setErrorMessage ("Jira ticket creation failed: " <> err)
                    Right link -> setSuccessMessage ("Linked " <> link.ticketKey)
        redirectTo ShowAlertAction { alertId }

    action DeleteJiraLinkAction { alertId, jiraLinkId } = do
        requirePrivilege "ack"
        link <- fetch jiraLinkId
        if link.origin == "manual" && link.alertId == alertId
            then do
                deleteRecord link
                setSuccessMessage ("Unlinked " <> link.ticketKey)
            else setErrorMessage "Only manual links can be removed"
        redirectTo ShowAlertAction { alertId }

    -- Manual re-analyze (milestone_4.md §4): advisory and non-destructive, so
    -- "view" privilege suffices. Appends a fresh analysis row; the card shows
    -- the latest done.
    action ReanalyzeAlertAction { alertId } = do
        requirePrivilege "view"
        _ <- fetch alertId :: IO Alert
        analysis <- newRecord @LlmAnalysis
            |> set #alertId alertId
            |> createRecord
        _ <- newRecord @LlmAnalysisJob
            |> set #analysisId (get #id analysis)
            |> createRecord
        setSuccessMessage "LLM analysis queued"
        redirectTo ShowAlertAction { alertId }

    -- 👍/👎 feedback (milestone_4.md D6): one vote per user per analysis,
    -- re-vote updates in place.
    action LlmFeedbackAction { alertId, analysisId } = do
        requirePrivilege "view"
        analysis <- fetch analysisId
        if analysis.alertId /= alertId
            then setErrorMessage "Analysis does not belong to this alert"
            else do
                let score = param @Int "score"
                existing <- query @LlmFeedback
                    |> filterWhere (#analysisId, analysisId)
                    |> filterWhere (#userId, get #id currentUser)
                    |> fetchOneOrNothing
                case existing of
                    Just vote -> void (vote |> set #score score |> updateRecord)
                    Nothing -> void do
                        newRecord @LlmFeedback
                            |> set #analysisId analysisId
                            |> set #userId (get #id currentUser)
                            |> set #score score
                            |> createRecord
        redirectTo ShowAlertAction { alertId }
