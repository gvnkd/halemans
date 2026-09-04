module Web.Controller.Dashboards where

import Web.Controller.Prelude
import Web.View.Dashboards.Index
import Web.View.Dashboards.New
import Web.View.Dashboards.Edit
import Web.View.Dashboards.Show
import Application.Helper.DashboardConfig
import qualified Data.Aeson as Aeson
import Data.Either (fromRight)
import Control.Monad (void)
import IHP.ModelSupport (withTransaction)
import IHP.ControllerSupport (respondAndExit)
import Network.HTTP.Types (status404)
import Network.Wai (responseLBS)

instance Controller DashboardsController where
    beforeAction = ensureIsUser

    action DashboardsAction = do
        dashboards <- ownDashboards
        render IndexView { .. }

    action NewDashboardAction = do
        render NewView

    action CreateDashboardAction = do
        let name = param @Text "name"
            configText = param @Text "config"
            isDefault = paramOrNothing @Text "isDefault" |> isJust
        case decodeDashboardConfigText configText of
            Left err -> do
                setErrorMessage ("Invalid dashboard config: " <> err)
                redirectTo NewDashboardAction
            Right cards -> do
                position <- nextPosition
                _ <- withTransaction do
                    when isDefault unsetDefaults
                    newRecord @Dashboard
                        |> set #userId currentUserId
                        |> set #name name
                        |> set #config (encodeDashboardConfig cards)
                        |> set #position position
                        |> set #isDefault isDefault
                        |> createRecord
                setSuccessMessage "Dashboard created"
                redirectTo DashboardsAction

    action ShowDashboardAction { dashboardId } = do
        dashboard <- fetchOwn dashboardId
        let cards = fromRight [] (decodeDashboardConfig dashboard.config)
        cardAlerts <- forM cards \card -> do
            alerts <- cardQuery card |> fetch
            pure (card, alerts)
        render ShowView { dashboard, cardAlerts }

    action EditDashboardAction { dashboardId } = do
        dashboard <- fetchOwn dashboardId
        let cards = fromRight [] (decodeDashboardConfig dashboard.config)
        render EditView { dashboard, configText = renderDashboardConfig cards }

    action UpdateDashboardAction { dashboardId } = do
        dashboard <- fetchOwn dashboardId
        let name = param @Text "name"
            configText = param @Text "config"
            isDefault = paramOrNothing @Text "isDefault" |> isJust
        case decodeDashboardConfigText configText of
            Left err -> do
                setErrorMessage ("Invalid dashboard config: " <> err)
                redirectTo EditDashboardAction { dashboardId }
            Right cards -> do
                _ <- withTransaction do
                    when isDefault unsetDefaults
                    dashboard
                        |> set #name name
                        |> set #config (encodeDashboardConfig cards)
                        |> set #isDefault isDefault
                        |> updateRecord
                setSuccessMessage "Dashboard updated"
                redirectTo DashboardsAction

    action DeleteDashboardAction { dashboardId } = do
        dashboard <- fetchOwn dashboardId
        deleteRecord dashboard
        setSuccessMessage "Dashboard deleted"
        redirectTo DashboardsAction

    action SetDefaultDashboardAction { dashboardId } = do
        dashboard <- fetchOwn dashboardId
        _ <- withTransaction do
            unsetDefaults
            dashboard
                |> set #isDefault True
                |> updateRecord
        setSuccessMessage "Default dashboard set"
        redirectTo DashboardsAction

    action MoveDashboardAction { dashboardId } = do
        dashboard <- fetchOwn dashboardId
        let position = param @Int "position"
        _ <- dashboard
            |> set #position position
            |> updateRecord
        renormalizePositions
        redirectTo DashboardsAction

-- Helpers

ownDashboards :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond, CurrentUserRecord ~ User) => IO [Dashboard]
ownDashboards = query @Dashboard
    |> filterWhere (#userId, currentUserId)
    |> orderByAsc #position
    |> fetch

fetchOwn :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond, CurrentUserRecord ~ User) => Id Dashboard -> IO Dashboard
fetchOwn dashboardId = do
    dashboard <- fetch dashboardId
    when (dashboard.userId /= currentUserId) do
        respondAndExit $ responseLBS status404 [("Content-Type", "text/plain")] "not found"
    pure dashboard

decodeDashboardConfigText :: Text -> Either Text [DashboardCard]
decodeDashboardConfigText raw =
    maybe (Left "config is not valid JSON") decodeDashboardConfig (Aeson.decode (cs raw))

unsetDefaults :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond, CurrentUserRecord ~ User) => IO ()
unsetDefaults = do
    defaults <- query @Dashboard
        |> filterWhere (#userId, currentUserId)
        |> filterWhere (#isDefault, True)
        |> fetch
    forM_ defaults \dashboard -> void do
        dashboard
            |> set #isDefault False
            |> updateRecord

nextPosition :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond, CurrentUserRecord ~ User) => IO Int
nextPosition = do
    dashboards <- ownDashboards
    pure (1 + maximum (0 : map (.position) dashboards))

renormalizePositions :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond, CurrentUserRecord ~ User) => IO ()
renormalizePositions = do
    dashboards <- ownDashboards
    forM_ (zip [1 ..] dashboards) \(position, dashboard) ->
        when (dashboard.position /= position) do
            void do
                dashboard
                    |> set #position position
                    |> updateRecord

cardQuery card = query @Alert
    |> filterWhere (#env, Just card.cardEnv)
    |> filterWhereNot (#status, "closed" :: Text)
    |> applyTextFilter #status card.cardStatuses
    |> applyTextFilter #severity card.cardSeverities
    |> orderByDesc #lastSeenAt
    |> limit 100

applyTextFilter field values builder = case values of
    [] -> builder
    _ -> builder |> filterWhereIn (field, values)
