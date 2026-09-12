module Web.Controller.Dashboards where

import Application.Helper.DashboardConfig
import Application.Service.DashboardCards (ExpandedCard (..), expandDashboardCards, expandedDomId, pinCard, runCardQuery)
import Control.Monad (guard, void)
import qualified Data.Aeson as Aeson
import Data.Either (fromRight)
import IHP.ControllerSupport (respondAndExit)
import IHP.ModelSupport (withTransaction)
import Network.HTTP.Types (status404)
import Network.Wai (ResponseReceived, responseLBS)
import Web.Controller.Prelude
import Web.View.Dashboards.Card
import Web.View.Dashboards.Edit
import Web.View.Dashboards.Index
import Web.View.Dashboards.New
import Web.View.Dashboards.Show

instance Controller DashboardsController where
    beforeAction = ensureIsUser

    action DashboardsAction = do
        dashboards <- ownDashboards
        render IndexView{..}
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
    action ShowDashboardAction{dashboardId} = do
        dashboard <- fetchOwn dashboardId
        let cards = fromRight [] (decodeDashboardConfig dashboard.config)
        expanded <- expandDashboardCards cards
        cardSections <- forM expanded \expandedCard -> do
            result <- fetchCardData expandedCard
            pure (expandedCard, result)
        render ShowView{dashboard, cardSections}
    action ShowDashboardCardAction{dashboardId, cardIndex} = do
        dashboard <- fetchOwn dashboardId
        let valueFilter = paramOrNothing @Text "value"
        let cards = fromRight [] (decodeDashboardConfig dashboard.config)
        expanded <- expandDashboardCards cards
        let matches = [ec | ec <- expanded, ec.ecIndex == cardIndex, maybe True (\value -> ec.ecValue == Just value) valueFilter]
        case matches of
            (expandedCard : _) -> renderCardDetail dashboard expandedCard
            [] -> case fallbackExpandedCard valueFilter cards cardIndex of
                Just expandedCard -> renderCardDetail dashboard expandedCard
                Nothing -> respondAndExit $ responseLBS status404 [("Content-Type", "text/plain")] "card not found"
    action EditDashboardAction{dashboardId} = do
        dashboard <- fetchOwn dashboardId
        let cards = fromRight [] (decodeDashboardConfig dashboard.config)
        render EditView{dashboard, configText = renderDashboardConfig cards}
    action UpdateDashboardAction{dashboardId} = do
        dashboard <- fetchOwn dashboardId
        let name = param @Text "name"
            configText = param @Text "config"
            isDefault = paramOrNothing @Text "isDefault" |> isJust
        case decodeDashboardConfigText configText of
            Left err -> do
                setErrorMessage ("Invalid dashboard config: " <> err)
                redirectTo EditDashboardAction{dashboardId}
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
    action DeleteDashboardAction{dashboardId} = do
        dashboard <- fetchOwn dashboardId
        deleteRecord dashboard
        setSuccessMessage "Dashboard deleted"
        redirectTo DashboardsAction
    action SetDefaultDashboardAction{dashboardId} = do
        dashboard <- fetchOwn dashboardId
        _ <- withTransaction do
            unsetDefaults
            dashboard
                |> set #isDefault True
                |> updateRecord
        setSuccessMessage "Default dashboard set"
        redirectTo DashboardsAction
    action MoveDashboardAction{dashboardId} = do
        dashboard <- fetchOwn dashboardId
        let position = param @Int "position"
        _ <-
            dashboard
                |> set #position position
                |> updateRecord
        renormalizePositions
        redirectTo DashboardsAction

-- Helpers

-- | The pinned value dropped out of the current expansion (its last
-- non-closed alert left): rebuild the pinned card synthetically so a
-- previously valid card link renders an empty table instead of 404ing.
fallbackExpandedCard :: Maybe Text -> [DashboardCard] -> Int -> Maybe ExpandedCard
fallbackExpandedCard valueFilter cards cardIndex = do
    template <- if cardIndex < length cards && cardIndex >= 0 then Just (cards !! cardIndex) else Nothing
    let card = case (valueFilter, template.cardForEach) of
            (Just value, Just facetRef) -> pinCard template facetRef value
            _ -> template
    pure
        ExpandedCard
            { ecDomId = expandedDomId cardIndex template valueFilter
            , ecCard = card
            , ecHidden = False
            , ecIndex = cardIndex
            , ecValue = valueFilter
            }

-- | Card detail table: transient ?sort/?dir params (never persisted to
-- users.settings, unlike /alerts) override the card's alertSortBy for this
-- render only; without either, alertSortBy or newest-first applies.
renderCardDetail :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond, CurrentUserRecord ~ User) => Dashboard -> ExpandedCard -> IO ResponseReceived
renderCardDetail dashboard expandedCard = do
    let sortParam = paramOrNothing @Text "sort"
        dirParam = paramOrNothing @Text "dir"
        override = do
            column <- sortParam
            guard (column `elem` validAlertSortColumns)
            let dir = if dirParam == Just "asc" then "asc" else "desc"
            pure (AlertSortKey (dir /= alertSortNaturalDir column) column, dir)
        effectiveCard = case override of
            Just (key, _) -> expandedCard.ecCard{cardAlertSortBy = [key]}
            Nothing -> expandedCard.ecCard
        (sortColumn, sortDir) = case override of
            Just (key, dir) -> (key.askColumn, dir)
            Nothing -> case expandedCard.ecCard.cardAlertSortBy of
                (key : _) -> (key.askColumn, alertSortDisplayDir key)
                [] -> ("last_seen_at", "desc")
    alerts <- runCardQuery effectiveCard
    render CardView{dashboard, expandedCard, alerts, sortColumn, sortDir}

ownDashboards :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond, CurrentUserRecord ~ User) => IO [Dashboard]
ownDashboards =
    query @Dashboard
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
    defaults <-
        query @Dashboard
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
