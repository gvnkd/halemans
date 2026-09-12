module Web.Controller.Dashboard where

import qualified Data.Aeson as Aeson
import Web.Controller.Prelude
import Web.View.Dashboard.Index

-- Landing page (design_docs/milestone_3.md §7): the user's default dashboard
-- when set; the milestone-1 overview otherwise, which falls back to the
-- team's default_dashboard_config when the user has no dashboards yet.
instance Controller DashboardController where
    beforeAction = ensureIsUser

    action DashboardAction = do
        defaultDashboard <-
            query @Dashboard
                |> filterWhere (#userId, currentUserId)
                |> filterWhere (#isDefault, True)
                |> fetchOneOrNothing
        case defaultDashboard of
            Just dashboard -> redirectTo ShowDashboardAction{dashboardId = get #id dashboard}
            Nothing -> do
                ownCount <-
                    query @Dashboard
                        |> filterWhere (#userId, currentUserId)
                        |> fetchCount
                teamDefault <- if ownCount == 0 then firstTeamDefault else pure Nothing
                (cards, unassigned) <- computeEnvCards
                render IndexView{cards, unassigned, teamDefault}

firstTeamDefault :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond, CurrentUserRecord ~ User) => IO (Maybe Aeson.Value)
firstTeamDefault = do
    membership <-
        query @TeamMember
            |> filterWhere (#userId, currentUserId)
            |> orderByAsc #createdAt
            |> fetchOneOrNothing
    case membership of
        Nothing -> pure Nothing
        Just member -> do
            team <- fetch member.teamId
            pure team.defaultDashboardConfig
