module Web.Controller.Teams where

import Application.Service.HostGroups (hostGroupsToJson, teamHostGroups)
import qualified Data.Aeson as Aeson
import Data.List (nub, sort)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Web.Controller.Prelude
import Web.View.Teams.Edit
import Web.View.Teams.Index
import Web.View.Teams.New

instance Controller TeamsController where
    beforeAction = ensureIsUser

    action TeamsAction = do
        requirePrivilege "manage_users"
        teams <- query @Team |> orderByAsc #name |> fetch
        members <- forM teams \team -> do
            rows <-
                query @TeamMember
                    |> filterWhere (#teamId, get #id team)
                    |> fetch
            forM rows \row -> do
                user <- fetch row.userId
                pure (user, row.teamRole)
        render IndexView{teamsWithMembers = zip teams members}
    action NewTeamAction = do
        requirePrivilege "manage_users"
        users <- query @User |> orderByAsc #email |> fetch
        availableGroups <- cachedHostGroupNames
        render NewView{users, currentRoles = [], availableGroups}
    action CreateTeamAction = do
        requirePrivilege "manage_users"
        team <-
            newRecord @Team
                |> set #name (param @Text "name")
                |> set #description (param @Text "description")
                |> set #hostGroups (hostGroupsToJson (paramList @Text "hostGroups"))
                |> createRecord
        saveMembers team
        setSuccessMessage "Team created"
        redirectTo TeamsAction
    action EditTeamAction{teamId} = do
        requirePrivilege "manage_users"
        team <- fetch teamId
        users <- query @User |> orderByAsc #email |> fetch
        rows <- query @TeamMember |> filterWhere (#teamId, teamId) |> fetch
        availableGroups <- cachedHostGroupNames
        let currentRoles = map (\row -> (row.userId, row.teamRole)) rows
            hostGroups = teamHostGroups team
        render EditView{team, users, currentRoles, hostGroups, availableGroups}
    action UpdateTeamAction{teamId} = do
        requirePrivilege "manage_users"
        team <- fetch teamId
        let dashboardConfig = paramOrNothing @Text "defaultDashboardConfig"
        case dashboardConfig of
            Just raw | raw /= "" -> case Aeson.decode (cs raw) of
                Nothing -> do
                    setErrorMessage "Default dashboard config is not valid JSON"
                    redirectTo EditTeamAction{teamId}
                Just config -> do
                    updateTeam team (Just config)
                    redirectTo TeamsAction
            _ -> do
                updateTeam team Nothing
                redirectTo TeamsAction
      where
        updateTeam team config = do
            updated <-
                team
                    |> set #name (param @Text "name")
                    |> set #description (param @Text "description")
                    |> set #hostGroups (hostGroupsToJson (paramList @Text "hostGroups"))
                    |> set #defaultDashboardConfig config
                    |> updateRecord
            _ <- sqlExecTyped [typedSql| DELETE FROM team_members WHERE team_id = ${teamId} |]
            saveMembers updated
            setSuccessMessage "Team updated"
    action DeleteTeamAction{teamId} = do
        requirePrivilege "manage_users"
        team <- fetch teamId
        _ <- sqlExecTyped [typedSql| DELETE FROM team_members WHERE team_id = ${teamId} |]
        _ <- sqlExecTyped [typedSql| DELETE FROM on_call_schedules WHERE team_id = ${teamId} |]
        deleteRecord team
        setSuccessMessage "Team deleted"
        redirectTo TeamsAction

-- | Distinct host group names across all zabbix source caches
-- (zabbix_host_groups), offered in the team form picker.
cachedHostGroupNames :: (?modelContext :: ModelContext) => IO [Text]
cachedHostGroupNames = do
    rows <- query @ZabbixHostGroup |> fetch
    pure (sort (nub (map (.name) rows)))

-- | Member picker: one select per user named member-<uuid> with values
-- ""/"member"/"lead". Also upserts the on-call schedule stub so
-- currentOnCall returns the first (lead-preferred) member (§7).
saveMembers :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond) => Team -> IO ()
saveMembers team = do
    users <- query @User |> orderByAsc #email |> fetch
    picks <- forM users \user -> do
        choice <- pure (paramOrNothing @Text (cs ("member-" <> tshow (get #id user) :: Text)))
        pure case choice of
            Just role | role `elem` ["member", "lead"] -> Just (get #id user, role)
            _ -> Nothing
    let selected = catMaybes picks
        ordered =
            map fst (filter (\(_, role) -> role == "lead") selected)
                ++ map fst (filter (\(_, role) -> role /= "lead") selected)
    forM_ selected \(userId, role) -> do
        _ <-
            newRecord @TeamMember
                |> set #teamId (get #id team)
                |> set #userId userId
                |> set #teamRole role
                |> createRecord
        pure ()
    let membersJson = Aeson.toJSON (map (tshow :: Id User -> Text) ordered)
        teamId = get #id team
    _ <-
        sqlExecTyped
            [typedSql|
        INSERT INTO on_call_schedules (team_id, members)
        VALUES (${teamId}, ${membersJson})
        ON CONFLICT (team_id) DO UPDATE SET members = EXCLUDED.members, updated_at = NOW()
    |]
    pure ()
