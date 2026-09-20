module Web.View.Teams.Index where

import Web.View.Fragments (editDeleteActionsHtml, emptyStateHtml, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView {teamsWithMembers :: [(Team, [(User, Text)])]}

instance View IndexView where
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml (tr "Teams") newButton}
        {tableOrEmpty}
    |]
      where
        tableOrEmpty =
            if null teamsWithMembers
                then emptyStateHtml "teams-empty" (tr "No teams yet — create one to group members and route notifications.")
                else
                    [hsx|
        <table class="table" data-testid="teams-table">
            <thead>
                <tr>
                    <th>{tr "Name"}</th>
                    <th>{tr "Description"}</th>
                    <th>{tr "Members"}</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach teamsWithMembers renderTeam}
            </tbody>
        </table>
                    |]
        newButton = [hsx|<a href={NewTeamAction} class="btn btn-brand" data-testid="new-team">{tr "New team"}</a>|]

renderTeam :: (Team, [(User, Text)]) -> Html
renderTeam (team, members) =
    [hsx|
    <tr data-testid="team-row">
        <td>{team.name} {protectedBadgeHtml (get #protected team)}</td>
        <td>{team.description}</td>
        <td>{memberList}</td>
        <td>
            {editDeleteActionsHtml (pathTo (EditTeamAction team.id)) (pathTo (DeleteTeamAction team.id)) "edit-team"}
        </td>
    </tr>
|]
  where
    memberList = forEach members \(user, role) ->
        [hsx|
            <span class="badge">{user.email} ({role})</span>
        |]
