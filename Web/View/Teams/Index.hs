module Web.View.Teams.Index where
import Web.View.Prelude

data IndexView = IndexView { teamsWithMembers :: [(Team, [(User, Text)])] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex justify-content-between align-items-center">
            <h1>Teams</h1>
            <a href={NewTeamAction} class="btn btn-sm btn-primary" data-testid="new-team">New team</a>
        </div>
        <table class="table" data-testid="teams-table">
            <thead>
                <tr>
                    <th>Name</th>
                    <th>Description</th>
                    <th>Members</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach teamsWithMembers renderTeam}
            </tbody>
        </table>
    |]

renderTeam :: (Team, [(User, Text)]) -> Html
renderTeam (team, members) = [hsx|
    <tr data-testid="team-row">
        <td>{team.name}</td>
        <td>{team.description}</td>
        <td>{memberList}</td>
        <td>
            <a href={EditTeamAction team.id} class="btn btn-sm btn-outline-secondary" data-testid="edit-team">Edit</a>
            <form method="POST" action={DeleteTeamAction team.id} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-danger">Delete</button>
            </form>
        </td>
    </tr>
|]
    where
        memberList = forEach members \(user, role) -> [hsx|
            <span class="badge bg-secondary">{user.email} ({role})</span>
        |]
