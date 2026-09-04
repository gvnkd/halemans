module Web.View.Teams.Edit where
import Web.View.Prelude
import Web.View.Teams.New (memberPicker)

data EditView = EditView
    { team :: Team
    , users :: [User]
    , currentRoles :: [(Id User, Text)]
    }

instance View EditView where
    html EditView { .. } = [hsx|
        <h1>Edit team</h1>
        <form method="POST" action={UpdateTeamAction team.id} data-testid="team-edit-form" style="max-width: 600px">
            <div class="mb-3">
                <label class="form-label">Name</label>
                <input name="name" type="text" class="form-control" value={team.name} data-testid="team-name" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Description</label>
                <input name="description" type="text" class="form-control" value={team.description} data-testid="team-description"/>
            </div>
            {memberPicker users currentRoles}
            <button type="submit" class="btn btn-primary" data-testid="team-submit">Save</button>
        </form>
    |]
