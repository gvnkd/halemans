module Web.View.Teams.Edit where
import Web.View.Prelude
import Web.View.Teams.New (memberPicker, hostGroupPicker)
import qualified Data.Aeson as Aeson

data EditView = EditView
    { team :: Team
    , users :: [User]
    , currentRoles :: [(Id User, Text)]
    , hostGroups :: [Text]
    , availableGroups :: [Text]
    }

instance View EditView where
    html EditView { .. } = [hsx|
        <h1>Edit team</h1>
        <form method="POST" action={UpdateTeamAction team.id} data-testid="team-edit-form" class="maxw-600">
            <div class="mb-3">
                <label class="form-label">Name</label>
                <input name="name" type="text" class="form-control" value={team.name} data-testid="team-name" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Description</label>
                <input name="description" type="text" class="form-control" value={team.description} data-testid="team-description"/>
            </div>
            {hostGroupPicker availableGroups hostGroups}
            {memberPicker users currentRoles}
            <div class="mb-3">
                <label class="form-label">Default dashboard config (JSON)</label>
                <textarea name="defaultDashboardConfig" class="form-control font-monospace" rows="4" data-testid="team-default-dashboard-config">{defaultConfig}</textarea>
                <div class="form-text">Template offered to team members with no own dashboard (e.g. {exampleConfig}). Empty clears it.</div>
            </div>
            <button type="submit" class="btn btn-primary" data-testid="team-submit">Save</button>
        </form>
    |]
        where
            defaultConfig :: Text
            defaultConfig = maybe "" (cs . Aeson.encode) team.defaultDashboardConfig
            exampleConfig :: Text
            exampleConfig = "[{\"env\": \"dev\"}]"
