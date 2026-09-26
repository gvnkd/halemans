module Web.View.Teams.Edit where

import qualified Data.Aeson as Aeson
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude
import Web.View.Teams.New (hostGroupPicker, memberPicker)

data EditView = EditView
    { team :: Team
    , users :: [User]
    , currentRoles :: [(Id User, Text)]
    , hostGroups :: [Text]
    , availableGroups :: [Text]
    }

instance View EditView where
    beforeRender _ = setPageTitle (tr "Teams")
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit team") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={UpdateTeamAction team.id} data-testid="team-edit-form">
            <div class="mb-3">
                <label class="form-label">{tr "Name"}</label>
                <input name="name" type="text" class="form-control" value={team.name} data-testid="team-name" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Description"}</label>
                <input name="description" type="text" class="form-control" value={team.description} data-testid="team-description"/>
            </div>
            {hostGroupPicker availableGroups hostGroups}
            {memberPicker users currentRoles}
            <div class="mb-3">
                <label class="form-label">{tr "Default dashboard config (JSON)"}</label>
                <textarea name="defaultDashboardConfig" class="form-control font-monospace" rows="4" data-testid="team-default-dashboard-config">{defaultConfig}</textarea>
                <div class="form-text">{trp "Template offered to team members with no own dashboard (e.g. {example}). Empty clears it." [("example", exampleConfig)]}</div>
            </div>
            <div class="mb-3">
                <label class="form-label">{tr "Team defaults (JSON)"}</label>
                <textarea name="defaults" class="form-control font-monospace" rows="3" data-testid="team-defaults">{defaultsValue}</textarea>
                <div class="form-text">{tr "Free-form JSON attached to the team (provisionable as teams.<name>.defaults). Empty clears it."}</div>
            </div>
            <button type="submit" class="btn btn-brand" data-testid="team-submit">{tr "Save"}</button>
        </form>
        </div></div>
    |]
      where
        defaultConfig :: Text
        defaultConfig = maybe "" (cs . Aeson.encode) team.defaultDashboardConfig
        defaultsValue :: Text
        defaultsValue = cs (Aeson.encode team.defaults)
        exampleConfig :: Text
        exampleConfig = "[{\"env\": \"dev\"}]"
