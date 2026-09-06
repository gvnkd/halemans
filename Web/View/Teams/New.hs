module Web.View.Teams.New where
import Web.View.Prelude

data NewView = NewView
    { users :: [User]
    , currentRoles :: [(Id User, Text)]
    , availableGroups :: [Text]
    }

instance View NewView where
    html NewView { .. } = [hsx|
        <h1>New team</h1>
        <form method="POST" action={CreateTeamAction} data-testid="team-form" style="max-width: 600px">
            <div class="mb-3">
                <label class="form-label">Name</label>
                <input name="name" type="text" class="form-control" data-testid="team-name" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Description</label>
                <input name="description" type="text" class="form-control" data-testid="team-description"/>
            </div>
            {hostGroupPicker availableGroups []}
            {memberPicker users currentRoles}
            <button type="submit" class="btn btn-primary" data-testid="team-submit">Create</button>
        </form>
    |]

-- | Multi-select of zabbix host groups gathered into zabbix_host_groups by
-- manual source sync. With an empty cache there is nothing to pick from, so
-- the picker degrades to instructions. Shared with the edit form.
hostGroupPicker :: [Text] -> [Text] -> Html
hostGroupPicker availableGroups selected = case availableGroups of
    [] -> [hsx|
        <div class="mb-3" data-testid="team-host-groups-empty">
            <label class="form-label">Zabbix host groups</label>
            <div class="form-text">
                No host groups fetched yet. Open the <a href={SourcesAction}>Sources</a> page
                and use "Sync host groups" on a zabbix source, then reload this page.
            </div>
        </div>
    |]
    _ -> [hsx|
        <div class="mb-3">
            <label class="form-label">Zabbix host groups</label>
            <select name="hostGroups" class="form-select" multiple="multiple" size={pickerSize} data-testid="team-host-groups">
                {forEach availableGroups groupOption}
            </select>
            <div class="form-text">Used by zabbix sources with host group scope "teams" to restrict which alerts are fetched. Ctrl-click to select multiple.</div>
        </div>
    |]
    where
        pickerSize :: Text
        pickerSize = tshow (min 8 (max 2 (length availableGroups)))
        groupOption name = [hsx|<option value={name} selected={name `elem` selected}>{name}</option>|]

-- | One select per user: "" (not a member) / member / lead. Shared with the
-- edit form.
memberPicker :: [User] -> [(Id User, Text)] -> Html
memberPicker users currentRoles = [hsx|
    <div class="mb-3" data-testid="team-members">
        <label class="form-label">Members</label>
        {forEach users userRow}
    </div>
|]
    where
        userRow user =
            let current = lookup (get #id user) currentRoles
            in [hsx|
                <div class="input-group input-group-sm mb-1">
                    <span class="input-group-text" style="min-width: 220px">{user.email}</span>
                    <select name={"member-" <> tshow (get #id user)} class="form-select" data-testid={"member-" <> user.email}>
                        <option value="" selected={isNothing current}>—</option>
                        <option value="member" selected={current == Just "member"}>member</option>
                        <option value="lead" selected={current == Just "lead"}>lead</option>
                    </select>
                </div>
            |]
