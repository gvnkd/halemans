module Web.View.Teams.New where
import Web.View.Prelude

data NewView = NewView
    { users :: [User]
    , currentRoles :: [(Id User, Text)]
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
            {memberPicker users currentRoles}
            <button type="submit" class="btn btn-primary" data-testid="team-submit">Create</button>
        </form>
    |]

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
