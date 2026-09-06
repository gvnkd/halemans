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
            <input type="text" class="form-control form-control-sm mb-1" placeholder="Filter groups…" data-testid="team-host-groups-filter" data-hg-filter=""/>
            <select name="hostGroups" class="form-select" multiple="multiple" size={pickerSize} data-testid="team-host-groups" data-hg-select="">
                {forEach availableGroups groupOption}
            </select>
            <div class="form-text">Used by zabbix sources with host group scope "teams" to restrict which alerts are fetched. Ctrl-click to select multiple.</div>
        </div>
    |]
    where
        pickerSize :: Text
        pickerSize = tshow (min 8 (max 2 (length availableGroups)))
        groupOption name = [hsx|<option value={name} selected={name `elem` selected}>{name}</option>|]

-- | Members section: only current members are visible; the filter field
-- reveals matching non-members so they can be added (pick a role in their
-- row). Remove resets the role to "" so saveMembers drops the row. Also
-- wires the host-group filter above. Behavior lives in teamPickersScript;
-- without JS every row stays visible (pre-rewrite behavior).
memberPicker :: [User] -> [(Id User, Text)] -> Html
memberPicker users currentRoles = [hsx|
    <div class="mb-3" data-testid="team-members">
        <label class="form-label">Members</label>
        <input type="text" class="form-control form-control-sm mb-1" placeholder="Filter users to add…" data-testid="team-members-filter" data-member-filter=""/>
        {forEach users userRow}
    </div>
    {teamPickersScript}
|]
    where
        userRow user =
            let current = lookup (get #id user) currentRoles
            in [hsx|
                <div class="input-group input-group-sm mb-1" data-member-row="" data-email={user.email}>
                    <span class="input-group-text" style="min-width: 220px">{user.email}</span>
                    <select name={"member-" <> tshow (get #id user)} class="form-select" data-testid={"member-" <> user.email}>
                        <option value="" selected={isNothing current}>—</option>
                        <option value="member" selected={current == Just "member"}>member</option>
                        <option value="lead" selected={current == Just "lead"}>lead</option>
                    </select>
                    <button type="button" class="btn btn-outline-danger" data-member-remove="" data-testid={"remove-" <> user.email}>×</button>
                </div>
            |]

teamPickersScript :: Html
teamPickersScript = [hsx|
    <script>
        // Team form pickers: filterable host-group multi-select + member
        // list showing only members, with filter-to-add and remove button.
        (function () {
            var init = function () {
                var hgFilter = document.querySelector('[data-hg-filter]');
                var hgSelect = document.querySelector('[data-hg-select]');
                if (hgFilter && hgSelect && !hgFilter.dataset.init) {
                    hgFilter.dataset.init = '1';
                    hgFilter.addEventListener('input', function () {
                        var q = hgFilter.value.toLowerCase();
                        Array.prototype.forEach.call(hgSelect.options, function (option) {
                            option.hidden = option.text.toLowerCase().indexOf(q) === -1;
                        });
                    });
                }

                var membersFilter = document.querySelector('[data-member-filter]');
                if (!membersFilter || membersFilter.dataset.init) return;
                membersFilter.dataset.init = '1';
                var rows = document.querySelectorAll('[data-member-row]');
                var applyVisibility = function () {
                    var q = membersFilter.value.toLowerCase();
                    Array.prototype.forEach.call(rows, function (row) {
                        var isMember = row.querySelector('select').value !== '';
                        var matches = row.getAttribute('data-email').toLowerCase().indexOf(q) !== -1;
                        row.style.display = (isMember || (q !== '' && matches)) ? '' : 'none';
                    });
                };
                membersFilter.addEventListener('input', applyVisibility);
                Array.prototype.forEach.call(rows, function (row) {
                    row.querySelector('select').addEventListener('change', applyVisibility);
                    row.querySelector('[data-member-remove]').addEventListener('click', function () {
                        row.querySelector('select').value = '';
                        applyVisibility();
                    });
                });
                applyVisibility();
            };
            document.addEventListener('DOMContentLoaded', init);
            document.addEventListener('turbolinks:load', init);
            init();
        })();
    </script>
|]
