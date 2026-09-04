module Web.View.Blackouts.New where
import Web.View.Prelude

data NewView = NewView
    { environments :: [Environment]
    , hosts :: [Host]
    , services :: [Service]
    }

instance View NewView where
    html NewView { .. } = [hsx|
        <h1>New blackout</h1>
        <form method="POST" action={CreateBlackoutAction} data-testid="blackout-form" style="max-width: 500px">
            <div class="mb-3">
                <label class="form-label">Scope type</label>
                <select name="scopeType" class="form-select" data-testid="blackout-scope-type">
                    <option value="environment">environment</option>
                    <option value="host">host</option>
                    <option value="service">service</option>
                </select>
            </div>
            <div class="mb-3" data-scope="environment">
                <label class="form-label">Environment</label>
                <select name="scopeId" class="form-select" data-testid="blackout-scope-id">
                    {forEach environments environmentOption}
                    {forEach hosts hostOption}
                    {forEach services serviceOption}
                </select>
            </div>
            <div class="mb-3">
                <label class="form-label">Starts at (UTC, ISO 8601)</label>
                <input name="startsAt" type="text" class="form-control" placeholder="2026-09-04T10:00:00Z" data-testid="blackout-starts-at" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Ends at (UTC, ISO 8601)</label>
                <input name="endsAt" type="text" class="form-control" placeholder="2026-09-04T12:00:00Z" data-testid="blackout-ends-at" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Reason</label>
                <input name="reason" type="text" class="form-control" data-testid="blackout-reason"/>
            </div>
            <button type="submit" class="btn btn-primary" data-testid="blackout-submit">Create</button>
        </form>
        <script>
            // Filter scope options by the selected scope type.
            document.addEventListener('DOMContentLoaded', function () {
                var typeSelect = document.querySelector('[data-testid="blackout-scope-type"]');
                var idSelect = document.querySelector('[data-testid="blackout-scope-id"]');
                if (!typeSelect || !idSelect) return;
                var applyFilter = function () {
                    var prefix = typeSelect.value + ':';
                    Array.prototype.forEach.call(idSelect.options, function (option) {
                        var visible = option.value.indexOf(prefix) === 0;
                        option.hidden = !visible;
                        option.disabled = !visible;
                    });
                    var first = Array.prototype.find.call(idSelect.options, function (o) { return !o.disabled; });
                    if (first) idSelect.value = first.value;
                };
                typeSelect.addEventListener('change', applyFilter);
                applyFilter();
            });
        </script>
    |]
        where
            environmentOption environment = [hsx|
                <option value={scopeValue "environment" environment.id}>{environment.name}</option>
            |]
            hostOption host = [hsx|
                <option value={scopeValue "host" host.id}>{host.fqdn}</option>
            |]
            serviceOption service = [hsx|
                <option value={scopeValue "service" service.id}>{service.name}</option>
            |]

-- The option value carries the scope type prefix; the controller strips it.
scopeValue :: Show (PrimaryKey table) => Text -> Id' table -> Text
scopeValue prefix id = prefix <> ":" <> tshow id
