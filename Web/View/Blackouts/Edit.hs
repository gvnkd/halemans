module Web.View.Blackouts.Edit where
import Web.View.Prelude
import Data.Time.Format (formatTime, defaultTimeLocale)

data EditView = EditView
    { blackout :: Blackout
    , environments :: [Environment]
    , hosts :: [Host]
    , services :: [Service]
    }

instance View EditView where
    html EditView { .. } = [hsx|
        <h1>Edit blackout</h1>
        <form method="POST" action={UpdateBlackoutAction blackout.id} data-testid="blackout-edit-form" class="maxw-500">
            <div class="mb-3">
                <label class="form-label">Scope type</label>
                <select name="scopeType" class="form-select" data-testid="blackout-scope-type">
                    <option value="environment" selected={isJust blackout.environmentId}>environment</option>
                    <option value="host" selected={isJust blackout.hostId}>host</option>
                    <option value="service" selected={isJust blackout.serviceId}>service</option>
                </select>
            </div>
            <div class="mb-3" data-scope="environment">
                <label class="form-label">Scope</label>
                <select name="scopeId" class="form-select" data-testid="blackout-scope-id">
                    {forEach environments (environmentOption blackout)}
                    {forEach hosts (hostOption blackout)}
                    {forEach services (serviceOption blackout)}
                </select>
            </div>
            <div class="mb-3">
                <label class="form-label">Starts at (UTC, ISO 8601)</label>
                <input name="startsAt" type="text" class="form-control" value={isoUtc blackout.startsAt} data-testid="blackout-starts-at" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Ends at (UTC, ISO 8601)</label>
                <input name="endsAt" type="text" class="form-control" value={isoUtc blackout.endsAt} data-testid="blackout-ends-at" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Reason</label>
                <input name="reason" type="text" class="form-control" value={blackout.reason} data-testid="blackout-reason"/>
            </div>
            <button type="submit" class="btn btn-primary" data-testid="blackout-submit">Save</button>
        </form>
        <script>
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
                };
                typeSelect.addEventListener('change', applyFilter);
                applyFilter();
            });
        </script>
    |]

environmentOption :: Blackout -> Environment -> Html
environmentOption blackout environment = [hsx|
    <option value={scopeValue "environment" environment.id} selected={blackout.environmentId == Just environment.id}>{environment.name}</option>
|]

hostOption :: Blackout -> Host -> Html
hostOption blackout host = [hsx|
    <option value={scopeValue "host" host.id} selected={blackout.hostId == Just host.id}>{host.fqdn}</option>
|]

serviceOption :: Blackout -> Service -> Html
serviceOption blackout service = [hsx|
    <option value={scopeValue "service" service.id} selected={blackout.serviceId == Just service.id}>{service.name}</option>
|]

scopeValue :: Show (PrimaryKey table) => Text -> Id' table -> Text
scopeValue prefix id = prefix <> ":" <> tshow id

-- Matches the format param @UTCTime parses.
isoUtc :: UTCTime -> Text
isoUtc time = cs (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time)
