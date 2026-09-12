module Web.View.Blackouts.Form (blackoutFormFields) where
import Web.View.Prelude
import Data.Time.Format (formatTime, defaultTimeLocale)

-- Shared new/edit fields for blackouts (milestone 12 §3). The scope-type /
-- scope-id filtering script lives in static/app.js keyed on the
-- blackout-scope-type / blackout-scope-id testids (auto-selects the first
-- visible option when the type changes).
blackoutFormFields :: Maybe Blackout -> [Environment] -> [Host] -> [Service] -> Html
blackoutFormFields blackout environments hosts services = [hsx|
    <div class="mb-3">
        <label class="form-label">Scope type</label>
        <select name="scopeType" class="form-select" data-testid="blackout-scope-type">
            <option value="environment" selected={scopeIs (.environmentId)}>environment</option>
            <option value="host" selected={scopeIs (.hostId)}>host</option>
            <option value="service" selected={scopeIs (.serviceId)}>service</option>
        </select>
    </div>
    <div class="mb-3" data-scope="environment">
        <label class="form-label">Scope</label>
        <select name="scopeId" class="form-select" data-testid="blackout-scope-id">
            {forEach environments environmentOption}
            {forEach hosts hostOption}
            {forEach services serviceOption}
        </select>
    </div>
    <div class="mb-3">
        <label class="form-label">Starts at (UTC, ISO 8601)</label>
        <input name="startsAt" type="text" class="form-control" value={startsAtValue} placeholder="2026-09-04T10:00:00Z" data-testid="blackout-starts-at" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Ends at (UTC, ISO 8601)</label>
        <input name="endsAt" type="text" class="form-control" value={endsAtValue} placeholder="2026-09-04T12:00:00Z" data-testid="blackout-ends-at" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Reason</label>
        <input name="reason" type="text" class="form-control" value={reasonValue} data-testid="blackout-reason"/>
    </div>
|]
    where
        scopeIs :: (Blackout -> Maybe (Id' table)) -> Bool
        scopeIs getter = maybe False (isJust . getter) blackout
        selectedId :: (Blackout -> Maybe (Id' table)) -> Maybe (Id' table)
        selectedId getter = maybe Nothing getter blackout
        startsAtValue = maybe "" (isoUtc . (.startsAt)) blackout
        endsAtValue = maybe "" (isoUtc . (.endsAt)) blackout
        reasonValue = maybe "" (.reason) blackout
        environmentOption environment = [hsx|
            <option value={scopeValue "environment" environment.id} selected={selectedId (.environmentId) == Just environment.id}>{environment.name}</option>
        |]
        hostOption host = [hsx|
            <option value={scopeValue "host" host.id} selected={selectedId (.hostId) == Just host.id}>{host.fqdn}</option>
        |]
        serviceOption service = [hsx|
            <option value={scopeValue "service" service.id} selected={selectedId (.serviceId) == Just service.id}>{service.name}</option>
        |]

-- The option value carries the scope type prefix; the controller strips it.
scopeValue :: Show (PrimaryKey table) => Text -> Id' table -> Text
scopeValue prefix id = prefix <> ":" <> tshow id

-- Matches the format param @UTCTime parses.
isoUtc :: UTCTime -> Text
isoUtc time = cs (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time)
