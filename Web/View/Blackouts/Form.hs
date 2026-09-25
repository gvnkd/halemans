module Web.View.Blackouts.Form (blackoutFormFields) where

import Data.Time.Format (defaultTimeLocale, formatTime)
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Prelude

-- Shared new/edit fields for blackouts (milestone 12 §3). The scope-type /
-- scope-id filtering script lives in static/app.js keyed on the
-- blackout-scope-type / blackout-scope-id testids (auto-selects the first
-- visible option when the type changes; the "pattern" type swaps the entity
-- picker for the glob inputs and disables whichever group is hidden so only
-- one kind is submitted).
blackoutFormFields :: (CurrentUserRecord ~ User, ?request :: Request) => Maybe Blackout -> [Environment] -> [Host] -> [Service] -> Html
blackoutFormFields blackout environments hosts services =
    [hsx|
    <div class="mb-3">
        <label class="form-label">{tr "Scope type"}</label>
        <select name="scopeType" class="select" data-testid="blackout-scope-type">
            <option value="environment" selected={scopeIs (.environmentId)}>environment</option>
            <option value="host" selected={scopeIs (.hostId)}>host</option>
            <option value="service" selected={scopeIs (.serviceId)}>service</option>
            <option value="pattern" selected={scopeIsGlobs}>{tr "pattern"}</option>
        </select>
    </div>
    <div class="mb-3" data-blackout-scope="entity" hidden={scopeIsGlobs}>
        <label class="form-label">{tr "Scope"}</label>
        <select name="scopeId" class="select" data-testid="blackout-scope-id">
            {forEach environments environmentOption}
            {forEach hosts hostOption}
            {forEach services serviceOption}
        </select>
    </div>
    <div class="mb-3" data-blackout-scope="pattern" hidden={not scopeIsGlobs}>
        <label class="form-label">{tr "Name patterns (shell globs: * and ?)"}</label>
        <input name="envGlob" type="text" class="form-control mb-2" value={globValue (.environmentGlob)} placeholder={tr "Environment glob (optional)"} data-testid="blackout-env-glob" disabled={not scopeIsGlobs}/>
        <input name="hostGlob" type="text" class="form-control mb-2" value={globValue (.hostGlob)} placeholder={tr "Host glob (optional)"} data-testid="blackout-host-glob" disabled={not scopeIsGlobs}/>
        <input name="serviceGlob" type="text" class="form-control" value={globValue (.serviceGlob)} placeholder={tr "Service glob (optional)"} data-testid="blackout-service-glob" disabled={not scopeIsGlobs}/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Starts at (UTC, ISO 8601)"}</label>
        <input name="startsAt" type="text" class="form-control" value={startsAtValue} placeholder="2026-09-04T10:00:00Z" data-testid="blackout-starts-at" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Ends at (UTC, ISO 8601)"}</label>
        <input name="endsAt" type="text" class="form-control" value={endsAtValue} placeholder="2026-09-04T12:00:00Z" data-testid="blackout-ends-at" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Reason"}</label>
        <input name="reason" type="text" class="form-control" value={reasonValue} data-testid="blackout-reason"/>
    </div>
|]
  where
    scopeIs :: (Blackout -> Maybe (Id' table)) -> Bool
    scopeIs getter = maybe False (isJust . getter) blackout
    scopeIsGlobs = maybe False (\b -> isJust b.environmentGlob || isJust b.hostGlob || isJust b.serviceGlob) blackout
    globValue getter = maybe "" (fromMaybe "" . getter) blackout
    selectedId :: (Blackout -> Maybe (Id' table)) -> Maybe (Id' table)
    selectedId getter = maybe Nothing getter blackout
    startsAtValue = maybe "" (isoUtc . (.startsAt)) blackout
    endsAtValue = maybe "" (isoUtc . (.endsAt)) blackout
    reasonValue = maybe "" (.reason) blackout
    environmentOption environment =
        [hsx|
            <option value={scopeValue "environment" environment.id} selected={selectedId (.environmentId) == Just environment.id}>{environment.name}</option>
        |]
    hostOption host =
        [hsx|
            <option value={scopeValue "host" host.id} selected={selectedId (.hostId) == Just host.id}>{host.fqdn}</option>
        |]
    serviceOption service =
        [hsx|
            <option value={scopeValue "service" service.id} selected={selectedId (.serviceId) == Just service.id}>{service.name}</option>
        |]

-- The option value carries the scope type prefix; the controller strips it.
scopeValue :: (Show (PrimaryKey table)) => Text -> Id' table -> Text
scopeValue prefix id = prefix <> ":" <> tshow id

-- Matches the format param @UTCTime parses.
isoUtc :: UTCTime -> Text
isoUtc time = cs (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time)
