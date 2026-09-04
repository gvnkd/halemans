module Application.Pipeline.Blackouts
( blackoutWindowActive
, blackoutApplies
) where

import IHP.Prelude
import Generated.Types

-- | One-shot windows only (design_docs/01_highlevel.md §18).
blackoutWindowActive :: UTCTime -> Blackout -> Bool
blackoutWindowActive now blackout =
    blackout.startsAt <= now && now < blackout.endsAt

-- | Does the blackout cover an alert carrying these inventory refs?
-- Scope is exactly one of environment/host/service (enforced at creation).
blackoutApplies
    :: UTCTime
    -> Maybe (Id Environment)
    -> Maybe (Id Host)
    -> Maybe (Id Service)
    -> Blackout
    -> Bool
blackoutApplies now environmentId hostId serviceId blackout =
    blackoutWindowActive now blackout && scopeMatches
    where
        scopeMatches = or
            [ isJust blackout.environmentId && blackout.environmentId == environmentId
            , isJust blackout.hostId && blackout.hostId == hostId
            , isJust blackout.serviceId && blackout.serviceId == serviceId
            ]
