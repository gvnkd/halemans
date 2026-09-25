module Application.Pipeline.Blackouts (
    BlackoutSubject (..),
    alertSubject,
    blackoutWindowActive,
    blackoutApplies,
) where

import Application.Pipeline.Grouping (globMatch)
import Generated.Types
import IHP.Prelude

-- | The inventory refs AND raw names of one alert. Blackouts match by ref
-- (exact inventory row) or by glob (raw ingest name, so a glob covers
-- inventory rows that appear after the blackout was created).
data BlackoutSubject = BlackoutSubject
    { subjectEnvironmentId :: Maybe (Id Environment)
    , subjectEnvironmentName :: Maybe Text
    , subjectHostId :: Maybe (Id Host)
    , subjectHostName :: Maybe Text
    , subjectServiceId :: Maybe (Id Service)
    , subjectServiceName :: Maybe Text
    }

alertSubject :: Alert -> BlackoutSubject
alertSubject alert =
    BlackoutSubject
        { subjectEnvironmentId = alert.environmentId
        , subjectEnvironmentName = alert.env
        , subjectHostId = alert.hostId
        , subjectHostName = alert.host
        , subjectServiceId = alert.serviceId
        , subjectServiceName = alert.service
        }

-- | One-shot windows only (design_docs/01_highlevel.md §18).
blackoutWindowActive :: UTCTime -> Blackout -> Bool
blackoutWindowActive now blackout =
    blackout.startsAt <= now && now < blackout.endsAt

-- | Does the blackout cover an alert carrying these inventory refs/names?
-- Every stored scope leg must match (AND): env+host means "this host in this
-- environment". A scopeless blackout matches nothing.
blackoutApplies :: UTCTime -> BlackoutSubject -> Blackout -> Bool
blackoutApplies now subject blackout =
    blackoutWindowActive now blackout && not (null legs) && and legs
  where
    legs =
        catMaybes
            [ idLeg blackout.environmentId subject.subjectEnvironmentId
            , idLeg blackout.hostId subject.subjectHostId
            , idLeg blackout.serviceId subject.subjectServiceId
            , globLeg blackout.environmentGlob subject.subjectEnvironmentName
            , globLeg blackout.hostGlob subject.subjectHostName
            , globLeg blackout.serviceGlob subject.subjectServiceName
            ]
    idLeg (Just wanted) actual = Just (actual == Just wanted)
    idLeg Nothing _ = Nothing
    globLeg (Just pat) actual = Just (maybe False (globMatch pat) actual)
    globLeg Nothing _ = Nothing
