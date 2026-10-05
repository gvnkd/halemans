module Application.Pipeline.Blackouts (
    BlackoutSubject (..),
    alertSubject,
    blackoutWindowActive,
    blackoutApplies,
    openEndedBlackoutEndsAt,
    blackoutEndsLabel,
) where

import Application.Pipeline.Grouping (globMatch)
import Data.Time.Calendar (fromGregorian)
import Generated.Types
import IHP.Prelude

-- | Stored ends_at for open-ended blackouts (a null/empty endsAt in the
-- agent or web API). The column stays NOT NULL so every window comparison
-- and the (starts_at, ends_at) provision identity keep working; this value
-- never passes `now < endsAt` in practice. All display paths render it via
-- 'blackoutEndsLabel' as "forever".
openEndedBlackoutEndsAt :: UTCTime
openEndedBlackoutEndsAt = UTCTime (fromGregorian 9999 12 31) 0

-- | Human-facing end of a blackout window.
blackoutEndsLabel :: Blackout -> Text
blackoutEndsLabel blackout
    | blackout.endsAt == openEndedBlackoutEndsAt = "forever"
    | otherwise = tshow blackout.endsAt

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
    , subjectTitle :: Maybe Text
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
        , subjectTitle = Just alert.title
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
            , globLeg blackout.titleGlob subject.subjectTitle
            ]
    idLeg (Just wanted) actual = Just (actual == Just wanted)
    idLeg Nothing _ = Nothing
    globLeg (Just pat) actual = Just (maybe False (globMatch pat) actual)
    globLeg Nothing _ = Nothing
