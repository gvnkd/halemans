module Application.Service.Timeline (
    TimelineGroup (..),
    timelineHiddenKind,
    groupTimeline,
    headTimelineGroup,
) where

import qualified Data.List.NonEmpty as NE
import Generated.Types
import IHP.Prelude

-- Internal-error events stay in alert_events for the audit log and the API,
-- but they are not state changes: the alert card timeline hides them and
-- folds consecutive repeats of the same kind into one aggregated entry.
timelineHiddenKind :: Text -> Bool
timelineHiddenKind kind =
    kind
        `elem` [ "enrichment_failed"
               , "writeback_failed"
               , "llm_failed"
               , "llm_skipped"
               ]

-- | One rendered timeline entry: a run of consecutive same-kind events.
-- tgAnchor is the OLDEST event of the run (stable dom id for live updates),
-- tgLatest the newest (timestamp/summary shown).
data TimelineGroup = TimelineGroup
    { tgAnchor :: AlertEvent
    , tgLatest :: AlertEvent
    , tgCount :: Int
    }

visible :: [AlertEvent] -> [AlertEvent]
visible = filter (not . timelineHiddenKind . get #kind)

toGroup :: NE.NonEmpty AlertEvent -> TimelineGroup
toGroup run =
    TimelineGroup
        { tgAnchor = NE.head run
        , tgLatest = NE.last run
        , tgCount = length run
        }

-- | Events in ascending created_at order (page render).
groupTimeline :: [AlertEvent] -> [TimelineGroup]
groupTimeline events =
    NE.groupBy (\a b -> get #kind a == get #kind b) (visible events)
        |> map toGroup

-- | Events in DESCENDING created_at order (live broadcaster): aggregates
-- only the current leading run.
headTimelineGroup :: [AlertEvent] -> Maybe TimelineGroup
headTimelineGroup eventsDesc =
    case visible eventsDesc of
        [] -> Nothing
        (latest : rest) ->
            let run = latest NE.:| takeWhile (\event -> get #kind event == get #kind latest) rest
             in Just (toGroup (NE.reverse run))
