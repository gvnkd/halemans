{-# LANGUAGE OverloadedStrings #-}
module Application.Service.Flapping
( EdgeKind (..)
, FlapEdge (..)
, FlapParams (..)
, defaultFlapParams
, FlapSubject (..)
, FlapReport (..)
, detectFlapping
) where

import IHP.Prelude
import Generated.Types ()
import IHP.ModelSupport (Id')
import Control.Monad (guard)
import Data.List (sort, sortOn, groupBy, foldl')
import Data.Ord (Down (..))

-- Flapping alerts detector (design_docs/milestone_11.md): pure fold over a
-- per-fingerprint timeline of loud/quiet edges. Loud = firing (row start,
-- refire from resolved/stalled); quiet = resolved or stalled. A flap is a
-- quiet -> loud gap of at most maxGapSeconds; episodes with >= minFlaps
-- flaps mark the fingerprint as flapping.

data EdgeKind = Loud | Quiet
    deriving (Eq, Show)

data FlapEdge = FlapEdge
    { edgeAt :: UTCTime
    , edgeKind :: EdgeKind
    } deriving (Eq, Show)

data FlapParams = FlapParams
    { minFlaps :: Int
    , maxGapSeconds :: Int
    , windowSeconds :: Int
    } deriving (Eq, Show)

defaultFlapParams :: FlapParams
defaultFlapParams = FlapParams { minFlaps = 3, maxGapSeconds = 1800, windowSeconds = 86400 }

-- Alert metadata carried through the analysis so the report rows are
-- self-contained for the admin view.
data FlapSubject = FlapSubject
    { fingerprint :: Text
    , latestAlertId :: Id' "alerts"
    , title :: Text
    , severity :: Text
    , effectiveEnv :: Maybe Text
    , host :: Maybe Text
    , sourceName :: Maybe Text
    } deriving (Eq, Show)

data FlapReport = FlapReport
    { subject :: FlapSubject
    , flapCount :: Int
    , flapRatePerHour :: Double
    , medianGapSeconds :: Double
    , p90GapSeconds :: Double
    , minGapSeconds :: Double
    , maxGapSecondsObs :: Double
    , mttrSeconds :: Maybe Double
    , activeFrom :: UTCTime
    , activeTo :: UTCTime
    , lastFlapAt :: UTCTime
    } deriving (Eq, Show)

-- | One flapping episode: the loud edge that opened it (Nothing when the
-- timeline starts mid-quiet) plus the (quiet, refire) flap pairs.
type Episode = (Maybe UTCTime, [(UTCTime, UTCTime)])

detectFlapping :: FlapParams -> [(FlapSubject, [FlapEdge])] -> [FlapReport]
detectFlapping params subjects =
    sortOn (Down . flapCount) (mapMaybe (reportFor params) subjects)

reportFor :: FlapParams -> (FlapSubject, [FlapEdge]) -> Maybe FlapReport
reportFor params (subject, edges) = do
    let qualifying = filter ((>= params.minFlaps) . length . snd) (episodes params edges)
    guard (not (null qualifying))
    let pairs = concatMap snd qualifying
        gaps = map (uncurry diffSeconds) pairs
        sortedGaps = sort gaps
        loudDurations = concatMap episodeLoudDurations qualifying
        total = length pairs
        -- Episode start: the leading loud edge, or the first quiet edge
        -- when the window opens mid-quiet.
        episodeStart (leading, episodePairs) = case (leading, episodePairs) of
            (Just at, _) -> at
            (Nothing, (quietAt, _) : _) -> quietAt
            (Nothing, []) -> lastFlap
        firstLoud = minimum (map episodeStart qualifying)
        lastFlap = maximum (map snd pairs)
    pure FlapReport
        { subject
        , flapCount = total
        , flapRatePerHour = fromIntegral total * 3600 / fromIntegral params.windowSeconds
        , medianGapSeconds = percentile 0.5 sortedGaps
        , p90GapSeconds = percentile 0.9 sortedGaps
        , minGapSeconds = minimum gaps
        , maxGapSecondsObs = maximum gaps
        , mttrSeconds = if null loudDurations then Nothing else Just (mean loudDurations)
        , activeFrom = firstLoud
        , activeTo = lastFlap
        , lastFlapAt = lastFlap
        }

-- | Collapse consecutive same-kind edges (loud runs keep the earliest,
-- quiet runs keep the latest) so the timeline strictly alternates, then
-- collect runs of quiet -> loud pairs whose gap fits maxGapSeconds.
episodes :: FlapParams -> [FlapEdge] -> [Episode]
episodes params edges =
    let collapsed = collapseEdges edges
        indexed = zip [0 ..] collapsed
        maxGap = fromIntegral params.maxGapSeconds :: NominalDiffTime
        candidates =
            [ (i, quietAt, loudAt)
            | ((i, FlapEdge quietAt Quiet), (_, FlapEdge loudAt Loud)) <- zip indexed (drop 1 indexed)
            , diffUTCTime loudAt quietAt <= maxGap
            ]
    in map (toEpisode collapsed) (groupRuns candidates)
  where
    toEpisode collapsed run = case run of
        ((i, _, _) : _) | i >= 1 ->
            case collapsed !! (i - 1) of
                FlapEdge at Loud -> (Just at, [(q, l) | (_, q, l) <- run])
                _ -> (Nothing, [(q, l) | (_, q, l) <- run])
        _ -> (Nothing, [(q, l) | (_, q, l) <- run])

-- | Candidates form one episode while their quiet-edge indices are
-- adjacent (i, i+2, i+4 ...) — a gap above maxGapSeconds breaks the run.
groupRuns :: [(Int, UTCTime, UTCTime)] -> [[(Int, UTCTime, UTCTime)]]
groupRuns [] = []
groupRuns (first:rest) = go first [first] rest
  where
    go _ acc [] = [reverse acc]
    go (i, _, _) acc (candidate@(j, _, _) : more)
        | j == i + 2 = go candidate (candidate : acc) more
        | otherwise = reverse acc : go candidate [candidate] more

collapseEdges :: [FlapEdge] -> [FlapEdge]
collapseEdges = concatMap collapse . groupBy sameKind . sortOn edgeAt
  where
    sameKind a b = a.edgeKind == b.edgeKind
    collapse run = case run of
        (edge@(FlapEdge _ Loud) : _) -> [edge]
        (edge : rest) -> [foldl' (\_ later -> later) edge rest]
        [] -> []

episodeLoudDurations :: Episode -> [Double]
episodeLoudDurations (leading, pairs) =
    -- Pair each loud edge with the quiet that ends it: with a leading loud
    -- that's quiets from the start; without one the first quiet belongs to
    -- a loud period outside the window.
    let louds = maybeToList leading ++ map snd pairs
        quiets = case leading of
            Just _ -> map fst pairs
            Nothing -> drop 1 (map fst pairs)
    in [ diffSeconds loudAt quietAt | (loudAt, quietAt) <- zip louds quiets ]

diffSeconds :: UTCTime -> UTCTime -> Double
diffSeconds from to = realToFrac (diffUTCTime to from)

percentile :: Double -> [Double] -> Double
percentile p sorted =
    let n = length sorted
        rank = max 1 (ceiling (p * fromIntegral n))
    in sorted !! min (n - 1) (rank - 1)

mean :: [Double] -> Double
mean xs = foldl' (+) 0 xs / fromIntegral (length xs)
