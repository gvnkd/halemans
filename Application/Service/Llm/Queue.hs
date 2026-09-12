module Application.Service.Llm.Queue (latestJobErrors) where

import Data.List (nubBy)
import Generated.Types
import IHP.Fetch (fetch)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

latestJobErrors :: (?modelContext :: ModelContext) => [Id LlmAnalysis] -> IO [(Id LlmAnalysis, Text)]
latestJobErrors [] = pure []
latestJobErrors analysisIds = do
    jobs <-
        query @LlmAnalysisJob
            |> filterWhereIn (#analysisId, analysisIds)
            |> orderByDesc #createdAt
            |> fetch
    let withErrors = filter (isJust . (.lastError)) jobs
        latest = nubBy (\a b -> a.analysisId == b.analysisId) withErrors
    pure (map (\job -> (job.analysisId, fromMaybe "" job.lastError)) latest)
