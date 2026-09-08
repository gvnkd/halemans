module Application.Service.Llm.Queue (latestJobErrors) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch)
import Generated.Types
import Data.List (nubBy)

latestJobErrors :: (?modelContext :: ModelContext) => [Id LlmAnalysis] -> IO [(Id LlmAnalysis, Text)]
latestJobErrors [] = pure []
latestJobErrors analysisIds = do
    jobs <- query @LlmAnalysisJob
        |> filterWhereIn (#analysisId, analysisIds)
        |> orderByDesc #createdAt
        |> fetch
    let withErrors = filter (isJust . (.lastError)) jobs
        latest = nubBy (\a b -> a.analysisId == b.analysisId) withErrors
    pure (map (\job -> (job.analysisId, fromMaybe "" job.lastError)) latest)
