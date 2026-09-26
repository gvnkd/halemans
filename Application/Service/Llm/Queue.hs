module Application.Service.Llm.Queue (latestJobErrors, ensureLanguageVariant, pickPreferredAnalysis) where

import Control.Monad (void)
import Data.List (nubBy)
import Generated.Types
import IHP.Fetch (fetch)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlQueryTyped, typedSql)

import Application.Service.Llm.AutoAnalyze (autoAnalyzeAllowed)

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

-- Per-language analysis variants: an alert card shows the latest terminal
-- analysis in the VIEWER's language. When the alert is already LLM-covered
-- (a queued/running/done row exists in some language) but has no row at all
-- in the viewer's language, queue one on demand. Existence is checked across
-- all statuses — a failed variant blocks lazy retries (manual Re-analyze
-- covers re-attempts), and alerts with no analysis in any language are never
-- generated for (the auto-analysis gate decides coverage, not page views).
ensureLanguageVariant :: (?modelContext :: ModelContext) => Alert -> Text -> IO ()
ensureLanguageVariant alert languageCode = do
    let alertId = get #id alert
    rows <-
        sqlQueryTyped
            [typedSql|
        SELECT language, status FROM llm_analyses
        WHERE alert_id = ${alertId}
    |]
    let covered = any (\row -> get #status row `elem` ["queued", "running", "done"] :: Bool) rows
        variantExists = any (\row -> get #language row == Just languageCode) rows
    when (covered && not variantExists) do
        allowed <- autoAnalyzeAllowed alert
        when allowed do
            void do
                analysis <-
                    newRecord @LlmAnalysis
                        |> set #alertId alertId
                        |> set #language (Just languageCode)
                        |> createRecord
                void do
                    newRecord @LlmAnalysisJob
                        |> set #analysisId (get #id analysis)
                        |> createRecord

-- Which analysis row the alert card shows for a viewer language code. A
-- terminal (done/failed) row in the viewer's language beats a newer one in
-- another language; a newest row whose job died surfaces its error as
-- before; NULL-language legacy rows only win when nothing matches. Pending
-- rows never hide a terminal result (the caller renders the pending note).
pickPreferredAnalysis :: Text -> [(Id LlmAnalysis, Text)] -> [LlmAnalysis] -> LlmAnalysis
pickPreferredAnalysis languageCode jobErrors = \case
    [] -> error "pickPreferredAnalysis: no analyses"
    allRows@(newest : _)
        | matches newest && newest.status `elem` ["done", "failed"] -> newest
        | isJust (lookup (get #id newest) jobErrors) -> newest
        | otherwise -> case [a | a <- allRows, matches a, a.status `elem` ["done", "failed"]] of
            (terminal : _) -> terminal
            [] -> case [a | a <- allRows, a.status `elem` ["done", "failed"]] of
                (terminal : _) -> terminal
                [] -> newest
  where
    matches a = a.language == Just languageCode
