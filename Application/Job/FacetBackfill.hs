module Application.Job.FacetBackfill where

import qualified Application.Service.Facets as Facets
import Control.Monad (void)
import Generated.Types
import IHP.Fetch (fetch)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlQueryTyped, typedSql)

-- Bounded facet backfill (design_docs/milestone_9.md §3): mapping edits do
-- not retro-update alerts; the admin "recompute facets" button enqueues this
-- job, which re-resolves facets over non-closed alerts in chunks, chaining
-- follow-up jobs via the cursor column until a short page ends the run.
instance Job FacetBackfillJob where
    perform job = do
        chunk <- case job.cursor of
            Nothing ->
                sqlQueryTyped
                    [typedSql|
                SELECT id FROM alerts
                WHERE status <> 'closed'
                ORDER BY id
                LIMIT ${chunkSize}
            |]
            Just cursor ->
                sqlQueryTyped
                    [typedSql|
                SELECT id FROM alerts
                WHERE status <> 'closed' AND id > ${cursor}
                ORDER BY id
                LIMIT ${chunkSize}
            |]
        forM_ chunk \alertId -> do
            alert <- fetch alertId
            void (Facets.materializeFacets alert)
        when (fromIntegral (length chunk) == chunkSize) do
            case reverse chunk of
                (lastId : _) -> void do
                    newRecord @FacetBackfillJob
                        |> set #cursor (Just (unpackId lastId))
                        |> createRecord
                [] -> pure ()

    maxAttempts = 3
    queuePollInterval = 5 * 1000000

chunkSize :: Int64
chunkSize = 500
