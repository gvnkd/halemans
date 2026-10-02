module Application.Service.Expose (
    exposeAlert,
) where

import Application.Helper.Ingest (publishAlertUpdate)
import Application.Service.Notify (dispatchNotification)
import Control.Monad (void)
import Generated.Types
import IHP.Fetch (fetch)
import IHP.ModelSupport
import IHP.Prelude
import IHP.TypedSql (sqlQueryTyped, typedSql)

-- Staged alert pipeline, Expose stage: notification dispatch (push,
-- mattermost, escalation trackers) + the "created" websocket fan-out. Runs
-- at the end of the Enrich stage (EnrichAlertJob completion) so every
-- channel renders the fully resolved alert (assets/CMDB/facets); the
-- expose_alert_jobs deadline (ingest enqueues it, see
-- Application.Helper.Ingest) guarantees exposition even when enrichment
-- hard-fails.

-- | Claim + run the Expose stage for an alert. Idempotent: the first caller
-- (enrichment completion or the deadline job) wins the exposed_at claim, so
-- duplicate triggers never double-notify. A suppressed or already
-- resolved/closed alert still fans out (the row/banner appear) but pages
-- nobody.
exposeAlert :: (?modelContext :: ModelContext) => Id Alert -> IO ()
exposeAlert alertId = do
    claimed <- claimExposition alertId
    when claimed do
        alert <- fetch alertId
        unless (alert.suppressed || alert.status `elem` (["resolved", "closed"] :: [Text])) do
            void (dispatchNotification alert)
        publishAlertUpdate alert "created"

-- Single-statement claim: concurrent exposers race on the UPDATE, exactly
-- one sees a row come back.
claimExposition :: (?modelContext :: ModelContext) => Id Alert -> IO Bool
claimExposition alertId = do
    rows <-
        sqlQueryTyped
            [typedSql|
        UPDATE alerts SET exposed_at = NOW()
        WHERE id = ${alertId} AND exposed_at IS NULL
        RETURNING id
    |]
    pure (not (null rows))
