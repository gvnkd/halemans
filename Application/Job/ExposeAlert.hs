module Application.Job.ExposeAlert where

import Application.Service.Expose (exposeAlert)
import Generated.Types
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Prelude

-- Deadline exposer (staged pipeline): ingest enqueues this at
-- +exposeDeadlineSeconds so the Expose stage runs even when EnrichAlertJob
-- hard-fails. exposeAlert is claim-gated, so a no-op when enrichment already
-- exposed the alert.
instance Job ExposeAlertJob where
    perform job = exposeAlert job.alertId

    maxAttempts = 3
