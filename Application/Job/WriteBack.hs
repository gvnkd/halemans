module Application.Job.WriteBack where

import IHP.Prelude
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Fetch (fetch)
import Generated.Types
import Application.Service.WriteBack (executeAttempt)

-- Thin executor over write_back_attempts rows (design_docs/milestone_3.md
-- §13): the row survives worker restarts and drives the card's status chip;
-- retry/backoff is re-enqueue based, handled by executeAttempt.
instance Job WriteBackJob where
    perform job = do
        attempt <- fetch job.attemptId
        executeAttempt attempt

    maxAttempts = 3
