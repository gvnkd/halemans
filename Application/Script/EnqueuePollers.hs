module Application.Script.EnqueuePollers where

import Application.Script.Prelude

-- Enqueues the self-rescheduling polling/maintenance loops. Safe to run
-- repeatedly: the jobs drop duplicate pending siblings on each run.
run :: Script
run = do
    _ <- newRecord @PollZabbixJob |> createRecord
    _ <- newRecord @AutoCloseJob |> createRecord
    _ <- newRecord @PollGrafanaJob |> createRecord
    _ <- newRecord @EscalationJob |> createRecord
    _ <- newRecord @JiraSyncJob |> createRecord
    _ <- newRecord @RetentionJob |> createRecord
    _ <- newRecord @SourceHealthJob |> createRecord
    pure ()
