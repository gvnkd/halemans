module Application.Script.EnqueuePollers where

import Application.Script.Prelude

-- Enqueues the self-rescheduling zabbix polling loop. Safe to run repeatedly:
-- the job drops duplicate pending siblings on each run.
run :: Script
run = do
    _ <- newRecord @PollZabbixJob |> createRecord
    pure ()
