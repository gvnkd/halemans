module Application.Job.JiraSync where

import Application.Service.Jira.DbConfig (syncOpenLinks)
import Generated.Types
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.TypedSql (sqlExecTyped, typedSql)

-- Periodic Jira status refresh (design_docs/milestone_3.md §5): every 5 min,
-- refresh summary/status of links whose alert is not closed.
-- Self-rescheduling like PollZabbixJob; seeded by EnqueuePollers.
instance Job JiraSyncJob where
    perform _job = do
        _ <- syncOpenLinks

        now <- getCurrentTime
        next <-
            newRecord @JiraSyncJob
                |> set #runAt (addUTCTime 300 now)
                |> createRecord
        let nextId = get #id next
        _ <-
            sqlExecTyped
                [typedSql|
            DELETE FROM jira_sync_jobs
            WHERE status = 'job_status_not_started' AND id <> ${nextId}
        |]
        pure ()

    queuePollInterval = 30 * 1000000
    maxAttempts = 3
