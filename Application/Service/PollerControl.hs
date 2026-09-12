module Application.Service.PollerControl (ensurePollerForSourceType) where

import Control.Monad (void)
import Generated.Types ()
import IHP.ModelSupport
import IHP.Prelude
import IHP.TypedSql (sqlExecTyped, typedSql)

-- Re-arm a self-stopping poll loop (poll loops stop rescheduling when no
-- enabled sources of their type exist): insert a due job row when none is
-- pending, so a source created or enabled while its loop is stopped starts
-- polling immediately. The job queue's INSERT notify trigger wakes the
-- worker; concurrent duplicate inserts self-heal via the sibling cleanup in
-- each poller's perform.
ensurePollerForSourceType :: (?modelContext :: ModelContext) => Text -> IO ()
ensurePollerForSourceType sourceType = case sourceType of
    "zabbix" ->
        void $
            sqlExecTyped
                [typedSql|
        INSERT INTO poll_zabbix_jobs (run_at)
        SELECT now()
        WHERE NOT EXISTS (SELECT 1 FROM poll_zabbix_jobs WHERE status = 'job_status_not_started')
    |]
    "grafana" ->
        void $
            sqlExecTyped
                [typedSql|
        INSERT INTO poll_grafana_jobs (run_at)
        SELECT now()
        WHERE NOT EXISTS (SELECT 1 FROM poll_grafana_jobs WHERE status = 'job_status_not_started')
    |]
    _ -> pure ()
