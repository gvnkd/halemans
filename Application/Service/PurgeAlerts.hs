module Application.Service.PurgeAlerts (purgeAllAlerts) where

import Control.Monad (void)
import IHP.ModelSupport (ModelContext, withTransaction)
import IHP.Prelude
import IHP.TypedSql (sqlExecTyped, typedSql)

-- | Danger zone (admin UI): wipes every alert with all dependent rows plus
-- now-empty groups. FK order matters: every table with an alerts FK must be
-- cleared first. asset_alert_links (milestone 8) is the easy one to miss —
-- it 23503'd the purge when it was absent.
purgeAllAlerts :: (?modelContext :: ModelContext) => IO ()
purgeAllAlerts = withTransaction do
    void $ sqlExecTyped [typedSql| DELETE FROM asset_alert_links |]
    void $ sqlExecTyped [typedSql| DELETE FROM llm_feedback |]
    void $ sqlExecTyped [typedSql| DELETE FROM llm_analysis_jobs |]
    void $ sqlExecTyped [typedSql| DELETE FROM llm_analyses |]
    void $ sqlExecTyped [typedSql| DELETE FROM alert_events |]
    void $ sqlExecTyped [typedSql| DELETE FROM comments |]
    void $ sqlExecTyped [typedSql| DELETE FROM push_notification_jobs |]
    void $ sqlExecTyped [typedSql| DELETE FROM escalation_trackers |]
    void $ sqlExecTyped [typedSql| DELETE FROM jira_links |]
    void $ sqlExecTyped [typedSql| DELETE FROM write_back_jobs |]
    void $ sqlExecTyped [typedSql| DELETE FROM write_back_attempts |]
    void $ sqlExecTyped [typedSql| DELETE FROM enrich_alert_jobs |]
    void $ sqlExecTyped [typedSql| DELETE FROM alerts |]
    void $ sqlExecTyped [typedSql| DELETE FROM alert_groups |]
