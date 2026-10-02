-- Staged alert pipeline: the Expose stage (notification dispatch + the
-- "created" websocket fan-out) is deferred until the Enrich stage completes.
-- exposed_at is the idempotency claim: the first exposer (EnrichAlertJob
-- completion or the expose_alert_jobs deadline) wins; a resolved/closed or
-- suppressed alert at exposition time pages nobody but still fans out.
ALTER TABLE alerts ADD COLUMN exposed_at TIMESTAMP WITH TIME ZONE DEFAULT NULL;

CREATE TABLE expose_alert_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    alert_id UUID NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE expose_alert_jobs ADD CONSTRAINT expose_alert_jobs_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
