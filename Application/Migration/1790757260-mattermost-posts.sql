-- Mattermost notification channel (phase 1). mattermost_posts maps an alert
-- to its Mattermost root post so alert status transitions can edit the root
-- message in place and the Ack button can find its alert back. One row per
-- (alert, notification rule): two rules may post the same alert into two
-- different channels.
CREATE TABLE mattermost_posts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    alert_id UUID NOT NULL,
    notification_rule_id UUID DEFAULT NULL,
    root_post_id TEXT NOT NULL,
    channel_id TEXT NOT NULL,
    rendered_status TEXT NOT NULL DEFAULT '',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE mattermost_posts ADD CONSTRAINT mattermost_posts_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
ALTER TABLE mattermost_posts ADD CONSTRAINT mattermost_posts_notification_rule_id_fkey FOREIGN KEY (notification_rule_id) REFERENCES notification_rules (id);
CREATE INDEX mattermost_posts_alert_id_idx ON mattermost_posts(alert_id);

-- Outbound delivery queue (the worker owns the HTTP, ingest only enqueues):
-- kind 'notify' creates the root post plus the details reply in its thread;
-- kind 'sync' re-renders and patches the root posts the alert already has.
CREATE TABLE mattermost_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    alert_id UUID NOT NULL,
    rule_id UUID DEFAULT NULL,
    kind TEXT NOT NULL DEFAULT 'sync',
    event_kind TEXT DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE mattermost_jobs ADD CONSTRAINT mattermost_jobs_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
ALTER TABLE mattermost_jobs ADD CONSTRAINT mattermost_jobs_rule_id_fkey FOREIGN KEY (rule_id) REFERENCES notification_rules (id);
