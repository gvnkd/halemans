-- Mattermost channel banner statistics.
-- mattermost_jobs gains kind 'banner': banner jobs have no alert, so
-- alert_id becomes nullable and the new channel column carries the
-- notification_channels row NAME the banner belongs to.
-- alert_stats_snapshots stores one counts document per banner refresh —
-- the history the per-severity trend arrows compare against.
ALTER TABLE mattermost_jobs ALTER COLUMN alert_id DROP NOT NULL;
ALTER TABLE mattermost_jobs ADD COLUMN channel TEXT DEFAULT NULL;

CREATE TABLE alert_stats_snapshots (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    channel TEXT NOT NULL,
    counts JSONB NOT NULL DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX alert_stats_snapshots_channel_created_idx ON alert_stats_snapshots(channel, created_at);
