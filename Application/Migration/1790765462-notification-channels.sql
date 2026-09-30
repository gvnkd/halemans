-- Notification channels become first-class config rows (like sources):
-- notification_rules.channel now REFERENCES the channel NAME (natural key,
-- existence validated in code like other name-based resolution). A channel
-- row carries the delivery type (browser_push|email|mattermost), the server
-- base URL (mattermost) and a config JSONB whose tokenEnv names the env var
-- holding the secret — exactly the zabbix/grafana source token pattern.
CREATE TABLE notification_channels (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    name TEXT NOT NULL,
    type TEXT NOT NULL,
    base_url TEXT NOT NULL DEFAULT '',
    config JSONB NOT NULL DEFAULT '{}',
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX notification_channels_name_idx ON notification_channels(name);

-- Backfill: every distinct rule channel value becomes a channel row (type =
-- the legacy channel value). Fresh databases get the same rows from
-- Application/Fixtures.sql.
INSERT INTO notification_channels (name, type, base_url, config, enabled)
SELECT DISTINCT channel, channel, '', '{}'::jsonb, true
FROM notification_rules
WHERE channel <> ''
ON CONFLICT (name) DO NOTHING;
