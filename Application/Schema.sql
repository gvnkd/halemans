-- Halemans schema. Milestone 0 carries only the minimal slice needed for the
-- smoke suite (doc §6); phase 1 extends this per design_docs/01_highlevel.md §3.

-- sources.type: zabbix | grafana | alertmanager | webhook.
-- sources.config: non-secret config; credentials are env-var references like {"tokenEnv":"ZABBIX_TOKEN"}.
CREATE TABLE sources (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    type TEXT NOT NULL,
    name TEXT NOT NULL,
    base_url TEXT NOT NULL DEFAULT '',
    env TEXT NOT NULL DEFAULT 'dev',
    poll_interval_seconds INT NOT NULL DEFAULT 30,
    enabled BOOLEAN NOT NULL DEFAULT true,
    config JSONB NOT NULL DEFAULT '{}',
    last_sync_cursor TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);

CREATE TABLE webhook_tokens (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    source_id UUID NOT NULL,
    token TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (token)
);
ALTER TABLE webhook_tokens ADD CONSTRAINT webhook_tokens_source_id_fkey FOREIGN KEY (source_id) REFERENCES sources (id);

CREATE TABLE raw_events (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    source_id UUID DEFAULT NULL,
    received_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    payload JSONB NOT NULL
);
ALTER TABLE raw_events ADD CONSTRAINT raw_events_source_id_fkey FOREIGN KEY (source_id) REFERENCES sources (id);

CREATE TABLE alerts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    fingerprint TEXT NOT NULL,
    source_id UUID DEFAULT NULL,
    external_id TEXT DEFAULT NULL,
    title TEXT NOT NULL,
    description TEXT NOT NULL DEFAULT '',
    severity TEXT NOT NULL DEFAULT 'warning',
    status TEXT NOT NULL DEFAULT 'firing',
    env TEXT DEFAULT NULL,
    host TEXT DEFAULT NULL,
    service TEXT DEFAULT NULL,
    check_name TEXT DEFAULT NULL,
    labels JSONB NOT NULL DEFAULT '{}',
    annotations JSONB NOT NULL DEFAULT '{}',
    source_url TEXT DEFAULT NULL,
    occurrences INT NOT NULL DEFAULT 1,
    started_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    first_seen_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    last_seen_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    resolved_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE alerts ADD CONSTRAINT alerts_source_id_fkey FOREIGN KEY (source_id) REFERENCES sources (id);
CREATE INDEX alerts_fingerprint_idx ON alerts(fingerprint);
CREATE INDEX alerts_status_idx ON alerts(status);

CREATE TABLE poll_zabbix_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
