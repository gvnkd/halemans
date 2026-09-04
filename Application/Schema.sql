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
    environment_id UUID DEFAULT NULL,
    host_id UUID DEFAULT NULL,
    service_id UUID DEFAULT NULL,
    suppressed BOOLEAN NOT NULL DEFAULT false,
    acknowledged_by UUID DEFAULT NULL,
    acknowledged_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    ack_comment TEXT DEFAULT NULL,
    ack_expires_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    closed_by UUID DEFAULT NULL,
    closed_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    close_reason TEXT DEFAULT NULL,
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

-- Milestone 1 (phase 1) schema delta, per design_docs/milestone_1.md §2.

CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    display_name TEXT NOT NULL DEFAULT '',
    settings JSONB NOT NULL DEFAULT '{}',
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX users_email_idx ON users(email);

CREATE TABLE roles (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    privileges TEXT[] NOT NULL DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX roles_name_idx ON roles(name);

CREATE TABLE user_roles (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    role_id UUID NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (user_id, role_id)
);
ALTER TABLE user_roles ADD CONSTRAINT user_roles_user_id_fkey FOREIGN KEY (user_id) REFERENCES users (id);
ALTER TABLE user_roles ADD CONSTRAINT user_roles_role_id_fkey FOREIGN KEY (role_id) REFERENCES roles (id);

CREATE TABLE environments (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX environments_name_idx ON environments(name);

CREATE TABLE hosts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    fqdn TEXT NOT NULL,
    environment_id UUID DEFAULT NULL,
    labels JSONB NOT NULL DEFAULT '{}',
    auto_created BOOLEAN NOT NULL DEFAULT false,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE hosts ADD CONSTRAINT hosts_environment_id_fkey FOREIGN KEY (environment_id) REFERENCES environments (id);
CREATE UNIQUE INDEX hosts_fqdn_idx ON hosts(fqdn);

CREATE TABLE services (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    environment_id UUID DEFAULT NULL,
    labels JSONB NOT NULL DEFAULT '{}',
    auto_created BOOLEAN NOT NULL DEFAULT false,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE services ADD CONSTRAINT services_environment_id_fkey FOREIGN KEY (environment_id) REFERENCES environments (id);
CREATE UNIQUE INDEX services_name_idx ON services(name);

ALTER TABLE alerts ADD CONSTRAINT alerts_environment_id_fkey FOREIGN KEY (environment_id) REFERENCES environments (id);
ALTER TABLE alerts ADD CONSTRAINT alerts_host_id_fkey FOREIGN KEY (host_id) REFERENCES hosts (id);
ALTER TABLE alerts ADD CONSTRAINT alerts_service_id_fkey FOREIGN KEY (service_id) REFERENCES services (id);
ALTER TABLE alerts ADD CONSTRAINT alerts_acknowledged_by_fkey FOREIGN KEY (acknowledged_by) REFERENCES users (id);
ALTER TABLE alerts ADD CONSTRAINT alerts_closed_by_fkey FOREIGN KEY (closed_by) REFERENCES users (id);

CREATE TABLE alert_events (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    alert_id UUID NOT NULL,
    user_id UUID DEFAULT NULL,
    kind TEXT NOT NULL,
    payload JSONB NOT NULL DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE alert_events ADD CONSTRAINT alert_events_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
ALTER TABLE alert_events ADD CONSTRAINT alert_events_user_id_fkey FOREIGN KEY (user_id) REFERENCES users (id);
CREATE INDEX alert_events_alert_id_idx ON alert_events(alert_id);

CREATE TABLE comments (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    alert_id UUID NOT NULL,
    user_id UUID NOT NULL,
    body TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE comments ADD CONSTRAINT comments_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
ALTER TABLE comments ADD CONSTRAINT comments_user_id_fkey FOREIGN KEY (user_id) REFERENCES users (id);
CREATE INDEX comments_alert_id_idx ON comments(alert_id);

CREATE TABLE blackouts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    environment_id UUID DEFAULT NULL,
    host_id UUID DEFAULT NULL,
    service_id UUID DEFAULT NULL,
    starts_at TIMESTAMP WITH TIME ZONE NOT NULL,
    ends_at TIMESTAMP WITH TIME ZONE NOT NULL,
    reason TEXT NOT NULL DEFAULT '',
    created_by UUID DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE blackouts ADD CONSTRAINT blackouts_environment_id_fkey FOREIGN KEY (environment_id) REFERENCES environments (id);
ALTER TABLE blackouts ADD CONSTRAINT blackouts_host_id_fkey FOREIGN KEY (host_id) REFERENCES hosts (id);
ALTER TABLE blackouts ADD CONSTRAINT blackouts_service_id_fkey FOREIGN KEY (service_id) REFERENCES services (id);
ALTER TABLE blackouts ADD CONSTRAINT blackouts_created_by_fkey FOREIGN KEY (created_by) REFERENCES users (id);

CREATE TABLE push_subscriptions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    endpoint TEXT NOT NULL,
    p256dh TEXT NOT NULL,
    auth TEXT NOT NULL,
    user_agent TEXT NOT NULL DEFAULT '',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (endpoint)
);
ALTER TABLE push_subscriptions ADD CONSTRAINT push_subscriptions_user_id_fkey FOREIGN KEY (user_id) REFERENCES users (id);

CREATE TABLE auto_close_jobs (
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

CREATE TABLE push_notification_jobs (
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
ALTER TABLE push_notification_jobs ADD CONSTRAINT push_notification_jobs_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
