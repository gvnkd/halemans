-- Milestone 1 phase-1 schema delta (design_docs/milestone_1.md §2).
-- Mirrors the additions appended to Application/Schema.sql.

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

ALTER TABLE alerts ADD COLUMN environment_id UUID DEFAULT NULL;
ALTER TABLE alerts ADD COLUMN host_id UUID DEFAULT NULL;
ALTER TABLE alerts ADD COLUMN service_id UUID DEFAULT NULL;
ALTER TABLE alerts ADD COLUMN suppressed BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE alerts ADD COLUMN acknowledged_by UUID DEFAULT NULL;
ALTER TABLE alerts ADD COLUMN acknowledged_at TIMESTAMP WITH TIME ZONE DEFAULT NULL;
ALTER TABLE alerts ADD COLUMN ack_comment TEXT DEFAULT NULL;
ALTER TABLE alerts ADD COLUMN ack_expires_at TIMESTAMP WITH TIME ZONE DEFAULT NULL;
ALTER TABLE alerts ADD COLUMN closed_by UUID DEFAULT NULL;
ALTER TABLE alerts ADD COLUMN closed_at TIMESTAMP WITH TIME ZONE DEFAULT NULL;
ALTER TABLE alerts ADD COLUMN close_reason TEXT DEFAULT NULL;
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
