-- Milestone 5 phase-5 schema delta (design_docs/milestone_5.md §2).
-- Mirrors the additions appended to Application/Schema.sql.

ALTER TABLE sources ADD COLUMN consecutive_failures INT NOT NULL DEFAULT 0;
ALTER TABLE sources ADD COLUMN last_error TEXT DEFAULT NULL;
ALTER TABLE sources ADD COLUMN next_poll_at TIMESTAMP WITH TIME ZONE DEFAULT NULL;

CREATE TABLE retention_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    raw_events_days INT NOT NULL DEFAULT 30,
    alert_events_days INT DEFAULT NULL,
    enabled BOOLEAN NOT NULL DEFAULT true,
    updated_by UUID DEFAULT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    last_run_at TIMESTAMP WITH TIME ZONE DEFAULT NULL
);
ALTER TABLE retention_configs ADD CONSTRAINT retention_configs_updated_by_fkey FOREIGN KEY (updated_by) REFERENCES users (id);

CREATE TABLE audit_exports (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID DEFAULT NULL,
    scope JSONB NOT NULL DEFAULT '{}',
    format TEXT NOT NULL DEFAULT 'csv',
    row_count INT NOT NULL DEFAULT 0,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE audit_exports ADD CONSTRAINT audit_exports_user_id_fkey FOREIGN KEY (user_id) REFERENCES users (id);
CREATE INDEX audit_exports_created_at_idx ON audit_exports(created_at DESC);

CREATE TABLE retention_jobs (
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

CREATE TABLE source_health_jobs (
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

CREATE INDEX alerts_environment_status_idx ON alerts(environment_id, status);
CREATE INDEX alerts_fingerprint_active_idx ON alerts(fingerprint) WHERE status <> 'closed';