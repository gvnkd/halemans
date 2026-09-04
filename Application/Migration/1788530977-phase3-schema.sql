-- Milestone 3 phase-3 schema delta (design_docs/milestone_3.md §2).
-- Mirrors the additions appended to Application/Schema.sql; column additions
-- to existing tables live here as ALTERs (Schema.sql carries them inline).

CREATE TABLE cmdb_entries (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    host_id UUID DEFAULT NULL,
    service_id UUID DEFAULT NULL,
    page_id TEXT DEFAULT NULL,
    title TEXT NOT NULL DEFAULT '',
    excerpt TEXT NOT NULL DEFAULT '',
    url TEXT NOT NULL DEFAULT '',
    fetched_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE cmdb_entries ADD CONSTRAINT cmdb_entries_host_id_fkey FOREIGN KEY (host_id) REFERENCES hosts (id);
ALTER TABLE cmdb_entries ADD CONSTRAINT cmdb_entries_service_id_fkey FOREIGN KEY (service_id) REFERENCES services (id);
CREATE UNIQUE INDEX cmdb_entries_host_idx ON cmdb_entries(host_id) WHERE host_id IS NOT NULL;
CREATE UNIQUE INDEX cmdb_entries_service_idx ON cmdb_entries(service_id) WHERE service_id IS NOT NULL;

CREATE TABLE jira_links (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    alert_id UUID NOT NULL,
    ticket_key TEXT NOT NULL,
    summary TEXT NOT NULL DEFAULT '',
    status TEXT NOT NULL DEFAULT '',
    url TEXT NOT NULL DEFAULT '',
    origin TEXT NOT NULL DEFAULT 'auto',
    synced_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (alert_id, ticket_key)
);
ALTER TABLE jira_links ADD CONSTRAINT jira_links_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
CREATE INDEX jira_links_alert_idx ON jira_links(alert_id);

CREATE TABLE dashboards (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    name TEXT NOT NULL,
    config JSONB NOT NULL DEFAULT '[]',
    position INT NOT NULL DEFAULT 0,
    is_default BOOLEAN NOT NULL DEFAULT false,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE dashboards ADD CONSTRAINT dashboards_user_id_fkey FOREIGN KEY (user_id) REFERENCES users (id);
CREATE UNIQUE INDEX dashboards_default_idx ON dashboards(user_id) WHERE is_default;

CREATE TABLE write_back_attempts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    alert_id UUID NOT NULL,
    action TEXT NOT NULL,
    source_id UUID NOT NULL,
    status TEXT NOT NULL DEFAULT 'queued',
    attempts INT NOT NULL DEFAULT 0,
    last_error TEXT DEFAULT NULL,
    silence_id TEXT DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE write_back_attempts ADD CONSTRAINT write_back_attempts_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
ALTER TABLE write_back_attempts ADD CONSTRAINT write_back_attempts_source_id_fkey FOREIGN KEY (source_id) REFERENCES sources (id);
CREATE INDEX write_back_attempts_alert_idx ON write_back_attempts(alert_id);
CREATE INDEX write_back_attempts_status_idx ON write_back_attempts(status);

ALTER TABLE hosts ADD COLUMN cmdb_page_id TEXT DEFAULT NULL;
ALTER TABLE services ADD COLUMN cmdb_page_id TEXT DEFAULT NULL;
ALTER TABLE teams ADD COLUMN default_dashboard_config JSONB DEFAULT NULL;

CREATE TABLE enrich_alert_jobs (
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
ALTER TABLE enrich_alert_jobs ADD CONSTRAINT enrich_alert_jobs_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);

CREATE TABLE write_back_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    attempt_id UUID NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE write_back_jobs ADD CONSTRAINT write_back_jobs_attempt_id_fkey FOREIGN KEY (attempt_id) REFERENCES write_back_attempts (id);

CREATE TABLE jira_sync_jobs (
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
