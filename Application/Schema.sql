-- Halemans schema. Milestone 0 carries only the minimal slice needed for the
-- smoke suite (doc §6); phase 1 extends this per design_docs/01_highlevel.md §3.

-- sources.type: zabbix | grafana | alertmanager | webhook.
-- sources.config: non-secret config; credentials are env-var references like {"tokenEnv":"ZABBIX_TOKEN"}.
CREATE TABLE sources (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    type TEXT NOT NULL,
    name TEXT NOT NULL,
    base_url TEXT NOT NULL DEFAULT '',
    env TEXT NOT NULL DEFAULT 'dev',
    poll_interval_seconds INT NOT NULL DEFAULT 30,
    enabled BOOLEAN NOT NULL DEFAULT true,
    config JSONB NOT NULL DEFAULT '{}',
    last_sync_cursor TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    last_reconcile_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    consecutive_failures INT NOT NULL DEFAULT 0,
    last_error TEXT DEFAULT NULL,
    next_poll_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
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
    facets JSONB NOT NULL DEFAULT '{}',
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
    suppressed_by TEXT DEFAULT NULL,
    acknowledged_by UUID DEFAULT NULL,
    acknowledged_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    ack_comment TEXT DEFAULT NULL,
    ack_expires_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    closed_by UUID DEFAULT NULL,
    closed_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    close_reason TEXT DEFAULT NULL,
    group_id UUID DEFAULT NULL,
    grouped_by_version INT DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE alerts ADD CONSTRAINT alerts_source_id_fkey FOREIGN KEY (source_id) REFERENCES sources (id);
CREATE INDEX alerts_fingerprint_idx ON alerts(fingerprint);
CREATE INDEX alerts_status_idx ON alerts(status);
CREATE INDEX alerts_facets_idx ON alerts USING GIN (facets jsonb_path_ops);

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

-- Cache of zabbix host groups per source (hostgroup.get). Host groups are
-- near-static, so they are synced manually (SyncHostGroupsAction) instead of
-- on every poll; PollZabbix resolves team group names against this table.
CREATE TABLE zabbix_host_groups (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    source_id UUID NOT NULL,
    name TEXT NOT NULL,
    group_id TEXT NOT NULL,
    synced_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (source_id, name)
);
ALTER TABLE zabbix_host_groups ADD CONSTRAINT zabbix_host_groups_source_id_fkey FOREIGN KEY (source_id) REFERENCES sources (id);

-- Milestone 1 (phase 1) schema delta, per design_docs/milestone_1.md §2.

CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
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
    protected BOOLEAN NOT NULL DEFAULT false,
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
    cmdb_page_id TEXT DEFAULT NULL,
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
    cmdb_page_id TEXT DEFAULT NULL,
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
    target_user_ids JSONB DEFAULT NULL,
    rule_id UUID DEFAULT NULL,
    group_id UUID DEFAULT NULL,
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

-- Milestone 2 (phase 2) schema delta, per design_docs/milestone_2.md §2.

CREATE TABLE teams (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    name TEXT NOT NULL,
    description TEXT NOT NULL DEFAULT '',
    host_groups JSONB NOT NULL DEFAULT '[]',
    defaults JSONB NOT NULL DEFAULT '{}',
    default_dashboard_config JSONB DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX teams_name_idx ON teams(name);

CREATE TABLE team_members (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    team_id UUID NOT NULL,
    user_id UUID NOT NULL,
    team_role TEXT NOT NULL DEFAULT 'member',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (team_id, user_id)
);
ALTER TABLE team_members ADD CONSTRAINT team_members_team_id_fkey FOREIGN KEY (team_id) REFERENCES teams (id);
ALTER TABLE team_members ADD CONSTRAINT team_members_user_id_fkey FOREIGN KEY (user_id) REFERENCES users (id);

CREATE TABLE on_call_schedules (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    team_id UUID NOT NULL,
    members JSONB NOT NULL DEFAULT '[]',
    rotation JSONB NOT NULL DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE on_call_schedules ADD CONSTRAINT on_call_schedules_team_id_fkey FOREIGN KEY (team_id) REFERENCES teams (id);
CREATE UNIQUE INDEX on_call_schedules_team_id_idx ON on_call_schedules(team_id);

CREATE TABLE escalation_policies (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    name TEXT NOT NULL,
    steps JSONB NOT NULL DEFAULT '[]',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX escalation_policies_name_idx ON escalation_policies(name);

CREATE TABLE grouping_rules (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    position INT NOT NULL DEFAULT 0,
    name TEXT NOT NULL,
    enabled BOOLEAN NOT NULL DEFAULT true,
    version INT NOT NULL DEFAULT 1,
    match JSONB NOT NULL DEFAULT '{}',
    group_key_template TEXT NOT NULL DEFAULT '',
    created_by UUID DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE grouping_rules ADD CONSTRAINT grouping_rules_created_by_fkey FOREIGN KEY (created_by) REFERENCES users (id);

CREATE TABLE alert_groups (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    group_key TEXT NOT NULL,
    title TEXT NOT NULL DEFAULT '',
    environment_id UUID DEFAULT NULL,
    status TEXT NOT NULL DEFAULT 'firing',
    worst_severity TEXT NOT NULL DEFAULT 'warning',
    member_count INT NOT NULL DEFAULT 0,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    resolved_at TIMESTAMP WITH TIME ZONE DEFAULT NULL
);
ALTER TABLE alert_groups ADD CONSTRAINT alert_groups_environment_id_fkey FOREIGN KEY (environment_id) REFERENCES environments (id);
CREATE UNIQUE INDEX alert_groups_group_key_idx ON alert_groups(group_key);

ALTER TABLE alerts ADD CONSTRAINT alerts_group_id_fkey FOREIGN KEY (group_id) REFERENCES alert_groups (id);
CREATE INDEX alerts_group_id_idx ON alerts(group_id);

CREATE TABLE notification_rules (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    position INT NOT NULL DEFAULT 0,
    name TEXT NOT NULL,
    enabled BOOLEAN NOT NULL DEFAULT true,
    match JSONB NOT NULL DEFAULT '{}',
    severity_threshold TEXT NOT NULL DEFAULT 'info',
    team_id UUID DEFAULT NULL,
    user_id UUID DEFAULT NULL,
    channel TEXT NOT NULL DEFAULT 'browser_push',
    channel_config JSONB NOT NULL DEFAULT '{}',
    throttle_seconds INT NOT NULL DEFAULT 300,
    escalation_policy_id UUID DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE notification_rules ADD CONSTRAINT notification_rules_team_id_fkey FOREIGN KEY (team_id) REFERENCES teams (id);
ALTER TABLE notification_rules ADD CONSTRAINT notification_rules_user_id_fkey FOREIGN KEY (user_id) REFERENCES users (id);
ALTER TABLE notification_rules ADD CONSTRAINT notification_rules_escalation_policy_id_fkey FOREIGN KEY (escalation_policy_id) REFERENCES escalation_policies (id);

CREATE TABLE escalation_trackers (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    alert_id UUID NOT NULL,
    policy_id UUID NOT NULL,
    rule_id UUID DEFAULT NULL,
    current_step INT NOT NULL DEFAULT 0,
    next_deadline TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    status TEXT NOT NULL DEFAULT 'active',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE escalation_trackers ADD CONSTRAINT escalation_trackers_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
ALTER TABLE escalation_trackers ADD CONSTRAINT escalation_trackers_policy_id_fkey FOREIGN KEY (policy_id) REFERENCES escalation_policies (id);
ALTER TABLE escalation_trackers ADD CONSTRAINT escalation_trackers_rule_id_fkey FOREIGN KEY (rule_id) REFERENCES notification_rules (id);
CREATE INDEX escalation_trackers_due_idx ON escalation_trackers(status, next_deadline);

ALTER TABLE push_notification_jobs ADD CONSTRAINT push_notification_jobs_rule_id_fkey FOREIGN KEY (rule_id) REFERENCES notification_rules (id);
ALTER TABLE push_notification_jobs ADD CONSTRAINT push_notification_jobs_group_id_fkey FOREIGN KEY (group_id) REFERENCES alert_groups (id);

CREATE TABLE poll_grafana_jobs (
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

CREATE TABLE escalation_jobs (
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

-- Milestone 3 (phase 3) schema delta, per design_docs/milestone_3.md §2.

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
    protected BOOLEAN NOT NULL DEFAULT false,
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

CREATE TABLE field_mappings (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    facet TEXT NOT NULL,
    rank INT NOT NULL,
    kind TEXT NOT NULL,
    key TEXT NOT NULL,
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (facet, rank)
);

CREATE TABLE facet_backfill_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    cursor UUID DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);

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

-- Milestone 4 (phase 4) schema delta, per design_docs/milestone_4.md §2.

CREATE TABLE llm_prompt_templates (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    name TEXT NOT NULL,
    version INT NOT NULL,
    body TEXT NOT NULL,
    active BOOLEAN NOT NULL DEFAULT false,
    notes TEXT DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (name, version)
);
CREATE UNIQUE INDEX llm_prompt_templates_active_idx ON llm_prompt_templates(name) WHERE active;

CREATE TABLE llm_analyses (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    alert_id UUID NOT NULL,
    provider TEXT NOT NULL DEFAULT '',
    model TEXT NOT NULL DEFAULT '',
    prompt_template_id UUID DEFAULT NULL,
    prompt_version INT DEFAULT NULL,
    prompt_hash TEXT NOT NULL DEFAULT '',
    status TEXT NOT NULL DEFAULT 'queued',
    error_message TEXT DEFAULT NULL,
    result_md TEXT DEFAULT NULL,
    result JSONB DEFAULT NULL,
    tokens_in INT DEFAULT NULL,
    tokens_out INT DEFAULT NULL,
    tool_calls JSONB DEFAULT NULL,
    deduped_from UUID DEFAULT NULL,
    agent_role_id UUID DEFAULT NULL,
    language TEXT DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE llm_analyses ADD CONSTRAINT llm_analyses_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
ALTER TABLE llm_analyses ADD CONSTRAINT llm_analyses_prompt_template_id_fkey FOREIGN KEY (prompt_template_id) REFERENCES llm_prompt_templates (id);
ALTER TABLE llm_analyses ADD CONSTRAINT llm_analyses_deduped_from_fkey FOREIGN KEY (deduped_from) REFERENCES llm_analyses (id);
CREATE INDEX llm_analyses_alert_idx ON llm_analyses(alert_id, created_at DESC);
CREATE INDEX llm_analyses_prompt_hash_idx ON llm_analyses(prompt_hash);

CREATE TABLE llm_feedback (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    analysis_id UUID NOT NULL,
    user_id UUID NOT NULL,
    score INT NOT NULL,
    comment TEXT DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (analysis_id, user_id)
);
ALTER TABLE llm_feedback ADD CONSTRAINT llm_feedback_analysis_id_fkey FOREIGN KEY (analysis_id) REFERENCES llm_analyses (id);
ALTER TABLE llm_feedback ADD CONSTRAINT llm_feedback_user_id_fkey FOREIGN KEY (user_id) REFERENCES users (id);

CREATE TABLE llm_budget_counters (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    provider TEXT NOT NULL,
    day DATE NOT NULL,
    tokens_in BIGINT NOT NULL DEFAULT 0,
    tokens_out BIGINT NOT NULL DEFAULT 0,
    requests INT NOT NULL DEFAULT 0,
    UNIQUE (provider, day)
);

CREATE TABLE llm_analysis_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    analysis_id UUID NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE llm_analysis_jobs ADD CONSTRAINT llm_analysis_jobs_analysis_id_fkey FOREIGN KEY (analysis_id) REFERENCES llm_analyses (id);

-- Milestone 5 (phase 5) schema delta, per design_docs/milestone_5.md §2.

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

CREATE TABLE api_tokens (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    name TEXT NOT NULL,
    token_hash TEXT NOT NULL,
    prefix TEXT NOT NULL,
    scopes TEXT[] NOT NULL DEFAULT '{}',
    last_used_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    expires_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    revoked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE api_tokens ADD CONSTRAINT api_tokens_user_id_fkey FOREIGN KEY (user_id) REFERENCES users (id);
CREATE UNIQUE INDEX api_tokens_token_hash_idx ON api_tokens(token_hash);
CREATE INDEX api_tokens_user_id_idx ON api_tokens(user_id);

-- Milestone 7 (phase 7) schema delta, per design_docs/milestone_7.md §5/§7.

-- sources.name becomes the provisioning upsert key (milestone_7.md §5).
CREATE UNIQUE INDEX sources_name_idx ON sources(name);

-- DB-resident LLM provider config; at most one enabled row. api_key_env
-- stores the env var NAME, never the key (milestone_7.md §7).
CREATE TABLE llm_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    provider_name TEXT NOT NULL,
    endpoint TEXT NOT NULL,
    model TEXT NOT NULL,
    api_key_env TEXT DEFAULT NULL,
    tools_enabled BOOLEAN NOT NULL DEFAULT false,
    enabled BOOLEAN NOT NULL DEFAULT false,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX llm_configs_provider_name_idx ON llm_configs(provider_name);
CREATE UNIQUE INDEX llm_configs_enabled_idx ON llm_configs(enabled) WHERE enabled;

-- Milestone 8 (enrichment phase 0) schema delta, per design_docs/milestone_8.md §2.

-- Read-only Jira Assets (Insight) info sources. token_env holds the env var
-- NAME, never the token; basic auth additionally reads jira_email_env.
-- attribute_names is a comma-separated list of flattened attribute keys the
-- card panel + LLM excerpt render.
CREATE TABLE assets_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    name TEXT NOT NULL,
    base_url TEXT NOT NULL,
    token_env TEXT NOT NULL,
    auth_mode TEXT NOT NULL DEFAULT 'bearer',
    jira_email_env TEXT DEFAULT NULL,
    default_schema_name TEXT NOT NULL DEFAULT '',
    host_query_template TEXT NOT NULL DEFAULT '',
    attribute_names TEXT NOT NULL DEFAULT 'Owner,Cluster,Database,IP,Datacenter',
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX assets_configs_name_idx ON assets_configs(name);

-- App-side asset cache (assets-api.md §8.2: ids are JSON numbers).
CREATE TABLE assets_objects (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    config_id UUID NOT NULL,
    object_id BIGINT NOT NULL,
    object_key TEXT NOT NULL DEFAULT '',
    label TEXT NOT NULL DEFAULT '',
    object_type_name TEXT NOT NULL DEFAULT '',
    attributes JSONB NOT NULL DEFAULT '{}',
    icon_url TEXT NOT NULL DEFAULT '',
    source_url TEXT NOT NULL DEFAULT '',
    fetched_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (config_id, object_id)
);
ALTER TABLE assets_objects ADD CONSTRAINT assets_objects_config_id_fkey FOREIGN KEY (config_id) REFERENCES assets_configs (id);

-- Alert ↔ asset links. A NULL assets_object_id row is a negative-cache
-- marker: matched_by holds the query input that missed, created_at the
-- attempt time (30 min TTL, milestone_8.md §4).
CREATE TABLE asset_alert_links (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    alert_id UUID NOT NULL,
    assets_object_id UUID DEFAULT NULL,
    matched_by TEXT NOT NULL DEFAULT '',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE asset_alert_links ADD CONSTRAINT asset_alert_links_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
ALTER TABLE asset_alert_links ADD CONSTRAINT asset_alert_links_assets_object_id_fkey FOREIGN KEY (assets_object_id) REFERENCES assets_objects (id);
CREATE UNIQUE INDEX asset_alert_links_pair_idx ON asset_alert_links(alert_id, assets_object_id) WHERE assets_object_id IS NOT NULL;
CREATE INDEX asset_alert_links_alert_idx ON asset_alert_links(alert_id);

-- Server-side icon/avatar image cache: the card renders <img> against the
-- app (AssetsIconsController) which fills rows lazily from the Jira origin.
CREATE TABLE assets_icon_cache (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    config_id UUID NOT NULL,
    url TEXT NOT NULL,
    content_type TEXT NOT NULL DEFAULT 'image/png',
    body BYTEA NOT NULL,
    fetched_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE assets_icon_cache ADD CONSTRAINT assets_icon_cache_config_id_fkey FOREIGN KEY (config_id) REFERENCES assets_configs (id);
CREATE UNIQUE INDEX assets_icon_cache_config_url_idx ON assets_icon_cache(config_id, url);

-- Agent roles for LLM enrichment (milestone_8.md §7): name + prompt template
-- + tool whitelist; at most one default.
CREATE TABLE llm_agent_roles (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    name TEXT NOT NULL,
    description TEXT NOT NULL DEFAULT '',
    prompt_template_name TEXT NOT NULL DEFAULT 'alert_enrichment',
    tools JSONB NOT NULL DEFAULT '[]',
    enabled BOOLEAN NOT NULL DEFAULT true,
    is_default BOOLEAN NOT NULL DEFAULT false,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX llm_agent_roles_name_idx ON llm_agent_roles(name);
CREATE UNIQUE INDEX llm_agent_roles_default_idx ON llm_agent_roles(is_default) WHERE is_default;

ALTER TABLE llm_analyses ADD CONSTRAINT llm_analyses_agent_role_id_fkey FOREIGN KEY (agent_role_id) REFERENCES llm_agent_roles (id);

-- Integration configs (milestone 10): DB-resident Jira/Confluence
-- connections with MULTIPLE search scopes. projects/spaces are JSONB string
-- arrays; an empty array means "no scope clause" (search everything the
-- token can see). token_env holds the env var NAME, never the token.
CREATE TABLE jira_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    name TEXT NOT NULL,
    base_url TEXT NOT NULL,
    token_env TEXT NOT NULL,
    api_version TEXT NOT NULL DEFAULT '3',
    projects JSONB NOT NULL DEFAULT '[]',
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX jira_configs_name_idx ON jira_configs(name);

CREATE TABLE cmdb_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    protected BOOLEAN NOT NULL DEFAULT false,
    name TEXT NOT NULL,
    base_url TEXT NOT NULL,
    token_env TEXT NOT NULL,
    spaces JSONB NOT NULL DEFAULT '[]',
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX cmdb_configs_name_idx ON cmdb_configs(name);

-- Auto-analysis gate (milestone 10 §5): which alert statuses/severities get
-- an LLM analysis enqueued automatically (ingest + enrichment retrigger).
-- Singleton-by-convention like retention_configs: no row = built-in
-- defaults (statuses firing+ack, all severities, all environments).
-- environments matches the EFFECTIVE env; empty list = no env scope.
CREATE TABLE llm_auto_analyze_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    statuses JSONB NOT NULL DEFAULT '["firing", "ack"]',
    severities JSONB NOT NULL DEFAULT '["critical", "high", "warning", "info"]',
    environments JSONB NOT NULL DEFAULT '[]',
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);

-- LLM tool result cache (milestone 10 §6): short-lived per (tool, arguments)
-- memo for the read-only agent tools; failure texts are never cached. TTL
-- and on/off live in the singleton llm_tool_cache_configs (no row =
-- defaults: enabled, 300s).
CREATE TABLE llm_tool_cache (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    tool TEXT NOT NULL,
    arguments TEXT NOT NULL,
    response TEXT NOT NULL,
    fetched_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX llm_tool_cache_key_idx ON llm_tool_cache(tool, arguments);

CREATE TABLE llm_tool_cache_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    ttl_seconds INT NOT NULL DEFAULT 300,
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
