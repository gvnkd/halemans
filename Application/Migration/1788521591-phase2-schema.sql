-- Milestone 2 phase-2 schema delta (design_docs/milestone_2.md §2).
-- Mirrors the additions appended to Application/Schema.sql; alerts column
-- additions live here as ALTERs (Schema.sql carries them inline).

CREATE TABLE teams (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    description TEXT NOT NULL DEFAULT '',
    defaults JSONB NOT NULL DEFAULT '{}',
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
    name TEXT NOT NULL,
    steps JSONB NOT NULL DEFAULT '[]',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX escalation_policies_name_idx ON escalation_policies(name);

CREATE TABLE grouping_rules (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
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

ALTER TABLE alerts ADD COLUMN group_id UUID DEFAULT NULL;
ALTER TABLE alerts ADD COLUMN grouped_by_version INT DEFAULT NULL;
ALTER TABLE alerts ADD CONSTRAINT alerts_group_id_fkey FOREIGN KEY (group_id) REFERENCES alert_groups (id);
CREATE INDEX alerts_group_id_idx ON alerts(group_id);

ALTER TABLE push_notification_jobs ADD COLUMN target_user_ids JSONB DEFAULT NULL;
ALTER TABLE push_notification_jobs ADD COLUMN rule_id UUID DEFAULT NULL;
ALTER TABLE push_notification_jobs ADD COLUMN group_id UUID DEFAULT NULL;

CREATE TABLE notification_rules (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
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
