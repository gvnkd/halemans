-- Milestone 4 phase-4 schema delta (design_docs/milestone_4.md §2).
-- Mirrors the additions appended to Application/Schema.sql.

CREATE TABLE llm_prompt_templates (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
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
