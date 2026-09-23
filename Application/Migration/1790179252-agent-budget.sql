-- Agent budget + scoped LLM budget counters (internal API milestone).
--
-- llm_budget_counters gains a scope ('analysis' for the enrichment pipeline,
-- 'agent' for the embedded chat agent) so the two consumers get dedicated
-- budgets instead of racing on one shared cap. Existing rows are the
-- analysis pipeline's, hence the 'analysis' default.
ALTER TABLE llm_budget_counters ADD COLUMN scope TEXT NOT NULL DEFAULT 'analysis';
ALTER TABLE llm_budget_counters DROP CONSTRAINT llm_budget_counters_provider_day_key;
ALTER TABLE llm_budget_counters ADD CONSTRAINT llm_budget_counters_scope_provider_day_key UNIQUE (scope, provider, day);

-- Agent chat budget, singleton-by-convention like llm_tool_cache_configs: no
-- row = the defaults below. Configurable on the admin/LLM page; the agent
-- (web chat) spends from this budget only, tracked under scope 'agent'.
CREATE TABLE llm_agent_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    daily_token_budget INT NOT NULL DEFAULT 200000,
    rate_per_minute INT NOT NULL DEFAULT 12,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
