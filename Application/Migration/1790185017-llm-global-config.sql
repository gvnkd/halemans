-- Global LLM limits (agent configuration milestone): one budget caps ALL LLM
-- consumers (analysis pipeline scope 'analysis' + chat agent scope 'agent'),
-- each consumer additionally keeps its own per-agent cap
-- (llm_agent_configs). Singleton-by-convention: no row = the env fallbacks
-- (LLM_DAILY_TOKEN_BUDGET, LLM_RATE_PER_MINUTE). Editable on the
-- admin/LLM → Agent configuration page.
CREATE TABLE llm_global_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    daily_token_budget INT NOT NULL DEFAULT 1000000,
    rate_per_minute INT NOT NULL DEFAULT 20,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
