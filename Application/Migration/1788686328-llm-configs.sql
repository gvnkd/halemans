-- Milestone 7: DB-resident LLM config + sources.name unique (design_docs/milestone_7.md §5/§7).
CREATE TABLE llm_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
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
CREATE UNIQUE INDEX sources_name_idx ON sources(name);
