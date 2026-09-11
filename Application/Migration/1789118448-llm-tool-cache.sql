-- Milestone 10 §6: short-lived LLM tool result cache. Singleton
-- llm_tool_cache_configs (no row = defaults: enabled, 300s TTL);
-- llm_tool_cache keyed by (tool, arguments).
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
