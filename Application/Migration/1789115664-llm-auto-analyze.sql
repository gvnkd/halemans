-- Milestone 10 §5: auto-analysis gate — which alert statuses/severities get
-- an LLM analysis enqueued automatically. Singleton-by-convention like
-- retention_configs: no row = built-in defaults (firing+ack, all severities).
CREATE TABLE llm_auto_analyze_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    statuses JSONB NOT NULL DEFAULT '["firing", "ack"]',
    severities JSONB NOT NULL DEFAULT '["critical", "high", "warning", "info"]',
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
