-- Agent chat (internal API milestone): per-user chat sessions with the
-- Halemans agent plus the persisted message/tool-call history. The web UI
-- widget, the internal API and the MCP server all read/write through these
-- tables; the raw LLM conversation state is rebuilt from agent_messages.
CREATE TABLE agent_sessions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    title TEXT DEFAULT NULL,
    page_context JSONB,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE agent_sessions ADD CONSTRAINT agent_sessions_user_id_fkey FOREIGN KEY (user_id) REFERENCES users (id);
CREATE INDEX agent_sessions_user_id_idx ON agent_sessions(user_id);

CREATE TABLE agent_messages (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    session_id UUID NOT NULL,
    role TEXT NOT NULL,
    content TEXT NOT NULL DEFAULT '',
    tool_calls JSONB,
    tool_call_id TEXT DEFAULT NULL,
    page_context JSONB,
    prompt_tokens INT DEFAULT NULL,
    completion_tokens INT DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE agent_messages ADD CONSTRAINT agent_messages_session_id_fkey FOREIGN KEY (session_id) REFERENCES agent_sessions (id) ON DELETE CASCADE;
CREATE INDEX agent_messages_session_id_idx ON agent_messages(session_id, created_at);

-- Audit trail for the internal API: one row per granted call (denied calls
-- are visible in the access log via their 4xx status). The act-as user gives
-- the trail an owner; method/path are what was attempted.
CREATE TABLE internal_api_audit (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    method TEXT NOT NULL,
    path TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE internal_api_audit ADD CONSTRAINT internal_api_audit_user_id_fkey FOREIGN KEY (user_id) REFERENCES users (id);
CREATE INDEX internal_api_audit_user_id_idx ON internal_api_audit(user_id, created_at);
