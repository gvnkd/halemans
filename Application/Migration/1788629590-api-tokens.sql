-- Milestone 6 schema delta (design_docs/milestone_6.md §2).
-- Mirrors the additions appended to Application/Schema.sql.

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
