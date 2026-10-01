-- One-time capability tokens for external interactions (markdown Ack links
-- from Mattermost, later unack/close). Only the SHA-256 hash is stored; the
-- plaintext exists solely in the rendered link. Consumption is atomic
-- (UPDATE ... WHERE used_at IS NULL RETURNING), tokens rotate on every
-- root-post re-render, and they expire.
CREATE TABLE action_tokens (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    alert_id UUID NOT NULL,
    action TEXT NOT NULL,
    token_hash TEXT NOT NULL UNIQUE,
    used_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    expires_at TIMESTAMP WITH TIME ZONE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE action_tokens ADD CONSTRAINT action_tokens_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
CREATE INDEX action_tokens_alert_id_idx ON action_tokens(alert_id);
