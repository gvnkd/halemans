-- Missing-problem scan replay watermarks: the newest event clock already
-- fetched+processed per zabbix trigger, independent of local alert rows.
-- Lets the scan push time_from down to the watermark (tail-only fetches)
-- instead of re-fetching the whole scanWindowSeconds history every reconcile
-- cycle for triggers that never become tracked. Same table as in Schema.sql
-- — this migration applies it to existing databases.
CREATE TABLE IF NOT EXISTS scan_watermarks (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    source_id UUID NOT NULL,
    trigger_id TEXT NOT NULL,
    watermark BIGINT NOT NULL DEFAULT 0,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX IF NOT EXISTS scan_watermarks_source_trigger_idx ON scan_watermarks(source_id, trigger_id);
ALTER TABLE scan_watermarks ADD CONSTRAINT scan_watermarks_source_id_fkey FOREIGN KEY (source_id) REFERENCES sources (id);
