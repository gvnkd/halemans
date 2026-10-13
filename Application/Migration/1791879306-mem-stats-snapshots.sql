-- Worker memory telemetry: one GHC RTS snapshot per mem-stats hook call
-- (autoclose tick, zabbix/grafana poll cycles). Written by
-- Application.Service.MemStats, surfaced on the admin "Worker memory" panel,
-- pruned by the Retention job after a week. Same table as in Schema.sql —
-- this migration applies it to existing databases.
CREATE TABLE IF NOT EXISTS mem_stats_snapshots (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    label TEXT NOT NULL,
    live_bytes BIGINT NOT NULL DEFAULT 0,
    max_live_bytes BIGINT NOT NULL DEFAULT 0,
    in_use_bytes BIGINT NOT NULL DEFAULT 0,
    allocated_bytes BIGINT NOT NULL DEFAULT 0,
    gcs BIGINT NOT NULL DEFAULT 0,
    major_gcs BIGINT NOT NULL DEFAULT 0,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX IF NOT EXISTS mem_stats_snapshots_label_created_idx ON mem_stats_snapshots(label, created_at);
