CREATE TABLE zabbix_host_groups (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    source_id UUID NOT NULL,
    name TEXT NOT NULL,
    group_id TEXT NOT NULL,
    synced_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (source_id, name)
);
ALTER TABLE zabbix_host_groups ADD CONSTRAINT zabbix_host_groups_source_id_fkey FOREIGN KEY (source_id) REFERENCES sources (id);
