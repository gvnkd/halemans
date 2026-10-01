-- Per-alert zabbix host group names (jsonb array), resolved at poll time via
-- host.get selectHostGroups. Drives per-team alert visibility and the
-- ungrouped-host drop.
ALTER TABLE alerts ADD COLUMN host_groups JSONB NOT NULL DEFAULT '[]';
