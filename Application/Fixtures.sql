-- Static dev seeds: the three alert sources (doc §4). WebhookTokens are NOT
-- seeded here — their values are runtime-generated under .devenv/state/ and
-- upserted by `seed-halemans` (psql cannot expand env vars in this file).
-- writeBack/jiraWritable enable the write-back + ticket-creation pipeline
-- (milestone 3 D9); jiraProjects/cmdbSpaces are per-source scope overrides.
-- The Jira/Confluence CONNECTIONS are jira_configs/cmdb_configs rows, seeded
-- by seed-halemans (which also applies the same merge for databases
-- initialized before this changed).

INSERT INTO sources (id, type, name, base_url, env, poll_interval_seconds, enabled, config) VALUES
    ('a0000000-0000-0000-0000-000000000001', 'zabbix',       'zabbix-dev',       'http://127.0.0.1:10080', 'dev', 5,  true, '{"tokenEnv":"ZABBIX_TOKEN","writeBack":true,"jiraWritable":true,"jiraProjects":["DEV"],"cmdbSpaces":["DEV"]}'),
    ('a0000000-0000-0000-0000-000000000002', 'grafana',      'grafana-dev',      'http://127.0.0.1:3001',  'dev', 30, true, '{"tokenEnv":"GRAFANA_TOKEN","writeBack":true,"jiraWritable":true,"jiraProjects":["DEV"],"cmdbSpaces":["DEV"]}'),
    ('a0000000-0000-0000-0000-000000000003', 'alertmanager', 'alertmanager-dev', 'http://127.0.0.1:9093',  'dev', 30, true, '{"writeBack":true,"jiraWritable":true,"jiraProjects":["DEV"],"cmdbSpaces":["DEV"]}')
ON CONFLICT (id) DO NOTHING;
