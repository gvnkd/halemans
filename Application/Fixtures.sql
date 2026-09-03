-- Static dev seeds: the three alert sources (doc §4). WebhookTokens are NOT
-- seeded here — their values are runtime-generated under .devenv/state/ and
-- upserted by `seed-halemans` (psql cannot expand env vars in this file).

INSERT INTO sources (id, type, name, base_url, env, poll_interval_seconds, enabled, config) VALUES
    ('a0000000-0000-0000-0000-000000000001', 'zabbix',       'zabbix-dev',       'http://127.0.0.1:10080', 'dev', 5,  true, '{"tokenEnv":"ZABBIX_TOKEN"}'),
    ('a0000000-0000-0000-0000-000000000002', 'grafana',      'grafana-dev',      'http://127.0.0.1:3001',  'dev', 30, true, '{"tokenEnv":"GRAFANA_TOKEN"}'),
    ('a0000000-0000-0000-0000-000000000003', 'alertmanager', 'alertmanager-dev', 'http://127.0.0.1:9093',  'dev', 30, true, '{}')
ON CONFLICT (id) DO NOTHING;
