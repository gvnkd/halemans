-- Standard roles for a fresh docker-compose deployment. Keep in sync with
-- nix/scripts/seed-halemans.sh and nix/scripts/smoke-check.sh.
INSERT INTO roles (name, privileges) VALUES
    ('admin',  '{view,ack,close,escalate,manage_blackouts,manage_rules,manage_users,manage_sources,admin}'),
    ('sre',    '{view,ack,close,escalate}'),
    ('viewer', '{view}')
ON CONFLICT (name) DO UPDATE SET privileges = EXCLUDED.privileges;
