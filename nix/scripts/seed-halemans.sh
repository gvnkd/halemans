set -euo pipefail

# Seeds Halemans-side rows: dev sources are static in Application/Fixtures.sql
# (loaded at postgres init); this script (idempotently) upserts the matching
# WebhookTokens from the runtime-generated token files, and enqueues the
# zabbix poller job.
halemans-ensure-tokens
# shellcheck disable=SC1091
source "$DEVENV_STATE/halemans/env.sh"

echo "seed-halemans: waiting for postgres"
for i in $(seq 1 30); do
    if pg_isready -q -h "${PGHOST:-/tmp}" 2>/dev/null; then break; fi
    if [ "$i" = 30 ]; then echo "seed-halemans: postgres not ready" >&2; exit 1; fi
    sleep 2
done

psql "${DATABASE_URL:?}" -v ON_ERROR_STOP=1 \
    -v am_token="$HALEMANS_AM_HOOK_TOKEN" \
    -v generic_token="$HALEMANS_GENERIC_HOOK_TOKEN" <<'SQL'
INSERT INTO webhook_tokens (source_id, token)
SELECT id, :'am_token' FROM sources WHERE type = 'alertmanager'
ON CONFLICT (token) DO NOTHING;

INSERT INTO webhook_tokens (source_id, token)
SELECT id, :'generic_token' FROM sources WHERE type = 'grafana'
ON CONFLICT (token) DO NOTHING;
SQL

# Enrichment/write-back source config (milestone 3 D9). Fixtures.sql carries
# the same values declaratively; this idempotent jsonb merge fixes databases
# initialized before the fixtures changed.
psql "${DATABASE_URL:?}" -v ON_ERROR_STOP=1 <<'SQL'
UPDATE sources
SET config = config || '{"writeBack":true,"cmdbSpace":"DEV","jiraProject":"DEV"}'::jsonb
WHERE type IN ('zabbix', 'grafana', 'alertmanager');

INSERT INTO cmdb_entries (host_id, page_id, title, excerpt, url)
SELECT h.id, '1001', 'dev-host-01',
       'Owner: team-sre. Runbook: https://wiki.example/runbooks/dev-host-01',
       '/spaces/DEV/pages/1001'
FROM hosts h
WHERE h.fqdn = 'dev-host-01'
  AND NOT EXISTS (SELECT 1 FROM cmdb_entries c WHERE c.host_id = h.id);
SQL

# Default LLM prompt template (milestone 4 D9). Idempotent; keep in sync with
# the inline seeding in nix/scripts/smoke-check.sh.
psql "${DATABASE_URL:?}" -v ON_ERROR_STOP=1 <<'SQL'
INSERT INTO llm_prompt_templates (name, version, body, active, notes)
SELECT 'alert_enrichment', 1, $tpl$You are an SRE assistant enriching an ops alert for the on-call engineer. Be concise; do not invent facts.

## Alert
- Title: {{alert.title}}
- Severity: {{alert.severity}}
- Environment: {{alert.env}}
- Host: {{alert.host}}
- Service: {{alert.service}}
- Check: {{alert.check_name}}
- Labels: {{alert.labels}}
- Annotations: {{alert.annotations}}

{{alert.description}}

## Recent events
{{events}}

## CMDB context
{{cmdb_excerpt}}

## Similar past alerts
{{similar_alerts}}

## Linked Jira tickets
{{jira_links}}

Analyze the probable cause of this alert using the context above and suggest concrete next steps for the on-call engineer.
$tpl$, true, 'seeded v1'
WHERE NOT EXISTS (SELECT 1 FROM llm_prompt_templates WHERE name = 'alert_enrichment' AND version = 1);
SQL

# Milestone 8 D5: template v2 gains the {{assets_excerpt}} slot. Deactivating
# only versions < 2 keeps admin-created newer versions untouched.
psql "${DATABASE_URL:?}" -v ON_ERROR_STOP=1 <<'SQL'
UPDATE llm_prompt_templates SET active = false, updated_at = NOW()
WHERE name = 'alert_enrichment' AND version < 2;

INSERT INTO llm_prompt_templates (name, version, body, active, notes)
SELECT 'alert_enrichment', 2, $tpl$You are an SRE assistant enriching an ops alert for the on-call engineer. Be concise; do not invent facts.

## Alert
- Title: {{alert.title}}
- Severity: {{alert.severity}}
- Environment: {{alert.env}}
- Host: {{alert.host}}
- Service: {{alert.service}}
- Check: {{alert.check_name}}
- Labels: {{alert.labels}}
- Annotations: {{alert.annotations}}

{{alert.description}}

## Recent events
{{events}}

## CMDB context
{{cmdb_excerpt}}

## Linked assets
{{assets_excerpt}}

## Similar past alerts
{{similar_alerts}}

## Linked Jira tickets
{{jira_links}}

Analyze the probable cause of this alert using the context above and suggest concrete next steps for the on-call engineer.
$tpl$, true, 'milestone 8: assets excerpt slot'
WHERE NOT EXISTS (SELECT 1 FROM llm_prompt_templates WHERE name = 'alert_enrichment' AND version = 2);
SQL

# Assets info source pointing at the mock (milestone 8 D9) + default agent
# role (milestone 8 D8, mirrors legacy behaviour: same template, full tool
# set). Keep in sync with the inline seeding in nix/scripts/smoke-check.sh.
psql "${DATABASE_URL:?}" -v ON_ERROR_STOP=1 <<'SQL'
INSERT INTO assets_configs (name, base_url, token_env, auth_mode, default_schema_name, host_query_template, enabled)
SELECT 'assets-dev', 'http://127.0.0.1:18085/rest/assets/latest', 'ASSETS_TOKEN', 'bearer', 'Capacity CMDB',
       'objectSchema = "Capacity CMDB" AND Name like "{host}"', true
WHERE NOT EXISTS (SELECT 1 FROM assets_configs WHERE name = 'assets-dev');

INSERT INTO llm_agent_roles (name, description, prompt_template_name, tools, enabled, is_default)
SELECT 'default-enricher', 'Default enrichment role', 'alert_enrichment',
       '["cmdb_lookup", "jira_search", "assets_lookup"]'::jsonb, true, true
WHERE NOT EXISTS (SELECT 1 FROM llm_agent_roles WHERE name = 'default-enricher');
SQL

# Default retention config (milestone 5 D10): 30 days raw_events, enabled.
# Keep in sync with the inline seeding in nix/scripts/smoke-check.sh.
psql "${DATABASE_URL:?}" -v ON_ERROR_STOP=1 <<'SQL'
INSERT INTO retention_configs (raw_events_days, enabled)
SELECT 30, true
WHERE NOT EXISTS (SELECT 1 FROM retention_configs);
SQL

# Roles + dev users (milestone 1 D2). Passwords are hashed with the
# pwstore-fast replica (nix/scripts/hash-password.py) so this stays pure SQL.
psql "${DATABASE_URL:?}" -v ON_ERROR_STOP=1 <<'SQL'
INSERT INTO roles (name, privileges) VALUES
    ('admin',  '{view,ack,close,escalate,manage_blackouts,manage_rules,manage_users,manage_sources,admin}'),
    ('sre',    '{view,ack,close,escalate}'),
    ('viewer', '{view}')
ON CONFLICT (name) DO UPDATE SET privileges = EXCLUDED.privileges;
SQL

seed_user() {
    email="$1"; password="$2"; role="$3"
    hash="$(halemans-hash-password "$password")"
    psql "${DATABASE_URL:?}" -v ON_ERROR_STOP=1 \
        -v email="$email" -v hash="$hash" -v role="$role" <<'SQL'
INSERT INTO users (email, password_hash, display_name)
VALUES (:'email', :'hash', :'email')
ON CONFLICT (email) DO UPDATE SET password_hash = EXCLUDED.password_hash;

INSERT INTO user_roles (user_id, role_id)
SELECT u.id, r.id FROM users u, roles r
WHERE u.email = :'email' AND r.name = :'role'
ON CONFLICT (user_id, role_id) DO NOTHING;
SQL
}

seed_user "admin@dev"  "${HALEMANS_ADMIN_PASSWORD:?}"  "admin"
seed_user "sre@dev"    "${HALEMANS_SRE_PASSWORD:?}"    "sre"
seed_user "viewer@dev" "${HALEMANS_VIEWER_PASSWORD:?}" "viewer"

# Demo API token for the sre@dev user (milestone 6 D8): both scopes, plaintext
# stored next to the other dev tokens; the DB only holds the sha256 hash.
api_token_file="${DEVENV_STATE:?}/halemans/api-token"
if [ ! -s "$api_token_file" ]; then
    head -c 32 /dev/urandom | base64 -w0 | tr '+/' '-_' | tr -d '=' > "$api_token_file"
fi
api_token="$(cat "$api_token_file")"
api_token_hash="$(printf %s "$api_token" | sha256sum | cut -d' ' -f1)"
psql "${DATABASE_URL:?}" -v ON_ERROR_STOP=1 \
    -v hash="$api_token_hash" -v prefix="${api_token:0:8}" <<'SQL'
INSERT INTO api_tokens (user_id, name, token_hash, prefix, scopes)
SELECT u.id, 'demo-cli', :'hash', :'prefix', '{alerts:read,metrics}'
FROM users u
WHERE u.email = 'sre@dev'
  AND NOT EXISTS (SELECT 1 FROM api_tokens t WHERE t.user_id = u.id AND t.name = 'demo-cli');
SQL

# Teams + default routing rules (milestone 2 D4/D5). The default notification
# rule reproduces milestone-1 dispatch: severity >= high pages the sre team,
# throttled to 5m.
psql "${DATABASE_URL:?}" -v ON_ERROR_STOP=1 <<'SQL'
INSERT INTO teams (name, description) VALUES
    ('sre', 'Site reliability engineering')
ON CONFLICT (name) DO NOTHING;

INSERT INTO team_members (team_id, user_id, team_role)
SELECT t.id, u.id, 'lead' FROM teams t, users u
WHERE t.name = 'sre' AND u.email = 'sre@dev'
ON CONFLICT (team_id, user_id) DO NOTHING;

INSERT INTO team_members (team_id, user_id, team_role)
SELECT t.id, u.id, 'member' FROM teams t, users u
WHERE t.name = 'sre' AND u.email = 'admin@dev'
ON CONFLICT (team_id, user_id) DO NOTHING;

INSERT INTO on_call_schedules (team_id, members, rotation)
SELECT t.id, jsonb_build_array(u.id::text), '{}'
FROM teams t, users u
WHERE t.name = 'sre' AND u.email = 'sre@dev'
ON CONFLICT (team_id) DO NOTHING;

INSERT INTO notification_rules (position, name, enabled, match, severity_threshold, team_id, channel, channel_config, throttle_seconds)
SELECT 0, 'default-high-severity', true, '{}', 'high', t.id, 'browser_push', '{}', 300
FROM teams t WHERE t.name = 'sre'
  AND NOT EXISTS (SELECT 1 FROM notification_rules WHERE name = 'default-high-severity');

INSERT INTO grouping_rules (position, name, enabled, version, match, group_key_template)
SELECT 0, 'env+host', true, 1, '{}', '{env}/{host}'
WHERE NOT EXISTS (SELECT 1 FROM grouping_rules WHERE name = 'env+host');

-- Team dashboard default (milestone 3 D7): fresh team members land on this
-- template until they save their own dashboard. Idempotent update.
UPDATE teams
SET default_dashboard_config = '[{"env":"dev","filters":{"status":[],"severity":[]}}]'::jsonb
WHERE name = 'sre';
SQL

# Kick off the zabbix polling loop (self-rescheduling job; the job itself
# drops duplicate pending siblings, so re-running seed is safe).
if [ -f "${DEVENV_ROOT:?}/Application/Script/EnqueuePollers.hs" ]; then
    run-script EnqueuePollers
fi

echo "seed-halemans: done"
