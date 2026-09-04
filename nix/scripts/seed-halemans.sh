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

# Kick off the zabbix polling loop (self-rescheduling job; the job itself
# drops duplicate pending siblings, so re-running seed is safe).
if [ -f "${DEVENV_ROOT:?}/Application/Script/EnqueuePollers.hs" ]; then
    run-script EnqueuePollers
fi

echo "seed-halemans: done"
