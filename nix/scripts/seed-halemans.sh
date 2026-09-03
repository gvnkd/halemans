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

# Kick off the zabbix polling loop (self-rescheduling job; the job itself
# drops duplicate pending siblings, so re-running seed is safe).
if [ -f "${DEVENV_ROOT:?}/Application/Script/EnqueuePollers.hs" ]; then
    run-script EnqueuePollers
fi

echo "seed-halemans: done"
