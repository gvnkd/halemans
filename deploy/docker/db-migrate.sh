#!/bin/sh
# Applies pending Application/Migration SQL files baked at
# /share/db-migrate/*.sql (<revision>-<description>.sql). Idempotent and safe
# to run from every container start: a session-level advisory lock serializes
# concurrent app/worker boots, and each file is re-checked for a recorded
# revision inside the lock before applying.
#
# Fresh databases are bootstrapped by db-init, which already records every
# baked revision, so this is a no-op there.
set -eu

: "${DATABASE_URL:?}"

# An uninitialized database belongs to db-init, not to migrations.
if [ "$(psql "$DATABASE_URL" -tAc "SELECT to_regclass('public.alerts') IS NOT NULL")" != "t" ]; then
    echo "db-migrate: schema missing, deferring to db-init"
    exit 0
fi

psql "$DATABASE_URL" -q -c "CREATE TABLE IF NOT EXISTS schema_migrations (revision BIGINT NOT NULL UNIQUE)"

pending=""
for f in /share/db-migrate/*.sql; do
    [ -e "$f" ] || continue
    rev="${f##*/}"; rev="${rev%%-*}"
    if [ "$(psql "$DATABASE_URL" -tAc "SELECT 1 FROM schema_migrations WHERE revision = $rev")" != "1" ]; then
        pending="$pending $f"
    fi
done

if [ -z "$pending" ]; then
    echo "db-migrate: up to date"
    exit 0
fi

{
    echo "SELECT pg_advisory_lock(hashtext('halemans-db-migrate'));"
    for f in $pending; do
        rev="${f##*/}"; rev="${rev%%-*}"
        # Re-check inside the lock: a concurrently booting container may
        # have applied the file while we waited.
        echo "SELECT EXISTS (SELECT 1 FROM schema_migrations WHERE revision = $rev) AS applied \\gset"
        echo "\\if :applied"
        echo "\\else"
        echo "\\echo 'db-migrate: applying $f'"
        echo "\\i $f"
        echo "INSERT INTO schema_migrations (revision) VALUES ($rev);"
        echo "\\endif"
    done
    echo "SELECT pg_advisory_unlock(hashtext('halemans-db-migrate'));"
} | psql "$DATABASE_URL" -v ON_ERROR_STOP=1 -q
echo "db-migrate: done"
