#!/bin/sh
# Applies the baked schema bundle (/share/db-init) to $DATABASE_URL unless the
# schema is already present. Fresh-deploy bootstrap; upgrades apply new
# Application/Migration SQL by hand.
set -eu

if [ "$(psql "$DATABASE_URL" -tAc "SELECT to_regclass('public.alerts') IS NOT NULL")" = "t" ]; then
    echo "db-init: schema already present, skipping"
    exit 0
fi

for f in /share/db-init/*.sql; do
    echo "db-init: applying $f"
    psql "$DATABASE_URL" -v ON_ERROR_STOP=1 -q -f "$f"
done
echo "db-init: done"
