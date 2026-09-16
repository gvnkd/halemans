# syntax=docker/dockerfile:1
# Builds Halemans without nix, on top of the precompiled deps image
# (docker/Dockerfile.deps). Produces the same container contract as the
# nix-built image: /bin/{RunProdServer,RunJobs,EnqueuePollers,GenPassword,
# db-init,db-migrate}, /share/{db-init,db-migrate}, /app/static.
#
#   docker build -f docker/Dockerfile.deps -t halemans-deps .
#   docker build -f docker/Dockerfile -t halemans .
ARG DEPS_IMAGE=halemans-deps
FROM ${DEPS_IMAGE} AS build

# PostgreSQL 18 for the compile-time database (typedSql Template Haskell
# introspects a scratch database during the build, mirroring
# buildTimePostgresSetup in IHP's NixSupport/default.nix). PGDG_REPO may
# point at a local mirror of apt.postgresql.org in isolated environments.
ARG PGDG_REPO="http://apt.postgresql.org/pub/repos/apt"
ARG PGDG_KEY_URL="https://www.postgresql.org/media/keys/ACCC4CF8.asc"

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends gnupg \
    && curl -fsSL ${PGDG_KEY_URL} | gpg --dearmor -o /usr/share/keyrings/pgdg.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/pgdg.gpg] ${PGDG_REPO} trixie-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends postgresql-18 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src
COPY . .

RUN sh docker/prepare-cabal-build.sh

RUN install -d -m 777 /var/lib/halemans-pg/sock \
    && install -d -o postgres -g postgres /var/lib/halemans-pg/data \
    && su -s /bin/sh postgres -c "/usr/lib/postgresql/18/bin/initdb -D /var/lib/halemans-pg/data --no-locale --encoding=UTF8 -A trust" >/dev/null \
    && echo "unix_socket_directories = '/var/lib/halemans-pg/sock'" >> /var/lib/halemans-pg/data/postgresql.conf \
    && echo "listen_addresses = ''" >> /var/lib/halemans-pg/data/postgresql.conf \
    && su -s /bin/sh postgres -c "/usr/lib/postgresql/18/bin/pg_ctl -D /var/lib/halemans-pg/data -l /var/lib/halemans-pg/data/pg.log -w start" \
    && su -s /bin/sh postgres -c "/usr/lib/postgresql/18/bin/createdb -h /var/lib/halemans-pg/sock app" \
    && su -s /bin/sh postgres -c "/usr/lib/postgresql/18/bin/createuser -h /var/lib/halemans-pg/sock -s root" \
    && su -s /bin/sh postgres -c "/usr/lib/postgresql/18/bin/psql -h /var/lib/halemans-pg/sock -d app -q -f /src/vendor/ihp/ihp-schema-compiler/data/IHPSchema.sql" \
    && su -s /bin/sh postgres -c "/usr/lib/postgresql/18/bin/psql -h /var/lib/halemans-pg/sock -d app -q -f /src/Application/Schema.sql" \
    && DATABASE_URL="postgresql:///app?host=/var/lib/halemans-pg/sock" cabal build all -j$(nproc) \
    ; su -s /bin/sh postgres -c "/usr/lib/postgresql/18/bin/pg_ctl -D /var/lib/halemans-pg/data stop -m fast"

RUN mkdir -p /out/bin \
    && for exe in RunProdServer RunJobs EnqueuePollers GenPassword; do \
        cp "$(cabal list-bin exe:${exe})" /out/bin/${exe}; \
    done \
    && strip /out/bin/*

# db-init bundle (mirrors the dbInit derivation in flake.nix): framework
# schema, app schema, roles, and the schema_migrations ledger for every
# migration already folded into Schema.sql.
RUN mkdir -p /out/share/db-init /out/share/db-migrate \
    && cp vendor/ihp/ihp-schema-compiler/data/IHPSchema.sql /out/share/db-init/00-ihp-schema.sql \
    && cp Application/Schema.sql /out/share/db-init/01-app-schema.sql \
    && cp deploy/docker/roles.sql /out/share/db-init/02-roles.sql \
    && { echo "CREATE TABLE IF NOT EXISTS schema_migrations (revision BIGINT NOT NULL UNIQUE)"';' ; \
         echo "INSERT INTO schema_migrations (revision) VALUES" ; \
         ls Application/Migration/*.sql | xargs -n1 basename | sed -n 's/^\([0-9][0-9]*\)-.*/(\1)/p' | paste -sd, - ; \
         echo "ON CONFLICT DO NOTHING"';' ; } > /out/share/db-init/99-schema-migrations.sql \
    && cp Application/Migration/*.sql /out/share/db-migrate/

# Static assets: app static/ plus IHP's own data/static (its vendor/
# subtree provides flatpickr, timeago, popper, morphdom, turbolinks).
RUN mkdir -p /out/app/static /out/app/ihp-static \
    && cp -r static/. /out/app/static/ \
    && cp -r vendor/ihp/ihp/data/static/. /out/app/ihp-static/ \
    && cp -r vendor/ihp/ihp/data/static/vendor/. /out/app/static/vendor/

FROM debian:13-slim

ARG PGDG_REPO="http://apt.postgresql.org/pub/repos/apt"
ARG PGDG_KEY_URL="https://www.postgresql.org/media/keys/ACCC4CF8.asc"
ARG ASSET_VERSION=dev

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
        libgmp10 libnuma1 zlib1g ca-certificates curl gnupg \
        libssl3 libicu76 libatomic1 libbz2-1.0 liblzma5 libelf1 libdw1 \
        libffi8 libzstd1 libstdc++6 libgcc-s1 \
    && curl -fsSL ${PGDG_KEY_URL} | gpg --dearmor -o /usr/share/keyrings/pgdg.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/pgdg.gpg] ${PGDG_REPO} trixie-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends libpq5 postgresql-client-18 \
    && apt-get purge -y gnupg \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /out/bin/ /bin/
COPY --from=build /out/share/ /share/
COPY --from=build /out/app/static/ /app/static/
COPY --from=build /out/app/ihp-static/ /app/ihp-static/
COPY --chmod=755 deploy/docker/db-init.sh /bin/db-init
COPY --chmod=755 deploy/docker/db-migrate.sh /bin/db-migrate

WORKDIR /app
ENV APP_STATIC=/app/static \
    IHP_STATIC=/app/ihp-static \
    IHP_ASSET_VERSION=${ASSET_VERSION} \
    PORT=8000
EXPOSE 8000
CMD ["/bin/RunProdServer"]
