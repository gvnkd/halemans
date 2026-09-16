# PostgreSQL for the Halemans app DB.
# The IHP flake module enables services.postgres; flake.nix overrides its
# hardcoded `app` database to `halemans` (initialDatabases + env.DATABASE_URL /
# PGDATABASE) and loads Application/Schema.sql + Application/Fixtures.sql into
# it. This module only carries Halemans-specific tweaks on top.
{ pkgs, lib, config, ... }:
{
    # Pin the dev/test database to PostgreSQL 18 (matches the container
    # deployment, deploy/docker/docker-compose.yaml). Existing devenv
    # datadirs are PG17-format: `devenv down`, remove the postgres state
    # dir under $DEVENV_STATE, then `devenv up` to reinitialise and re-seed.
    services.postgres.package = pkgs.postgresql_18;
}
