# PostgreSQL for the Halemans app DB.
# The IHP flake module enables services.postgres; flake.nix overrides its
# hardcoded `app` database to `halemans` (initialDatabases + env.DATABASE_URL /
# PGDATABASE) and loads Application/Schema.sql + Application/Fixtures.sql into
# it. This module only carries Halemans-specific tweaks on top.
{ pkgs, lib, config, ... }:
{
    # Nothing to override yet. Kept as the home for future Halemans-specific
    # postgres settings (extensions, tuning) so all nix code stays in ./nix/.
}
