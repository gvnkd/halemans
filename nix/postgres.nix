# PostgreSQL for the Halemans app DB.
# The IHP flake module already enables services.postgres, creates the `app`
# database and loads Application/Schema.sql + Application/Fixtures.sql into it.
# This module only carries Halemans-specific tweaks on top.
{ pkgs, lib, config, ... }:
{
    # Nothing to override yet. Kept as the home for future Halemans-specific
    # postgres settings (extensions, tuning) so all nix code stays in ./nix/.
}
