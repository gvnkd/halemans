# checks.smoke — milestone 0 §6: boots an isolated instance of the Halemans
# stack (postgres + grafana + alertmanager + app + jobs worker) inside the
# nix build sandbox and runs the smoke suite against it.
#
# Zabbix is excluded here (SMOKE_ZABBIX=0): it runs in docker, and the docker
# daemon socket is unreachable from the nix sandbox. The full three-source
# suite runs against the dev stack via `smoke-test` (doc §7).
#
# Loopback networking works inside the sandbox, so the check uses the same
# ports as the dev stack (28080/3001/9093) without conflicts.
{ pkgs, lib, config, self }:
let
    shell = config.devenv.shells.default;
    halemansLib = import ./lib.nix { inherit pkgs; };
    prodServer = config.packages."unoptimized-prod-server";
in
{
    smoke = pkgs.stdenvNoCC.mkDerivation {
        name = "halemans-smoke";

        # Re-run whenever any tracked source file changes.
        srcHash = self.outPath;

        dontUnpack = true;
        dontInstall = true;

        nativeBuildInputs = [
            pkgs.curl
            pkgs.jq
            pkgs.postgresql
            pkgs.coreutils
            pkgs.gnused
            pkgs.grafana
            pkgs.prometheus-alertmanager
        ];

        # Referenced by nix/scripts/smoke-check.sh
        HALEMANS_PROFILE = shell.devenv.profile;
        RUN_PROD_SERVER = "${prodServer}/bin/RunProdServer";
        RUN_JOBS = "${prodServer}/bin/RunJobs";
        GRAFANA_INI = halemansLib.grafanaIni;
        GRAFANA_HOME = "${pkgs.grafana}/share/grafana";
        AM_TEMPLATE = halemansLib.alertmanagerConfigTemplate;
        IHP_SCHEMA = "${config.packages.ihp-schema}/IHPSchema.sql";
        APP_SCHEMA = "${config.packages.schema}/Schema.sql";
        APP_FIXTURES = "${self}/Application/Fixtures.sql";
        SMOKE_RUN = "${self}/tests/smoke/run.sh";

        buildPhase = builtins.readFile ./scripts/smoke-check.sh;
    };
}
