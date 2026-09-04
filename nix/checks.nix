# checks.smoke — milestone 0 §6: boots an isolated instance of the full
# Halemans stack (postgres + zabbix + grafana + alertmanager + app + jobs
# worker) inside the nix build sandbox and runs the smoke suite against it.
# All sources are native nixpkgs builds, so no docker is needed and the suite
# covers all three sources. Loopback networking works inside the sandbox, so
# the check uses the same ports as the dev stack without conflicts.
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
            pkgs.zabbix70.server-pgsql
            pkgs.zabbix70.agent
            pkgs.php
        ];

        # Referenced by nix/scripts/smoke-check.sh
        HALEMANS_PROFILE = shell.devenv.profile;
        RUN_PROD_SERVER = "${prodServer}/bin/RunProdServer";
        RUN_JOBS = "${prodServer}/bin/RunJobs";
        GRAFANA_INI = halemansLib.grafanaIni;
        GRAFANA_HOME = "${pkgs.grafana}/share/grafana";
        AM_TEMPLATE = halemansLib.alertmanagerConfigTemplate;
        ZABBIX_SERVER_TEMPLATE = halemansLib.zabbixServerConfTemplate;
        ZABBIX_WEB_TEMPLATE = halemansLib.zabbixWebConfTemplate;
        ZABBIX_AGENT_TEMPLATE = halemansLib.zabbixAgentConfTemplate;
        ZABBIX_SERVER_BIN = "${pkgs.zabbix70.server-pgsql}/sbin/zabbix_server";
        ZABBIX_WEB_DIR = "${pkgs.zabbix70.web}/share/zabbix";
        ZABBIX_AGENT_BIN = "${pkgs.zabbix70.agent}/bin/zabbix_agentd";
        PHP_BIN = "${pkgs.php}/bin/php";
        IHP_SCHEMA = "${config.packages.ihp-schema}/IHPSchema.sql";
        APP_SCHEMA = "${config.packages.schema}/Schema.sql";
        APP_FIXTURES = "${self}/Application/Fixtures.sql";
        SMOKE_RUN = "${self}/tests/smoke/run.sh";

        buildPhase = builtins.readFile ./scripts/smoke-check.sh;
    };
}
