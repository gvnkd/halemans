# checks.smoke — milestone 0 §6: boots an isolated instance of the full
# Halemans stack (postgres + zabbix + grafana + alertmanager + app + jobs
# worker) inside the nix build sandbox and runs the smoke suite against it.
# All sources are native nixpkgs builds, so no docker is needed and the suite
# covers all three sources. Loopback networking works inside the sandbox, so
# the check uses the same ports as the dev stack without conflicts.
{ pkgs, lib, config, self, ihpLib }:
let
    shell = config.devenv.shells.default;
    halemansLib = import ./lib.nix { inherit pkgs; };
    prodServer = config.packages."unoptimized-prod-server";
in
{
    # Override of the IHP flake module's auto-generated integration-tests
    # check: the module's version never applies the schema to its temp
    # database, so typedSql compile-time introspection fails on any spec that
    # imports app modules. This one loads IHPSchema + Schema + Fixtures first.
    "integration-tests" = lib.mkForce (pkgs.stdenv.mkDerivation {
        name = "${config.ihp.appName}-integration-tests";
        src = builtins.path { path = config.ihp.projectPath; name = "source"; };
        nativeBuildInputs = with pkgs; [
            (config.ihp.ghcCompiler.ghcWithPackages (p: config.ihp.haskellPackages p ++ config.ihp.devHaskellPackages p ++ [p.ihp-ide p.ihp-schema-compiler]))
            gnumake
            postgresql
        ];
        buildPhase = ''
            export IHP_LIB=${ihpLib}

            export PGDATA="$TMPDIR/pgdata"
            export PGHOST="$TMPDIR/pghost"
            mkdir -p "$PGHOST"
            initdb -D "$PGDATA" --no-locale --encoding=UTF8
            echo "unix_socket_directories = '$PGHOST'" >> "$PGDATA/postgresql.conf"
            echo "listen_addresses = '''" >> "$PGDATA/postgresql.conf"
            pg_ctl -D "$PGDATA" -l "$TMPDIR/pg.log" start

            createdb -h "$PGHOST" app
            export DATABASE_URL="postgresql:///app?host=$PGHOST"

            psql -h "$PGHOST" -d app -v ON_ERROR_STOP=1 -q \
                -f ${config.packages.ihp-schema}/IHPSchema.sql \
                -f Application/Schema.sql \
                -f Application/Fixtures.sql

            make -f $IHP_LIB/lib/IHP/Makefile.dist build/Generated/Types.hs

            # shellcheck disable=SC2046
            runghc $(make -f $IHP_LIB/lib/IHP/Makefile.dist print-ghc-extensions) -i. -ibuild -iConfig Test/Integration.hs

            pg_ctl -D "$PGDATA" stop || true
            touch $out
        '';
        installPhase = "true";
    });

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
            halemansLib.hashPassword
            (pkgs.python3.withPackages (p: [ p.playwright ]))
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
        PLAYWRIGHT_SUITE = "${self}/tests/playwright/run.py";
        PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright-driver.browsers}";
        # chromium FATALs without fontconfig (SkFontMgr_FontConfigInterface)
        FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = [ pkgs.dejavu_fonts ]; };

        buildPhase = builtins.readFile ./scripts/smoke-check.sh;
    };
}
