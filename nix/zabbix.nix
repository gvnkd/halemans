# Zabbix as native devenv processes (no docker): zabbix-server + zabbix-web
# (PHP frontend, serves the JSON-RPC API) + zabbix-agent, all from nixpkgs
# zabbix70. The zabbix DB lives in the devenv postgres as a separate `zabbix`
# database; schema import is idempotent and runs in the server wrapper.
{ pkgs, lib, config, halemansLib, ... }:
let
    zabbixServer = pkgs.zabbix70.server-pgsql;
    zabbixWeb = pkgs.zabbix70.web;
    zabbixAgent = pkgs.zabbix70.agent;
    php = pkgs.php;
    schemaDir = "${zabbixServer}/share/zabbix/database/postgresql";

    seedZabbix = pkgs.writeShellApplication {
        name = "seed-zabbix";
        runtimeInputs = [ pkgs.curl pkgs.jq pkgs.coreutils ];
        text = builtins.readFile ./scripts/seed-zabbix.sh;
    };

    fireTestAlertZabbix = pkgs.writeShellApplication {
        name = "fire-test-alert-zabbix";
        runtimeInputs = [ pkgs.curl pkgs.jq ];
        text = builtins.readFile ./scripts/fire-test-alert-zabbix.sh;
    };

    # Imports the zabbix DB schema into the `zabbix` database of the local
    # postgres, idempotently. Used by the server wrapper (dev) and checks.smoke.
    ensureZabbixDb = pkgs.writeShellApplication {
        name = "halemans-zabbix-db-init";
        runtimeInputs = [ pkgs.postgresql ];
        text = ''
            set -euo pipefail
            # shellcheck disable=SC2034
            for i in $(seq 1 60); do
                pg_isready -q -h "''${PGHOST:?}" 2>/dev/null && break
                sleep 1
            done
            pg_isready -q -h "$PGHOST"
            psql -h "$PGHOST" -d postgres -tA -c "SELECT 1 FROM pg_database WHERE datname='zabbix'" \
                | grep -q 1 || createdb -h "$PGHOST" zabbix
            if [ -z "$(psql -h "$PGHOST" -d zabbix -tA -c "SELECT 1 FROM pg_tables WHERE tablename='dbversion'" 2>/dev/null)" ]; then
                psql -h "$PGHOST" -d zabbix -v ON_ERROR_STOP=1 -q \
                    -f ${schemaDir}/schema.sql -f ${schemaDir}/images.sql -f ${schemaDir}/data.sql
                echo "zabbix db schema imported"
            fi
        '';
    };

    # Renders @PGHOST@/@PGPORT@/@DBUSER@/@STATE@ templates into $DEVENV_STATE/zabbix/.
    renderZabbixConfigs = ''
        state="$DEVENV_STATE/zabbix"
        mkdir -p "$state/sock"
        dbuser=$(psql -h "$PGHOST" -d postgres -tA -c "SELECT current_user")
        sed -e "s|@PGHOST@|$PGHOST|g" -e "s|@PGPORT@|$PGPORT|g" \
            -e "s|@DBUSER@|$dbuser|g" -e "s|@STATE@|$state|g" \
            ${halemansLib.zabbixServerConfTemplate} > "$state/zabbix_server.conf"
        sed -e "s|@PGHOST@|$PGHOST|g" -e "s|@PGPORT@|$PGPORT|g" -e "s|@DBUSER@|$dbuser|g" \
            ${halemansLib.zabbixWebConfTemplate} > "$state/zabbix.conf.php"
        sed -e "s|@STATE@|$state|g" \
            ${halemansLib.zabbixAgentConfTemplate} > "$state/zabbix_agentd.conf"
    '';
in
{
    packages = [ seedZabbix fireTestAlertZabbix ensureZabbixDb ];

    processes.zabbix-server = {
        exec = ''
            ${renderZabbixConfigs}
            halemans-zabbix-db-init
            exec ${zabbixServer}/sbin/zabbix_server -c "$DEVENV_STATE/zabbix/zabbix_server.conf" -f
        '';
        process-compose = {
            depends_on.postgres.condition = "process_healthy";
        };
    };

    # PHP built-in server for the frontend/API. Waits for the zabbix DB so the
    # readiness probe implies "API + DB ready" for seed-zabbix.
    processes.zabbix-web = {
        exec = ''
            ${renderZabbixConfigs}
            until psql -h "$PGHOST" -d zabbix -tA -c "SELECT 1 FROM dbversion" > /dev/null 2>&1; do
                sleep 1
            done
            export ZABBIX_CONFIG="$DEVENV_STATE/zabbix/zabbix.conf.php"
            exec ${php}/bin/php \
                -d memory_limit=256M \
                -d max_execution_time=300 \
                -d max_input_time=300 \
                -d post_max_size=16M \
                -d date.timezone=UTC \
                -S 127.0.0.1:10080 -t ${zabbixWeb}/share/zabbix
        '';
        process-compose = {
            depends_on.postgres.condition = "process_healthy";
            readiness_probe.http_get = {
                host = "127.0.0.1";
                port = 10080;
                path = "/";
            };
        };
    };

    processes.zabbix-agent = {
        exec = ''
            ${renderZabbixConfigs}
            exec ${zabbixAgent}/bin/zabbix_agentd -c "$DEVENV_STATE/zabbix/zabbix_agentd.conf" -f
        '';
        process-compose = {
            depends_on.zabbix-server.condition = "process_started";
        };
    };
}
