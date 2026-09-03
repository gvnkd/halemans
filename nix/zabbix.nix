# Zabbix via OCI containers (docker compose), wrapped in a devenv process
# (milestone 0, doc §3.1). Isolated postgres container for the zabbix DB;
# dev-only credentials are fixed and local (the real credential is the
# runtime-generated API token under .devenv/state/zabbix/token, gitignored).
{ pkgs, lib, config, halemansLib, ... }:
let
    composeFile = pkgs.writeText "zabbix-compose.yml" ''
        name: halemans-zabbix
        services:
          zabbix-db:
            image: postgres:16-alpine
            environment:
              POSTGRES_USER: zabbix
              POSTGRES_PASSWORD: zabbix
              POSTGRES_DB: zabbix
            volumes:
              - zabbix-dbdata:/var/lib/postgresql/data
            healthcheck:
              test: ["CMD-SHELL", "pg_isready -U zabbix"]
              interval: 5s
              timeout: 5s
              retries: 20
          zabbix-server:
            image: zabbix/zabbix-server-pgsql:alpine-7.0-latest
            environment:
              DB_SERVER_HOST: zabbix-db
              POSTGRES_USER: zabbix
              POSTGRES_PASSWORD: zabbix
              POSTGRES_DB: zabbix
            ports:
              - "127.0.0.1:10051:10051"
            depends_on:
              zabbix-db:
                condition: service_healthy
          zabbix-web:
            image: zabbix/zabbix-web-nginx-pgsql:alpine-7.0-latest
            environment:
              ZBX_SERVER_HOST: zabbix-server
              DB_SERVER_HOST: zabbix-db
              POSTGRES_USER: zabbix
              POSTGRES_PASSWORD: zabbix
              POSTGRES_DB: zabbix
              PHP_TZ: UTC
            ports:
              - "127.0.0.1:10080:8080"
            depends_on:
              zabbix-db:
                condition: service_healthy
              zabbix-server:
                condition: service_started
          zabbix-agent:
            image: zabbix/zabbix-agent:alpine-7.0-latest
            environment:
              ZBX_SERVER_HOST: zabbix-server
              ZBX_HOSTNAME: dev-host-01
            depends_on:
              zabbix-server:
                condition: service_started
        volumes:
          zabbix-dbdata:
    '';

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
in
{
    packages = [ seedZabbix fireTestAlertZabbix ];

    processes.zabbix-compose = {
        exec = "exec docker compose -f ${composeFile} up";
        # Readiness of the JSON-RPC API is awaited inside seed-zabbix; the
        # container stack takes ~1min on first boot (db init + schema import).
    };
}
