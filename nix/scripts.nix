# Devenv scripts for the Halemans stack (milestone 0, doc §7) plus the
# one-shot `seed` process that gates app/worker startup (doc §5).
{ pkgs, lib, config, halemansLib, ... }:
let
    seedHalemans = pkgs.writeShellApplication {
        name = "seed-halemans";
        runtimeInputs = [ pkgs.postgresql pkgs.coreutils halemansLib.ensureTokens halemansLib.hashPassword ];
        text = builtins.readFile ./scripts/seed-halemans.sh;
    };

    seed = pkgs.writeShellApplication {
        name = "seed";
        runtimeInputs = [ seedHalemans ];
        text = ''
            set -euo pipefail
            marker="''${DEVENV_STATE:?}/halemans/seed.done"
            rm -f "$marker"
            halemans-ensure-tokens
            seed-halemans
            seed-zabbix
            seed-grafana
            touch "$marker"
            echo "seed: all sources provisioned"
        '';
    };

    fireTestAlertAlertmanager = pkgs.writeShellApplication {
        name = "fire-test-alert-alertmanager";
        runtimeInputs = [ pkgs.curl pkgs.jq ];
        text = builtins.readFile ./scripts/fire-test-alert-alertmanager.sh;
    };

    stackStatus = pkgs.writeShellApplication {
        name = "stack-status";
        runtimeInputs = [ pkgs.curl pkgs.postgresql ];
        text = builtins.readFile ./scripts/stack-status.sh;
    };

    smokeTest = pkgs.writeShellApplication {
        name = "smoke-test";
        runtimeInputs = [ pkgs.curl pkgs.jq pkgs.postgresql pkgs.coreutils ];
        text = ''
            set -euo pipefail
            exec bash "''${DEVENV_ROOT:?}/tests/smoke/run.sh" "$@"
        '';
    };
    # Wrapper env for app processes: shared hook tokens + source API tokens
    # (written by seed-zabbix/seed-grafana) are exported before start.
    # mkForce overrides the plain "start"/"start-worker" from the IHP module.
    # App/worker wait for the seed marker file (webhook tokens must exist in
    # the DB before receivers point at them, doc §5). A marker file is used
    # instead of a process-compose depends_on because the devenv 2.0 tasks
    # wrapper never reports a finished one-shot process as completed.
    waitForSeed = ''
        for i in $(seq 1 600); do
            [ -f "$DEVENV_STATE/halemans/seed.done" ] && break
            sleep 1
        done
        [ -f "$DEVENV_STATE/halemans/seed.done" ] || { echo "seed did not complete in time" >&2; exit 1; }
    '';
in
{
    packages = [ seed seedHalemans fireTestAlertAlertmanager stackStatus smokeTest ];

    processes.web.exec = lib.mkForce ''
        halemans-ensure-tokens
        source "$DEVENV_STATE/halemans/env.sh"
        ${waitForSeed}
        [ -f "$DEVENV_STATE/zabbix/token" ] && export ZABBIX_TOKEN="$(cat "$DEVENV_STATE/zabbix/token")"
        [ -f "$DEVENV_STATE/grafana/token" ] && export GRAFANA_TOKEN="$(cat "$DEVENV_STATE/grafana/token")"
        exec start
    '';
    processes.worker.exec = lib.mkForce ''
        halemans-ensure-tokens
        source "$DEVENV_STATE/halemans/env.sh"
        ${waitForSeed}
        [ -f "$DEVENV_STATE/zabbix/token" ] && export ZABBIX_TOKEN="$(cat "$DEVENV_STATE/zabbix/token")"
        [ -f "$DEVENV_STATE/grafana/token" ] && export GRAFANA_TOKEN="$(cat "$DEVENV_STATE/grafana/token")"
        exec start-worker
    '';

    processes.seed = {
        exec = "${seed}/bin/seed";
        process-compose = {
            availability.restart = "no";
            depends_on = {
                postgres.condition = "process_healthy";
                zabbix-web.condition = "process_healthy";
                grafana.condition = "process_healthy";
                alertmanager.condition = "process_healthy";
                mock-confluence.condition = "process_healthy";
                mock-jira.condition = "process_healthy";
                mock-llm.condition = "process_healthy";
                mock-assets.condition = "process_healthy";
            };
        };
    };
}
