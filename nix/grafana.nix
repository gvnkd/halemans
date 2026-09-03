# Grafana as a native devenv process (milestone 0, doc §3.2).
# Data/config under .devenv/state/grafana. Datasource, contact point and
# notification policy are file-provisioned (nix/lib.nix); the dev
# service-account token and the dev-cpu-sim alert rule are created
# idempotently by seed-grafana (API), because both need runtime values
# (generated token / writable rule for fire-test-alert-grafana to flip
# deterministically).
{ pkgs, lib, config, halemansLib, ... }:
let
    seedGrafana = pkgs.writeShellApplication {
        name = "seed-grafana";
        runtimeInputs = [ pkgs.curl pkgs.jq pkgs.coreutils ];
        text = builtins.readFile ./scripts/seed-grafana.sh;
    };

    fireTestAlertGrafana = pkgs.writeShellApplication {
        name = "fire-test-alert-grafana";
        runtimeInputs = [ pkgs.curl pkgs.jq ];
        text = builtins.readFile ./scripts/fire-test-alert-grafana.sh;
    };
in
{
    packages = [ seedGrafana fireTestAlertGrafana ];

    processes.grafana = {
        exec = ''
            halemans-ensure-tokens
            source "$DEVENV_STATE/halemans/env.sh"
            mkdir -p "$DEVENV_STATE/grafana/data"
            exec ${pkgs.grafana}/bin/grafana server \
                --homepath ${pkgs.grafana}/share/grafana \
                --config ${halemansLib.grafanaIni}
        '';
        process-compose = {
            readiness_probe.http_get = {
                host = "127.0.0.1";
                port = 3001;
                path = "/api/health";
            };
        };
    };
}
