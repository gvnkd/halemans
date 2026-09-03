# Alertmanager as a native devenv process (milestone 0, doc §3.3).
# Config is rendered at process start because the Halemans webhook token is a
# runtime-generated dev secret under .devenv/state/halemans/.
{ pkgs, lib, config, halemansLib, ... }:
{
    processes.alertmanager = {
        exec = ''
            halemans-ensure-tokens
            source "$DEVENV_STATE/halemans/env.sh"
            cfg="$DEVENV_STATE/alertmanager"
            mkdir -p "$cfg/data"
            sed "s|@HALEMANS_AM_HOOK_TOKEN@|$HALEMANS_AM_HOOK_TOKEN|g" \
                ${halemansLib.alertmanagerConfigTemplate} > "$cfg/alertmanager.yml"
            exec ${pkgs.prometheus-alertmanager}/bin/alertmanager \
                --config.file="$cfg/alertmanager.yml" \
                --storage.path="$cfg/data" \
                --web.listen-address=127.0.0.1:9093 \
                --cluster.listen-address=""
        '';
        process-compose = {
            readiness_probe.http_get = {
                host = "127.0.0.1";
                port = 9093;
                path = "/-/healthy";
            };
        };
    };
}
