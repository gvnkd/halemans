# devenv shell module for the Halemans dev/test stack (milestone 0).
# Imported by the thin root flake.nix. See design_docs/milestone_0.md.
{ pkgs, lib, config, ... }:
let
    halemansLib = import ./lib.nix { inherit pkgs; };
in
{
    imports = [
        ./postgres.nix
        ./alertmanager.nix
        ./grafana.nix
        ./zabbix.nix
        ./mocks.nix
        ./scripts.nix
    ];

    _module.args.halemansLib = halemansLib;

    packages = with pkgs; [
        curl
        jq
        coreutils
        halemansLib.ensureTokens
    ];

    # Port map (avoid clashes with IHP tooling :8001/:8002 and local Taiga :8000):
    #   halemans app : 28080
    #   grafana      : 3001
    #   alertmanager : 9093
    #   zabbix web   : 10080
    #   zabbix server/trapper : 10051
    #   mock-confluence : 18082
    #   mock-jira       : 18083
    #   mock-llm        : 18084
    env = {
        HALEMANS_GRAFANA_URL = "http://127.0.0.1:3001";
        HALEMANS_ALERTMANAGER_URL = "http://127.0.0.1:9093";
        HALEMANS_ZABBIX_URL = "http://127.0.0.1:10080";
        HALEMANS_APP_URL = "http://127.0.0.1:28080";
        # IHP FrameworkConfig picks up PORT for the dev server.
        PORT = "28080";
    };
}
