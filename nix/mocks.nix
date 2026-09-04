# Mock Confluence + Jira HTTP servers for enrichment dev fixtures
# (milestone 3 D9, doc §10). Pure-stdlib python; tokens come from
# .devenv/state/halemans/env.sh (halemans-ensure-tokens).
{ pkgs, lib, config, halemansLib, ... }:
let
    mockConfluence = pkgs.writeShellApplication {
        name = "mock-confluence";
        runtimeInputs = [ pkgs.python3 halemansLib.ensureTokens ];
        text = ''
            halemans-ensure-tokens
            # shellcheck disable=SC1091
            source "''${DEVENV_STATE:?}/halemans/env.sh"
            exec python3 ${./mocks/mock_confluence.py}
        '';
    };

    mockJira = pkgs.writeShellApplication {
        name = "mock-jira";
        runtimeInputs = [ pkgs.python3 halemansLib.ensureTokens ];
        text = ''
            halemans-ensure-tokens
            # shellcheck disable=SC1091
            source "''${DEVENV_STATE:?}/halemans/env.sh"
            exec python3 ${./mocks/mock_jira.py}
        '';
    };
in
{
    packages = [ mockConfluence mockJira ];

    processes.mock-confluence = {
        exec = "${mockConfluence}/bin/mock-confluence";
        process-compose = {
            readiness_probe.http_get = {
                host = "127.0.0.1";
                port = 18082;
                path = "/health";
            };
        };
    };

    processes.mock-jira = {
        exec = "${mockJira}/bin/mock-jira";
        process-compose = {
            readiness_probe.http_get = {
                host = "127.0.0.1";
                port = 18083;
                path = "/health";
            };
        };
    };
}
