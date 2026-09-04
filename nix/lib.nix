# Shared nix values used by both the devenv modules and checks.smoke:
# token helper, alertmanager config template, grafana ini + provisioning.
{ pkgs }:
let
    ensureTokens = pkgs.writeShellApplication {
        name = "halemans-ensure-tokens";
        runtimeInputs = [ pkgs.coreutils ];
        text = ''
            state="''${DEVENV_STATE:?}/halemans"
            mkdir -p "$state"
            gen() { od -An -N32 -tx1 /dev/urandom | tr -d ' \n'; }
            [ -f "$state/am-hook-token" ]      || gen > "$state/am-hook-token"
            [ -f "$state/generic-hook-token" ] || gen > "$state/generic-hook-token"
            {
                printf 'export HALEMANS_AM_HOOK_TOKEN="%s"\n'      "$(cat "$state/am-hook-token")"
                printf 'export HALEMANS_GENERIC_HOOK_TOKEN="%s"\n' "$(cat "$state/generic-hook-token")"
            } > "$state/env.sh"
        '';
    };

    # @HALEMANS_AM_HOOK_TOKEN@ is substituted at process start (runtime secret).
    alertmanagerConfigTemplate = pkgs.writeText "alertmanager.yml.tpl" ''
        global:
          resolve_timeout: 1m
        route:
          receiver: halemans
          group_by: ['alertname', 'env', 'host']
          group_wait: 1s
          group_interval: 5s
          repeat_interval: 1h
        receivers:
          - name: halemans
            webhook_configs:
              - url: "http://127.0.0.1:28080/hooks/alertmanager/@HALEMANS_AM_HOOK_TOKEN@"
                send_resolved: true
    '';

    grafanaIni = pkgs.writeText "grafana.ini" ''
        [paths]
        data = $__env{DEVENV_STATE}/grafana/data
        logs = $__env{DEVENV_STATE}/grafana/logs
        plugins = $__env{DEVENV_STATE}/grafana/plugins
        provisioning = ${grafanaProvisioning}
        [server]
        http_addr = 127.0.0.1
        http_port = 3001
        [security]
        admin_user = admin
        admin_password = admin
        [analytics]
        reporting_enabled = false
        [log]
        level = warn
        [unified_alerting]
        enabled = true
    '';

    grafanaProvisioning = pkgs.linkFarm "grafana-provisioning" [
        { name = "datasources/testdata.yaml"; path = datasourcesYaml; }
        { name = "alerting/contactpoints.yaml"; path = contactpointsYaml; }
        { name = "alerting/policies.yaml"; path = policiesYaml; }
    ];

    datasourcesYaml = pkgs.writeText "grafana-datasources.yaml" ''
        apiVersion: 1
        datasources:
          - name: TestData
            type: testdata
            uid: testdata
            access: proxy
    '';

    # $HALEMANS_GENERIC_HOOK_TOKEN is expanded by grafana from the process env.
    contactpointsYaml = pkgs.writeText "grafana-contactpoints.yaml" ''
        apiVersion: 1
        contactPoints:
          - orgId: 1
            name: halemans
            receivers:
              - uid: dev-halemans-webhook
                type: webhook
                settings:
                  url: "http://127.0.0.1:28080/hooks/generic/$HALEMANS_GENERIC_HOOK_TOKEN"
                  httpMethod: POST
    '';

    policiesYaml = pkgs.writeText "grafana-policies.yaml" ''
        apiVersion: 1
        policies:
          - orgId: 1
            receiver: halemans
            group_wait: 5s
            group_interval: 10s
            repeat_interval: 1h
    '';

    # @PGHOST@/@PGPORT@ and @STATE@ are substituted at process start.
    zabbixServerConfTemplate = pkgs.writeText "zabbix_server.conf.tpl" ''
        LogFile=@STATE@/server.log
        PidFile=@STATE@/server.pid
        SocketDir=@STATE@/sock
        DBHost=@PGHOST@
        DBPort=@PGPORT@
        DBName=zabbix
        DBUser=@DBUSER@
        ListenIP=127.0.0.1
        ListenPort=10051
        StatsAllowedIP=127.0.0.1
        CacheUpdateFrequency=5
    '';

    zabbixWebConfTemplate = pkgs.writeText "zabbix.conf.php.tpl" ''
        <?php
        $DB["TYPE"] = "POSTGRESQL";
        $DB["SERVER"] = "@PGHOST@";
        $DB["PORT"] = "@PGPORT@";
        $DB["DATABASE"] = "zabbix";
        $DB["USER"] = "@DBUSER@";
        $DB["PASSWORD"] = "";
        $DB["SCHEMA"] = "";
        $ZBX_SERVER = "127.0.0.1";
        $ZBX_SERVER_PORT = "10051";
        $ZBX_SERVER_NAME = "halemans-dev";
    '';

    zabbixAgentConfTemplate = pkgs.writeText "zabbix_agentd.conf.tpl" ''
        LogFile=@STATE@/agent.log
        PidFile=@STATE@/agent.pid
        Server=127.0.0.1
        ListenIP=127.0.0.1
        ListenPort=10050
        Hostname=dev-host-01
    '';
in
{
    inherit ensureTokens alertmanagerConfigTemplate grafanaIni grafanaProvisioning;
    inherit zabbixServerConfTemplate zabbixWebConfTemplate zabbixAgentConfTemplate;
}
