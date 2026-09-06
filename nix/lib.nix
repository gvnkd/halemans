# Shared nix values used by both the devenv modules and checks.smoke:
# token helper, alertmanager config template, grafana ini + provisioning.
{ pkgs }:
let
    ensureTokens = pkgs.writeShellApplication {
        name = "halemans-ensure-tokens";
        runtimeInputs = [ pkgs.coreutils pkgs.openssl pkgs.gnused pkgs.jq ];
        text = ''
            state="''${DEVENV_STATE:?}/halemans"
            mkdir -p "$state"
            gen() { od -An -N32 -tx1 /dev/urandom | tr -d ' \n'; }
            [ -f "$state/am-hook-token" ]      || gen > "$state/am-hook-token"
            [ -f "$state/generic-hook-token" ] || gen > "$state/generic-hook-token"

            # Mock Confluence/Jira bearer tokens (milestone 3 D9).
            [ -f "$state/confluence-token" ]   || gen > "$state/confluence-token"
            [ -f "$state/jira-token" ]         || gen > "$state/jira-token"

            # Dev user passwords (milestone 1 D2): fixed-per-environment,
            # gitignored like the other tokens.
            [ -f "$state/admin-password" ]  || gen > "$state/admin-password"
            [ -f "$state/sre-password" ]    || gen > "$state/sre-password"
            [ -f "$state/viewer-password" ] || gen > "$state/viewer-password"

            # VAPID keypair for Web Push (milestone 1 D10). vapid.json holds
            # base64url raw keys: private = 32-byte scalar, public = 65-byte
            # uncompressed point (0x04 || X || Y).
            if [ ! -f "$state/vapid.json" ]; then
                openssl ecparam -name prime256v1 -genkey -noout -out "$state/vapid.pem.tmp"
                priv_hex="$(openssl ec -in "$state/vapid.pem.tmp" -noout -text 2>/dev/null \
                    | awk '/^priv:/{f=1;next} /^pub:/{f=0} f' | tr -d ' :\n')"
                # shellcheck disable=SC2034 # pub block ends at ASN1 OID
                pub_hex="$(openssl ec -in "$state/vapid.pem.tmp" -noout -text 2>/dev/null \
                    | awk '/^pub:/{f=1;next} /^ASN1 OID:/{f=0} f' | tr -d ' :\n')"
                rm -f "$state/vapid.pem.tmp"
                # strip a leading 00 padding byte from the scalar if present
                if [ "''${#priv_hex}" = 66 ]; then priv_hex="''${priv_hex#00}"; fi
                b64url() { sed 's/\(..\)/\\x\1/g' | { IFS= read -r esc; printf '%b' "$esc"; } | base64 -w0 | tr '+/' '-_' | tr -d '='; }
                priv_b64="$(printf %s "$priv_hex" | b64url)"
                pub_b64="$(printf %s "$pub_hex" | b64url)"
                jq -n --arg pub "$pub_b64" --arg priv "$priv_b64" \
                    '{publicKey: $pub, privateKey: $priv}' > "$state/vapid.json"
            fi

            {
                printf 'export HALEMANS_AM_HOOK_TOKEN="%s"\n'      "$(cat "$state/am-hook-token")"
                printf 'export HALEMANS_GENERIC_HOOK_TOKEN="%s"\n' "$(cat "$state/generic-hook-token")"
                printf 'export CONFLUENCE_TOKEN="%s"\n'            "$(cat "$state/confluence-token")"
                printf 'export JIRA_TOKEN="%s"\n'                  "$(cat "$state/jira-token")"
                printf 'export HALEMANS_CONFLUENCE_URL="%s"\n'     "http://127.0.0.1:18082"
                printf 'export HALEMANS_JIRA_URL="%s"\n'           "http://127.0.0.1:18083"
                # Mock LLM (milestone 4 D9): local endpoint, no token needed.
                printf 'export LLM_ENDPOINT="%s"\n'                 "http://127.0.0.1:18084"
                printf 'export LLM_MODEL="%s"\n'                    "mock-llm-1"
                printf 'export HALEMANS_ADMIN_PASSWORD="%s"\n'     "$(cat "$state/admin-password")"
                printf 'export HALEMANS_SRE_PASSWORD="%s"\n'       "$(cat "$state/sre-password")"
                printf 'export HALEMANS_VIEWER_PASSWORD="%s"\n'    "$(cat "$state/viewer-password")"
                printf 'export HALEMANS_VAPID_JSON="%s"\n'         "$state/vapid.json"
                printf 'export HALEMANS_VAPID_PUBLIC_KEY="%s"\n'   "$(jq -r .publicKey "$state/vapid.json")"
            } > "$state/env.sh"
        '';
    };

    hashPassword = pkgs.writeShellApplication {
        name = "halemans-hash-password";
        runtimeInputs = [ pkgs.python3 ];
        text = ''exec python3 ${./scripts/hash-password.py} "$1"'';
    };

    # Milestone 7 D4: plaintext from pwgen + pbkdf1 hash + users.items fragment.
    genPassword = pkgs.writeShellApplication {
        name = "halemans-gen-password";
        runtimeInputs = [ pkgs.pwgen pkgs.jq hashPassword ];
        text = builtins.readFile ./scripts/gen-password.sh;
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
    inherit ensureTokens hashPassword genPassword alertmanagerConfigTemplate grafanaIni grafanaProvisioning;
    inherit zabbixServerConfTemplate zabbixWebConfTemplate zabbixAgentConfTemplate;
}
