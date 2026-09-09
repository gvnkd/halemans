module Web.View.Sources.New where
import Web.View.Prelude

data NewView = NewView

instance View NewView where
    html NewView = [hsx|
        <h1>New source</h1>
        <form method="POST" action={CreateSourceAction} data-testid="source-form" class="maxw-500">
            <div class="mb-3">
                <label class="form-label">Name</label>
                <input name="name" type="text" class="form-control" data-testid="source-name" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Type</label>
                <select name="type" class="form-select" data-testid="source-type">
                    <option value="webhook">webhook</option>
                    <option value="zabbix">zabbix</option>
                    <option value="grafana">grafana</option>
                    <option value="alertmanager">alertmanager</option>
                </select>
            </div>
            <div class="mb-3">
                <label class="form-label">Base URL</label>
                <input name="baseUrl" type="text" class="form-control" data-testid="source-base-url" placeholder="http://127.0.0.1:3001"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Environment</label>
                <input name="env" type="text" class="form-control" data-testid="source-env" value="dev"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Poll interval (seconds)</label>
                <input name="pollIntervalSeconds" type="number" class="form-control" data-testid="source-poll-interval" value="30"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Token env var</label>
                <input name="tokenEnv" type="text" class="form-control" data-testid="source-token-env" placeholder="GRAFANA_TOKEN"/>
            </div>
            <div class="mb-3 form-check">
                <input name="writeBack" type="checkbox" class="form-check-input" data-testid="source-write-back"/>
                <label class="form-check-label">Write-back (ack/close propagates to the source)</label>
            </div>
            <div class="mb-3">
                <label class="form-label">CMDB space</label>
                <input name="cmdbSpace" type="text" class="form-control" data-testid="source-cmdb-space" placeholder="DEV"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Jira project key</label>
                <input name="jiraProject" type="text" class="form-control" data-testid="source-jira-project" placeholder="DEV"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Initial history (days, zabbix first sync; empty = 1)</label>
                <input name="initialHistoryDays" type="number" class="form-control" data-testid="source-history-days" placeholder="1"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Host group scope (zabbix)</label>
                <select name="hostGroupScope" class="form-select" data-testid="source-host-group-scope">
                    <option value="all">all — fetch every alert</option>
                    <option value="teams">teams — only host groups configured on teams</option>
                </select>
            </div>
            <button type="submit" class="btn btn-primary" data-testid="source-submit">Create</button>
        </form>
    |]
