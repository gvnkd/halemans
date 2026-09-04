module Web.View.Sources.Edit where
import Web.View.Prelude

data EditView = EditView
    { source :: Source
    , tokenEnv :: Text
    }

instance View EditView where
    html EditView { .. } = [hsx|
        <h1>Edit source</h1>
        <form method="POST" action={UpdateSourceAction source.id} data-testid="source-edit-form" style="max-width: 500px">
            <div class="mb-3">
                <label class="form-label">Name</label>
                <input name="name" type="text" class="form-control" value={source.name} data-testid="source-name" required="required"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Type</label>
                <select name="type" class="form-select" data-testid="source-type">
                    {forEach ["webhook", "zabbix", "grafana", "alertmanager"] typeOption}
                </select>
            </div>
            <div class="mb-3">
                <label class="form-label">Base URL</label>
                <input name="baseUrl" type="text" class="form-control" value={source.baseUrl} data-testid="source-base-url"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Environment</label>
                <input name="env" type="text" class="form-control" value={source.env} data-testid="source-env"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Poll interval (seconds)</label>
                <input name="pollIntervalSeconds" type="number" class="form-control" value={source.pollIntervalSeconds} data-testid="source-poll-interval"/>
            </div>
            <div class="mb-3">
                <label class="form-label">Token env var</label>
                <input name="tokenEnv" type="text" class="form-control" value={tokenEnv} data-testid="source-token-env"/>
            </div>
            <button type="submit" class="btn btn-primary" data-testid="source-submit">Save</button>
        </form>
    |]
        where
            sourceType :: Text
            sourceType = get #type_ source
            typeOption value = [hsx|<option value={value} selected={sourceType == value}>{value}</option>|]
