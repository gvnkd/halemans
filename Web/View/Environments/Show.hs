module Web.View.Environments.Show where
import Web.View.Prelude
import Web.View.Fragments (alertRowHtml)

data EnvFilters = EnvFilters
    { filterSeverity :: Maybe Text
    , filterStatus :: Maybe Text
    , filterHost :: Maybe Text
    , filterService :: Maybe Text
    , filterText :: Maybe Text
    }

data ShowView = ShowView
    { environment :: Environment
    , alerts :: [Alert]
    , blackouts :: [Blackout]
    , filters :: EnvFilters
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div data-live-scope={"env:" <> environment.name}>
            <h1>{environment.name}</h1>
            {activeBlackoutNotice}
            <form method="GET" action={ShowEnvironmentAction environment.name} class="row g-2 mb-3" data-testid="env-filters">
                <div class="col-auto">
                    <select name="severity" class="form-select form-select-sm">
                        <option value="">severity: any</option>
                        {forEach ["critical", "high", "warning", "info"] severityOption}
                    </select>
                </div>
                <div class="col-auto">
                    <select name="status" class="form-select form-select-sm">
                        <option value="">status: any</option>
                        {forEach ["firing", "ack", "resolved", "closed"] statusOption}
                    </select>
                </div>
                <div class="col-auto"><input name="host" class="form-control form-control-sm" placeholder="host" value={fromMaybe "" filters.filterHost}/></div>
                <div class="col-auto"><input name="service" class="form-control form-control-sm" placeholder="service" value={fromMaybe "" filters.filterService}/></div>
                <div class="col-auto"><input name="q" class="form-control form-control-sm" placeholder="title contains" value={fromMaybe "" filters.filterText}/></div>
                <div class="col-auto"><button type="submit" class="btn btn-sm btn-primary">Filter</button></div>
            </form>
            <table class="table" data-testid="env-alerts-table">
                <thead>
                    <tr>
                        <th>Status</th>
                        <th>Severity</th>
                        <th>Title</th>
                        <th>Host</th>
                        <th>Service</th>
                        <th>Occurrences</th>
                        <th>Last seen</th>
                    </tr>
                </thead>
                <tbody id="env-alerts-tbody">
                    {forEach alerts alertRowHtml}
                </tbody>
            </table>
        </div>
    |]
        where
            activeBlackoutNotice = if null blackouts
                then mempty
                else [hsx|
                    <div class="alert alert-secondary blackout-notice" data-testid="blackout-notice">
                        Blackout active — new alerts are suppressed.
                    </div>
                |]
            severityOption value = [hsx|<option value={value} selected={filters.filterSeverity == Just value}>{value}</option>|]
            statusOption value = [hsx|<option value={value} selected={filters.filterStatus == Just value}>{value}</option>|]

utcTooltip :: UTCTime -> Text
utcTooltip time = "UTC: " <> cs (show time)
