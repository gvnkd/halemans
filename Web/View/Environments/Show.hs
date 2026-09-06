module Web.View.Environments.Show where
import Web.View.Prelude
import Web.View.Fragments (alertRowHtml, groupRowHtml)

data EnvFilters = EnvFilters
    { filterSeverity :: Maybe Text
    , filterStatus :: Maybe Text
    , filterHost :: Maybe Text
    , filterService :: Maybe Text
    , filterText :: Maybe Text
    , filterGroup :: Maybe Text
    }

data ShowView = ShowView
    { environment :: Environment
    , alerts :: [Alert]
    , groups :: [(AlertGroup, [Alert])]
    , blackouts :: [Blackout]
    , filters :: EnvFilters
    , viewMode :: Text
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div data-live-scope={"env:" <> environment.name}>
            <h1>{environment.name}</h1>
            {activeBlackoutNotice}
            <form method="GET" action={ShowEnvironmentAction environment.name} class="row g-2 mb-3" data-testid="env-filters">
                <div class="col-auto">
                    <select name="severity" class="form-select form-select-sm" onchange="this.form.submit()">
                        <option value="">severity: any</option>
                        {forEach ["critical", "high", "warning", "info"] severityOption}
                    </select>
                </div>
                <div class="col-auto">
                    <select name="status" class="form-select form-select-sm" onchange="this.form.submit()">
                        <option value="">status: any</option>
                        {forEach ["firing", "ack", "resolved", "closed"] statusOption}
                    </select>
                </div>
                <div class="col-auto"><input name="host" class="form-control form-control-sm" placeholder="host" value={fromMaybe "" filters.filterHost} onchange="this.form.submit()"/></div>
                <div class="col-auto"><input name="service" class="form-control form-control-sm" placeholder="service" value={fromMaybe "" filters.filterService} onchange="this.form.submit()"/></div>
                <div class="col-auto"><input name="q" class="form-control form-control-sm" placeholder="title contains" value={fromMaybe "" filters.filterText} onchange="this.form.submit()"/></div>
                <div class="col-auto"><input name="group" class="form-control form-control-sm" placeholder="group key" value={fromMaybe "" filters.filterGroup} data-testid="env-filter-group" onchange="this.form.submit()"/></div>
                <input type="hidden" name="view" value={viewMode}/>
                <div class="col-auto"><a href={resetUrl} class="btn btn-sm btn-outline-secondary" data-testid="env-filters-reset">Reset</a></div>
            </form>
            <div class="mb-2" data-testid="view-toggle">
                <a href={toggleUrl "flat"} class={toggleClass "flat"} data-testid="view-flat">Flat</a>
                <a href={toggleUrl "grouped"} class={toggleClass "grouped"} data-testid="view-grouped">Grouped</a>
            </div>
            {content}
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
            toggleUrl mode = pathTo (ShowEnvironmentAction environment.name) <> "?view=" <> mode
            resetUrl :: Text
            resetUrl = toggleUrl viewMode
            toggleClass :: Text -> Text
            toggleClass mode = if viewMode == mode then "btn btn-sm btn-secondary" else "btn btn-sm btn-outline-secondary"
            content = if viewMode == "grouped"
                then groupedTable
                else flatTable
            flatTable = [hsx|
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
            |]
            groupedTable = [hsx|
                <table class="table" data-testid="env-groups-table">
                    <thead>
                        <tr>
                            <th>Status</th>
                            <th>Worst severity</th>
                            <th>Group</th>
                            <th></th>
                        </tr>
                    </thead>
                    <tbody id="env-groups-tbody">
                        {forEach groups groupRowHtml}
                    </tbody>
                </table>
            |]

utcTooltip :: UTCTime -> Text
utcTooltip time = "UTC: " <> cs (show time)
