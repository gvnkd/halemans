module Web.View.Alerts.Index where
import Web.View.Prelude
import Web.View.Fragments (alertRowHtml)
import Application.Service.AlertList (AlertListFilters (..))
import Network.HTTP.Types.URI (renderQuery)

data IndexView = IndexView
    { alerts :: [Alert]
    , filters :: AlertListFilters
    , counts :: [(Text, Int64)]
    , environments :: [Environment]
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Alerts</h1>
        {severityCounts}
        <form method="GET" action={AlertsAction} class="row g-2 mb-3" data-testid="alerts-filters">
            {multiSelect "severity" "severity" severities filters.alfSeverities}
            {multiSelect "status" "status" statuses filters.alfStatuses}
            {multiSelect "env" "env" envNames filters.alfEnvs}
            <div class="col-auto"><input name="host" class="form-control form-control-sm" placeholder="host" value={fromMaybe "" filters.alfHost} onchange="this.form.submit()"/></div>
            <div class="col-auto"><input name="service" class="form-control form-control-sm" placeholder="service" value={fromMaybe "" filters.alfService} onchange="this.form.submit()"/></div>
            <div class="col-auto"><input name="q" class="form-control form-control-sm" placeholder="title contains" value={fromMaybe "" filters.alfTitle} onchange="this.form.submit()"/></div>
            <div class="col-auto"><input name="group" class="form-control form-control-sm" placeholder="group key" value={fromMaybe "" filters.alfGroup} data-testid="alerts-filter-group" onchange="this.form.submit()"/></div>
            <input type="hidden" name="sort" value={filters.alfSort}/>
            <input type="hidden" name="dir" value={filters.alfDir}/>
            <div class="col-auto"><a href={AlertsAction} class="btn btn-sm btn-outline-secondary" data-testid="alerts-filters-reset">Reset</a></div>
        </form>
        <table class="table" data-testid="alerts-table" data-live-scope="alerts">
            <thead>
                <tr>
                    {sortableTh "status" "Status"}
                    {sortableTh "severity" "Severity"}
                    {sortableTh "title" "Title"}
                    {sortableTh "env" "Env"}
                    {sortableTh "host" "Host"}
                    {sortableTh "occurrences" "Occurrences"}
                    {sortableTh "last_seen_at" "Last seen"}
                </tr>
            </thead>
            <tbody id="alerts-tbody">
                {forEach alerts alertRowHtml}
            </tbody>
        </table>
    |]
        where
            severities = ["critical", "high", "warning", "info"]
            statuses = ["firing", "ack", "resolved", "closed"]
            envNames = map (\environment -> environment.name) environments

            severityCounts = [hsx|
                <div class="mb-2" data-testid="severity-counts">
                    {forEach severities countBadge}
                </div>
            |]
            countBadge severity = [hsx|
                <span class={"badge severity-badge severity-" <> severity <> " me-1"} data-testid={"count-" <> severity}>{severity} {countFor severity}</span>
            |]
            countFor severity = fromMaybe 0 (lookup severity counts)

            -- Checkbox dropdown multi-select; the button summarizes the
            -- selection, onchange resubmits the GET form (repeated params).
            multiSelect :: Text -> Text -> [Text] -> [Text] -> Html
            multiSelect name label options selected = [hsx|
                <div class="col-auto dropdown" data-testid={"filter-" <> name}>
                    <button class="btn btn-sm btn-outline-secondary dropdown-toggle" type="button" data-bs-toggle="dropdown" data-bs-auto-close="outside">{buttonLabel}</button>
                    <div class="dropdown-menu p-2">
                        {forEach options optionItem}
                    </div>
                </div>
            |]
                where
                    buttonLabel :: Text
                    buttonLabel = label <> ": " <> if null selected then "any" else tshow (length selected)
                    optionItem value = [hsx|
                        <div class="form-check">
                            <input class="form-check-input" type="checkbox" name={name} value={value} id={name <> "-" <> value} checked={value `elem` selected} onchange="this.form.submit()"/>
                            <label class="form-check-label" for={name <> "-" <> value}>{value}</label>
                        </div>
                    |]

            sortableTh :: Text -> Text -> Html
            sortableTh column label = [hsx|
                <th><a href={sortUrl column} class="text-decoration-none" data-testid={"sort-" <> column}>{label}{indicator}</a></th>
            |]
                where
                    indicator = if filters.alfSort == column
                        then [hsx|<span class="sort-indicator">{arrow}</span>|]
                        else mempty
                    arrow :: Text
                    arrow = if filters.alfDir == "asc" then " ▲" else " ▼"

            sortUrl :: Text -> Text
            sortUrl column = pathTo AlertsAction <> cs (renderQuery True (queryItems column))
                where
                    queryItems col = baseItems filters { alfSort = col, alfDir = nextDir col }
                    nextDir col
                        | filters.alfSort == col = if filters.alfDir == "asc" then "desc" else "asc"
                        | col == "last_seen_at" = "desc"
                        | otherwise = "asc"

baseItems :: AlertListFilters -> [(ByteString, Maybe ByteString)]
baseItems f =
    map (\value -> ("severity", Just (cs value))) f.alfSeverities
    ++ map (\value -> ("status", Just (cs value))) f.alfStatuses
    ++ map (\value -> ("env", Just (cs value))) f.alfEnvs
    ++ maybe [] (\value -> [("host", Just (cs value))]) f.alfHost
    ++ maybe [] (\value -> [("service", Just (cs value))]) f.alfService
    ++ maybe [] (\value -> [("q", Just (cs value))]) f.alfTitle
    ++ maybe [] (\value -> [("group", Just (cs value))]) f.alfGroup
    ++ [ ("sort", Just (cs f.alfSort)), ("dir", Just (cs f.alfDir)) ]
