module Web.View.Alerts.Index where
import Web.View.Prelude
import Web.View.Fragments (alertRowHtml, filterMultiSelect, filterTextInput)
import Application.Service.AlertList (AlertListFilters (..))
import Network.HTTP.Types.URI (renderQuery)
import qualified Data.List as List

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
            {filterMultiSelect "severity" "severity" severities filters.alfSeverities}
            {filterMultiSelect "status" "status" statuses filters.alfStatuses}
            {filterMultiSelect "env" "env" envNames filters.alfEnvs}
            {filterTextInput "host" "host" filters.alfHost hostSuggestions}
            {filterTextInput "service" "service" filters.alfService serviceSuggestions}
            {filterTextInput "q" "title contains" filters.alfTitle titleSuggestions}
            <div class="col-auto"><input name="group" class="form-control form-control-sm" placeholder="group key" value={fromMaybe "" filters.alfGroup} data-testid="alerts-filter-group" onchange="this.form.submit()"/></div>
            <input type="hidden" name="sort" value={filters.alfSort}/>
            <input type="hidden" name="dir" value={filters.alfDir}/>
            <div class="col-auto"><a href={resetUrl} class="btn btn-sm btn-outline-secondary" data-testid="alerts-filters-reset">Reset</a></div>
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
            resetUrl :: Text
            resetUrl = pathTo AlertsAction <> "?reset=1"
            envNames = map (\environment -> environment.name) environments
            hostSuggestions = List.sort (nub (mapMaybe (\alert -> alert.host) alerts))
            serviceSuggestions = List.sort (nub (mapMaybe (\alert -> alert.service) alerts))
            titleSuggestions = List.sort (nub (map (\alert -> alert.title) alerts))

            severityCounts = [hsx|
                <div class="mb-2" data-testid="severity-counts">
                    {forEach severities countBadge}
                </div>
            |]
            countBadge severity = [hsx|
                <span class={"badge severity-badge severity-" <> severity <> " me-1"} data-testid={"count-" <> severity}>{severity} {countFor severity}</span>
            |]
            countFor severity = fromMaybe 0 (lookup severity counts)

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
