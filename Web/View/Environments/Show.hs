module Web.View.Environments.Show where
import Web.View.Prelude
import Web.View.Fragments (alertRowHtml, groupRowHtml, filterMultiSelect, filterTextInput)
import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)
import qualified Data.List as List
import qualified Data.Aeson as Aeson
import Data.Aeson ((.!=), (.=))
import Data.Aeson.Types (parseMaybe)
import Network.HTTP.Types.URI (renderQuery)

data EnvFilters = EnvFilters
    { filterSeverities :: [Text]
    , filterStatuses :: [Text]
    , filterHost :: Maybe Text
    , filterService :: Maybe Text
    , filterText :: Maybe Text
    , filterGroup :: Maybe Text
    } deriving (Eq, Show)

emptyEnvFilters :: EnvFilters
emptyEnvFilters = EnvFilters [] [] Nothing Nothing Nothing Nothing

-- Persisted shape for users.settings.filters.env (view mode included).
envFiltersToValue :: EnvFilters -> Text -> Aeson.Value
envFiltersToValue filters viewMode = Aeson.object
    [ "severity" .= filters.filterSeverities
    , "status" .= filters.filterStatuses
    , "host" .= filters.filterHost
    , "service" .= filters.filterService
    , "q" .= filters.filterText
    , "group" .= filters.filterGroup
    , "view" .= viewMode
    ]

envFiltersFromValue :: Aeson.Value -> Maybe (EnvFilters, Text)
envFiltersFromValue = parseMaybe (Aeson.withObject "envFilters" \o -> do
    severities <- o Aeson..:? "severity" .!= []
    statuses <- o Aeson..:? "status" .!= []
    host <- nonEmptyField o "host"
    service <- nonEmptyField o "service"
    title <- nonEmptyField o "q"
    group <- nonEmptyField o "group"
    view :: Text <- o Aeson..:? "view" .!= "flat"
    pure ( EnvFilters
            { filterSeverities = severities
            , filterStatuses = statuses
            , filterHost = host
            , filterService = service
            , filterText = title
            , filterGroup = group
            }
         , if view == "grouped" then "grouped" else "flat"
         ))
    where
        nonEmptyField o key = do
            raw <- o Aeson..:? key .!= ""
            pure (if raw == "" then Nothing else Just raw)

envBaseItems :: EnvFilters -> Text -> [(ByteString, Maybe ByteString)]
envBaseItems f viewMode =
    map (\value -> ("severity", Just (cs value))) f.filterSeverities
    ++ map (\value -> ("status", Just (cs value))) f.filterStatuses
    ++ maybe [] (\value -> [("host", Just (cs value))]) f.filterHost
    ++ maybe [] (\value -> [("service", Just (cs value))]) f.filterService
    ++ maybe [] (\value -> [("q", Just (cs value))]) f.filterText
    ++ maybe [] (\value -> [("group", Just (cs value))]) f.filterGroup
    ++ [("view", Just (cs viewMode))]

envPrefsAreDefault :: (EnvFilters, Text) -> Bool
envPrefsAreDefault (filters, viewMode) = filters == emptyEnvFilters && viewMode == "flat"

data ShowView = ShowView
    { environmentName :: Text
    , environment :: Maybe Environment
    , alerts :: [Alert]
    , groups :: [(AlertGroup, [Alert])]
    , blackouts :: [Blackout]
    , filters :: EnvFilters
    , viewMode :: Text
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div data-live-scope={"env:" <> environmentName} data-live-filters={liveFilters}>
            <h1>{environmentName}</h1>
            {activeBlackoutNotice}
            <form method="GET" action={ShowEnvironmentAction environmentName} class="row g-2 mb-3" data-testid="env-filters">
                {filterMultiSelect "severity" "severity" severities filters.filterSeverities}
                {filterMultiSelect "status" "status" statuses filters.filterStatuses}
                {filterTextInput "host" "host" filters.filterHost hostSuggestions}
                {filterTextInput "service" "service" filters.filterService serviceSuggestions}
                {filterTextInput "q" "title contains" filters.filterText titleSuggestions}
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
            -- Same staleness caveat as the /alerts table (see Alerts.Index):
            -- the client subscribes with THESE filters, not location.search.
            liveFilters :: Text
            liveFilters = cs (Aeson.encode (envFiltersToValue filters viewMode))
            severities = ["critical", "high", "warning", "info"]
            statuses = ["firing", "ack", "resolved", "closed"]
            hostSuggestions = List.sort (nub (mapMaybe (effectiveFieldText FieldHost) alerts))
            serviceSuggestions = List.sort (nub (mapMaybe (effectiveFieldText FieldService) alerts))
            titleSuggestions = List.sort (nub (map (\alert -> alert.title) alerts))
            activeBlackoutNotice = if null blackouts
                then mempty
                else [hsx|
                    <div class="alert alert-secondary blackout-notice" data-testid="blackout-notice">
                        Blackout active — new alerts are suppressed.
                    </div>
                |]
            toggleUrl mode = pathTo (ShowEnvironmentAction environmentName) <> cs (renderQuery True (envBaseItems filters mode))
            resetUrl :: Text
            resetUrl = pathTo (ShowEnvironmentAction environmentName) <> "?reset=1"
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
