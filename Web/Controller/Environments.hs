module Web.Controller.Environments where

import Application.Helper.DashboardConfig (alertListColumnKeys, alertListPageSizes, defaultAlertListColumns, defaultAlertListPageSize, validAlertSortColumns)
import qualified Application.Helper.FilterPrefs as FilterPrefs
import qualified Application.Service.AlertList as AlertList
import Application.Service.DynTable (pageCountFor)
import Network.HTTP.Types.URI (renderQuery)
import Web.Controller.Prelude
import Web.View.Environments.Show

instance Controller EnvironmentsController where
    beforeAction = ensureIsUser

    action ShowEnvironmentAction{environmentName}
        | isJust (paramOrNothing @Text "reset") = do
            FilterPrefs.clearFilterPrefs currentUser "env"
            redirectTo ShowEnvironmentAction{environmentName}
        | FilterPrefs.hasQueryKeys envFilterQueryKeys ?request = do
            let filters = filtersFromParams
                viewMode = fromMaybe "flat" (nonEmptyParam "view")
            FilterPrefs.saveFilterPrefs currentUser "env" (envFiltersToValue filters viewMode)
            renderEnv environmentName filters viewMode
        | otherwise = case FilterPrefs.filterPrefsFor currentUser.settings "env" >>= envFiltersFromValue of
            Just stored
                | not (envPrefsAreDefault stored) ->
                    redirectToPath (pathTo (ShowEnvironmentAction environmentName) <> cs (renderQuery True (uncurry envBaseItems stored)))
            _ -> renderEnv environmentName emptyEnvFilters "flat"
      where
        filtersFromParams =
            let requestedSort = fromMaybe "last_seen_at" (nonEmptyParam "sort")
                requestedCols = [c | c <- alertListColumnKeys, c `elem` paramList @Text "cols"]
                requestedPageSize = fromMaybe defaultAlertListPageSize (paramOrNothing @Int "pageSize")
             in EnvFilters
                    { filterSeverities = paramList @Text "severity"
                    , filterStatuses = paramList @Text "status"
                    , filterHost = nonEmptyParam "host"
                    , filterService = nonEmptyParam "service"
                    , filterText = nonEmptyParam "q"
                    , filterGroup = nonEmptyParam "group"
                    , filterSort = if requestedSort `elem` validAlertSortColumns then requestedSort else "last_seen_at"
                    , filterDir = if nonEmptyParam "dir" == Just "asc" then "asc" else "desc"
                    , filterCols = if null requestedCols then defaultAlertListColumns else requestedCols
                    , filterPage = max 1 (fromMaybe 1 (paramOrNothing @Int "page"))
                    , filterPageSize = if requestedPageSize `elem` alertListPageSizes then requestedPageSize else defaultAlertListPageSize
                    , filterOccMin = paramOrNothing @Int "occ_min"
                    , filterSeenWithin = case nonEmptyParam "seen" of
                        Just window | isJust (AlertList.parseRelativeWindow window) -> Just window
                        _ -> Nothing
                    }

renderEnv :: (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext) => Text -> EnvFilters -> Text -> IO ResponseReceived
renderEnv environmentName filters viewMode = do
    -- The inventory row is optional: an env name that exists only as a
    -- materialized env facet (field-mapping override) still gets a page.
    environment <-
        query @Environment
            |> filterWhere (#name, environmentName)
            |> fetchOneOrNothing
    -- The flat list reuses the /alerts query engine (typedSql, dynamic
    -- sort, offset pagination); unlike /alerts, an empty status selection
    -- shows ALL statuses here, closed included (alfIncludeClosed).
    total <- AlertList.countAlerts alertFilters
    let effAlertFilters = alertFilters{AlertList.alfPage = min alertFilters.alfPage (pageCountFor total alertFilters.alfPageSize)}
    alerts <- AlertList.listAlerts effAlertFilters
    groupKeys <- case filters.filterCols of
        cols
            | "group" `elem` cols && not (null alerts) ->
                map (\group -> (get #id group, group.groupKey))
                    <$> (query @AlertGroup |> filterWhereIn (#id, mapMaybe (.groupId) alerts) |> fetch)
        _ -> pure []
    groups <-
        if viewMode == "grouped"
            then case environment of
                Nothing -> pure []
                Just env -> do
                    envGroups <-
                        query @AlertGroup
                            |> filterWhere (#environmentId, Just (get #id env))
                            |> orderByDesc #createdAt
                            |> fetch
                    forM envGroups \group -> do
                        members <-
                            query @Alert
                                |> filterWhere (#groupId, Just (get #id group))
                                |> orderByDesc #lastSeenAt
                                |> fetch
                        pure (group, members)
            else pure []
    blackouts <- case environment of
        Nothing -> pure []
        Just env ->
            query @Blackout
                |> filterWhere (#environmentId, Just (get #id env))
                |> filterWhereSql (#endsAt, "> NOW()")
                |> orderByDesc #startsAt
                |> fetch
    render ShowView{filters = filters{filterPage = effAlertFilters.alfPage}, ..}
  where
    alertFilters =
        AlertList.AlertListFilters
            { alfSeverities = filters.filterSeverities
            , alfStatuses = filters.filterStatuses
            , alfEnvs = [environmentName]
            , alfHost = filters.filterHost
            , alfService = filters.filterService
            , alfTitle = filters.filterText
            , alfGroup = filters.filterGroup
            , alfMuted = []
            , alfSort = filters.filterSort
            , alfDir = filters.filterDir
            , alfColumns = filters.filterCols
            , alfPage = filters.filterPage
            , alfPageSize = filters.filterPageSize
            , alfMinOccurrences = filters.filterOccMin
            , alfSeenWithin = filters.filterSeenWithin
            , alfIncludeClosed = True
            }

envFilterQueryKeys :: [ByteString]
envFilterQueryKeys = ["severity", "status", "host", "service", "q", "group", "view", "sort", "dir", "cols", "page", "pageSize", "occ_min", "seen"]
