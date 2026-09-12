module Web.Controller.Environments where

import Application.Helper.DashboardConfig (quoteSqlText)
import qualified Application.Helper.FilterPrefs as FilterPrefs
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
            EnvFilters
                { filterSeverities = paramList @Text "severity"
                , filterStatuses = paramList @Text "status"
                , filterHost = nonEmptyParam "host"
                , filterService = nonEmptyParam "service"
                , filterText = nonEmptyParam "q"
                , filterGroup = nonEmptyParam "group"
                }

renderEnv :: (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext) => Text -> EnvFilters -> Text -> IO ResponseReceived
renderEnv environmentName filters viewMode = do
    -- The inventory row is optional: an env name that exists only as a
    -- materialized env facet (field-mapping override) still gets a page.
    environment <-
        query @Environment
            |> filterWhere (#name, environmentName)
            |> fetchOneOrNothing
    groupFilterIds <- case filters.filterGroup of
        Nothing -> pure Nothing
        Just pattern -> do
            matchingGroups <-
                query @AlertGroup
                    |> filterWhereILike (#groupKey, "%" <> pattern <> "%")
                    |> fetch
            pure (Just (map (Just . get #id) matchingGroups))
    -- Membership and host/service filters match on the EFFECTIVE value:
    -- a facet named env/host/service overrides the raw column. The
    -- filterWhereSql fragment is spliced behind `facets IS NOT NULL AND`
    -- (facets is NOT NULL by schema) because the builder only appends.
    alerts <-
        query @Alert
            |> filterWhereSql (#facets, "IS NOT NULL AND coalesce(nullif(alerts.facets ->> 'env', ''), alerts.env) = " <> quoteSqlText environmentName)
            |> applyList filters.filterSeverities (\values -> filterWhereIn (#severity, values))
            |> applyList filters.filterStatuses (\values -> filterWhereIn (#status, values))
            |> applyMaybe filters.filterHost (\value -> filterWhereSql (#facets, "IS NOT NULL AND coalesce(nullif(alerts.facets ->> 'host', ''), alerts.host) = " <> quoteSqlText value))
            |> applyMaybe filters.filterService (\value -> filterWhereSql (#facets, "IS NOT NULL AND coalesce(nullif(alerts.facets ->> 'service', ''), alerts.service) = " <> quoteSqlText value))
            |> applyMaybe filters.filterText (\value -> filterWhereILike (#title, "%" <> value <> "%"))
            |> applyMaybe groupFilterIds (\ids -> filterWhereIn (#groupId, ids))
            |> orderByDesc #lastSeenAt
            |> limit 200
            |> fetch
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
    render ShowView{..}

envFilterQueryKeys :: [ByteString]
envFilterQueryKeys = ["severity", "status", "host", "service", "q", "group", "view"]

applyMaybe :: Maybe value -> (value -> query -> query) -> query -> query
applyMaybe Nothing _ query' = query'
applyMaybe (Just value) f query' = f value query'

applyList :: [value] -> ([value] -> query -> query) -> query -> query
applyList [] _ query' = query'
applyList values f query' = f values query'
