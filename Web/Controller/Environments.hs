module Web.Controller.Environments where

import Web.Controller.Prelude
import Web.View.Environments.Show

instance Controller EnvironmentsController where
    beforeAction = ensureIsUser

    action ShowEnvironmentAction { environmentName } = do
        environment <- query @Environment
            |> filterWhere (#name, environmentName)
            |> fetchOne
        let filters = EnvFilters
                { filterSeverity = nonEmptyParam "severity"
                , filterStatus = nonEmptyParam "status"
                , filterHost = nonEmptyParam "host"
                , filterService = nonEmptyParam "service"
                , filterText = nonEmptyParam "q"
                , filterGroup = nonEmptyParam "group"
                }
        let viewMode = fromMaybe "flat" (nonEmptyParam "view")
        groupFilterIds <- case filters.filterGroup of
            Nothing -> pure Nothing
            Just pattern -> do
                matchingGroups <- query @AlertGroup
                    |> filterWhereILike (#groupKey, "%" <> pattern <> "%")
                    |> fetch
                pure (Just (map (Just . get #id) matchingGroups))
        alerts <- query @Alert
            |> filterWhere (#environmentId, Just (get #id environment))
            |> applyMaybe filters.filterSeverity (\value -> filterWhere (#severity, value))
            |> applyMaybe filters.filterStatus (\value -> filterWhere (#status, value))
            |> applyMaybe filters.filterHost (\value -> filterWhere (#host, Just value))
            |> applyMaybe filters.filterService (\value -> filterWhere (#service, Just value))
            |> applyMaybe filters.filterText (\value -> filterWhereILike (#title, "%" <> value <> "%"))
            |> applyMaybe groupFilterIds (\ids -> filterWhereIn (#groupId, ids))
            |> orderByDesc #lastSeenAt
            |> limit 200
            |> fetch
        groups <- if viewMode == "grouped"
            then do
                envGroups <- query @AlertGroup
                    |> filterWhere (#environmentId, Just (get #id environment))
                    |> orderByDesc #createdAt
                    |> fetch
                forM envGroups \group -> do
                    members <- query @Alert
                        |> filterWhere (#groupId, Just (get #id group))
                        |> orderByDesc #lastSeenAt
                        |> fetch
                    pure (group, members)
            else pure []
        blackouts <- query @Blackout
            |> filterWhere (#environmentId, Just (get #id environment))
            |> filterWhereSql (#endsAt, "> NOW()")
            |> orderByDesc #startsAt
            |> fetch
        render ShowView { .. }

applyMaybe :: Maybe value -> (value -> query -> query) -> query -> query
applyMaybe Nothing _ query' = query'
applyMaybe (Just value) f query' = f value query'

nonEmptyParam :: (?request :: Request) => ByteString -> Maybe Text
nonEmptyParam name = paramOrNothing @Text name >>= \value ->
    if value == "" then Nothing else Just value
