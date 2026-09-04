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
                { filterSeverity = paramOrNothing @Text "severity"
                , filterStatus = paramOrNothing @Text "status"
                , filterHost = paramOrNothing @Text "host"
                , filterService = paramOrNothing @Text "service"
                , filterText = paramOrNothing @Text "q"
                }
        alerts <- query @Alert
            |> filterWhere (#environmentId, Just (get #id environment))
            |> applyMaybe filters.filterSeverity (\value -> filterWhere (#severity, value))
            |> applyMaybe filters.filterStatus (\value -> filterWhere (#status, value))
            |> applyMaybe filters.filterHost (\value -> filterWhere (#host, Just value))
            |> applyMaybe filters.filterService (\value -> filterWhere (#service, Just value))
            |> applyMaybe filters.filterText (\value -> filterWhereILike (#title, "%" <> value <> "%"))
            |> orderByDesc #lastSeenAt
            |> limit 200
            |> fetch
        blackouts <- query @Blackout
            |> filterWhere (#environmentId, Just (get #id environment))
            |> filterWhereSql (#endsAt, "> NOW()")
            |> orderByDesc #startsAt
            |> fetch
        render ShowView { .. }

applyMaybe :: Maybe value -> (value -> query -> query) -> query -> query
applyMaybe Nothing _ query' = query'
applyMaybe (Just value) f query' = f value query'
