module Application.Service.ProvisionExport (buildProvisionExport) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.List as List
import Generated.Types
import IHP.Fetch (fetch)
import IHP.ModelSupport (Id' (..), ModelContext)
import IHP.Prelude
import IHP.QueryBuilder (orderByAsc, query)
import IHP.TypedSql (sqlQueryTyped, typedSql)

-- Renders the current DB state as a provision config (the map-keyed format
-- parsed by Application.Service.Provision), for the admin "export" download.
-- strict is always false: the export is a snapshot, never a reconcile order.
-- webhook_tokens and hostGroupsFile are NOT exported — the table holds raw
-- token values while the config wants env references, and secrets must not
-- land in a downloadable file; re-add them by hand after exporting.

buildProvisionExport :: (?modelContext :: ModelContext) => IO Aeson.Value
buildProvisionExport = do
    users <- exportUsers
    sources <- exportSources
    teams <- exportTeams
    llm <- exportLlm
    fieldMappings <- exportFieldMappings
    dashboards <- exportDashboards
    jiraConfigs <- exportJiraConfigs
    cmdbConfigs <- exportCmdbConfigs
    autoAnalyze <- exportAutoAnalyze
    pure $
        object $
            [ "strict" .= False
            , "users" .= users
            , "sources" .= sources
            , "teams" .= teams
            , "llm" .= llm
            , "fieldMappings" .= fieldMappings
            , "dashboards" .= dashboards
            , "jiraConfigs" .= jiraConfigs
            , "cmdbConfigs" .= cmdbConfigs
            ]
                <> ["autoAnalyze" .= autoAnalyze | isJust autoAnalyze]
  where
    exportUsers = do
        users <- query @User |> orderByAsc #email |> fetch
        entries <- forM users \user -> do
            let userId = get #id user
            roles <-
                sqlQueryTyped
                    [typedSql|
                SELECT r.name FROM user_roles ur JOIN roles r ON r.id = ur.role_id
                WHERE ur.user_id = ${userId} ORDER BY r.name
            |]
            pure $
                Key.fromText user.email
                    .= object
                        [ "displayName" .= user.displayName
                        , "passwordHash" .= user.passwordHash
                        , "roles" .= (roles :: [Text])
                        , "settings" .= user.settings
                        ]
        pure (object entries)
    exportSources = do
        sources <- query @Source |> orderByAsc #name |> fetch
        pure $
            object
                [ Key.fromText source.name
                    .= object
                        [ "type" .= source.type_
                        , "baseUrl" .= source.baseUrl
                        , "env" .= source.env
                        , "pollIntervalSeconds" .= source.pollIntervalSeconds
                        , "enabled" .= source.enabled
                        , "config" .= source.config
                        ]
                | source <- sources
                ]
    exportTeams = do
        teams <- query @Team |> orderByAsc #name |> fetch
        entries <- forM teams \team -> do
            let teamId = get #id team
            members <-
                sqlQueryTyped
                    [typedSql|
                SELECT u.email, tm.team_role FROM team_members tm
                JOIN users u ON u.id = tm.user_id WHERE tm.team_id = ${teamId}
                ORDER BY u.email
            |]
            pure $
                Key.fromText team.name
                    .= object
                        ( [ "description" .= team.description
                          , "hostGroups" .= team.hostGroups
                          , "defaults" .= team.defaults
                          , "members" .= object [Key.fromText (get #email row) .= object ["role" .= get #team_role row] | row <- members]
                          ]
                            <> ["defaultDashboardConfig" .= config | Just config <- [team.defaultDashboardConfig]]
                        )
        pure (object entries)
    exportLlm = do
        configs <- query @LlmConfig |> orderByAsc #providerName |> fetch
        templates <- query @LlmPromptTemplate |> orderByAsc #name |> orderByAsc #version |> fetch
        let templateNames = List.nub (map (.name) templates)
            templatesObject =
                object
                    [ Key.fromText name
                        .= object
                            [ Key.fromText (tshow template.version)
                                .= object
                                    ( [ "body" .= template.body
                                      , "active" .= template.active
                                      ]
                                        <> ["notes" .= notes | Just notes <- [template.notes]]
                                    )
                            | template <- templates
                            , template.name == name
                            ]
                    | name <- templateNames
                    ]
        pure $
            object
                [ Key.fromText config.providerName
                    .= object
                        ( [ "endpoint" .= config.endpoint
                          , "model" .= config.model
                          , "toolsEnabled" .= config.toolsEnabled
                          , "enabled" .= config.enabled
                          , "promptTemplates" .= templatesObject
                          ]
                            <> ["apiKeyEnv" .= apiKeyEnv | Just apiKeyEnv <- [config.apiKeyEnv]]
                        )
                | config <- configs
                ]
    exportFieldMappings = do
        mappings <- query @FieldMapping |> orderByAsc #facet |> orderByAsc #rank |> fetch
        let facets = List.nub (map (.facet) mappings)
        pure $
            object
                [ Key.fromText facet
                    .= object
                        [ Key.fromText (tshow mapping.rank)
                            .= object
                                [ "kind" .= mapping.kind
                                , "key" .= mapping.key
                                , "enabled" .= mapping.enabled
                                ]
                        | mapping <- mappings
                        , mapping.facet == facet
                        ]
                | facet <- facets
                ]
    exportDashboards = do
        dashboards <- query @Dashboard |> orderByAsc #name |> fetch
        entries <- forM dashboards \dashboard -> do
            owner <- fetch dashboard.userId
            pure
                ( owner.email
                , Key.fromText dashboard.name
                    .= object
                        [ "config" .= dashboard.config
                        , "position" .= dashboard.position
                        , "isDefault" .= dashboard.isDefault
                        ]
                )
        let ownerEmails = List.nub (map fst entries)
        pure $
            object
                [ Key.fromText email .= object [entry | (owner, entry) <- entries, owner == email]
                | email <- ownerEmails
                ]
    exportJiraConfigs = do
        configs <- query @JiraConfig |> orderByAsc #name |> fetch
        pure $
            object
                [ Key.fromText config.name
                    .= object
                        [ "baseUrl" .= config.baseUrl
                        , "tokenEnv" .= config.tokenEnv
                        , "apiVersion" .= config.apiVersion
                        , "projects" .= config.projects
                        , "enabled" .= config.enabled
                        ]
                | config <- configs
                ]
    exportCmdbConfigs = do
        configs <- query @CmdbConfig |> orderByAsc #name |> fetch
        pure $
            object
                [ Key.fromText config.name
                    .= object
                        [ "baseUrl" .= config.baseUrl
                        , "tokenEnv" .= config.tokenEnv
                        , "spaces" .= config.spaces
                        , "enabled" .= config.enabled
                        ]
                | config <- configs
                ]
    exportAutoAnalyze = do
        rows <- query @LlmAutoAnalyzeConfig |> fetch
        pure $ case rows of
            [] -> Nothing
            (row : _) ->
                Just $
                    object
                        [ "statuses" .= row.statuses
                        , "severities" .= row.severities
                        , "environments" .= row.environments
                        , "enabled" .= row.enabled
                        ]
