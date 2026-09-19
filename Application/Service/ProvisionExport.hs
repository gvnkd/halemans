module Application.Service.ProvisionExport (buildProvisionExport, renderProvisionJson, renderProvisionYaml) where

import Data.Aeson (object, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Yaml.Internal (isSpecialString)
import Generated.Types
import IHP.Fetch (fetch)
import IHP.ModelSupport (Id' (..), ModelContext, textToId)
import IHP.Prelude
import IHP.QueryBuilder (orderByAsc, query)
import IHP.TypedSql (sqlQueryTyped, typedSql)
import System.Environment (getEnvironment)

-- DB shape of an escalation policy step (what Application.Pipeline.Escalation
-- consumes): UUIDs as text, unlike the provision file's natural keys.
data StoredStep = StoredStep
    { stAfterSeconds :: Int
    , stTargetTeamId :: Maybe Text
    , stTargetUserId :: Maybe Text
    , stUnlessStatus :: Maybe Text
    }

parseStoredStep :: Aeson.Value -> Parser StoredStep
parseStoredStep = Aeson.withObject "escalation step" \o ->
    StoredStep
        <$> o .: "after_seconds"
        <*> o .:? "target_team_id"
        <*> o .:? "target_user_id"
        <*> o .:? "unless_status"

-- Renders the current DB state as a provision config (the map-keyed format
-- parsed by Application.Service.Provision), for the admin "export" download.
-- strict is always false: the export is a snapshot, never a reconcile order.
-- hostGroupsFile is NOT exported (needs a local file). webhook_tokens are
-- exported as env REFERENCES ONLY: a token whose value matches a process
-- environment variable is emitted as that variable's name (first match in
-- sorted order); tokens with no env match are omitted from the export, so
-- re-provisioning the file on a fresh node doesn't recreate them — set a
-- dedicated env var per token to make them round-trip.

buildProvisionExport :: (?modelContext :: ModelContext) => IO Aeson.Value
buildProvisionExport = do
    users <- exportUsers
    roles <- exportRoles
    sources <- exportSources
    teams <- exportTeams
    llm <- exportLlm
    fieldMappings <- exportFieldMappings
    dashboards <- exportDashboards
    jiraConfigs <- exportJiraConfigs
    cmdbConfigs <- exportCmdbConfigs
    assetsConfigs <- exportAssetsConfigs
    groupingRules <- exportGroupingRules
    escalationPolicies <- exportEscalationPolicies
    notificationRules <- exportNotificationRules
    llmAgentRoles <- exportLlmAgentRoles
    autoAnalyze <- exportAutoAnalyze
    pure $
        object $
            [ "strict" .= False
            , "users" .= users
            , "roles" .= roles
            , "sources" .= sources
            , "teams" .= teams
            , "llm" .= llm
            , "fieldMappings" .= fieldMappings
            , "dashboards" .= dashboards
            , "jiraConfigs" .= jiraConfigs
            , "cmdbConfigs" .= cmdbConfigs
            , "assetsConfigs" .= assetsConfigs
            , "groupingRules" .= groupingRules
            , "escalationPolicies" .= escalationPolicies
            , "notificationRules" .= notificationRules
            , "llmAgentRoles" .= llmAgentRoles
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
    exportRoles = do
        roleRows <- query @Role |> orderByAsc #name |> fetch
        pure $
            object
                [ Key.fromText role.name
                    .= object ["privileges" .= role.privileges]
                | role <- roleRows
                ]
    exportSources = do
        envVars <- List.sortOn fst <$> getEnvironment
        sources <- query @Source |> orderByAsc #name |> fetch
        entries <- forM sources \source -> do
            let sourceId = get #id source
            tokens <-
                sqlQueryTyped
                    [typedSql|
                SELECT t.token FROM webhook_tokens t
                WHERE t.source_id = ${sourceId}
                ORDER BY t.created_at
            |]
            -- webhook_tokens has no name column (the file-format key is a
            -- label only, dropped at import), so env-matched tokens are
            -- keyed by the env var name.
            let tokenEnvNames =
                    List.nub
                        [ cs envName :: Text
                        | tokenValue <- tokens
                        , (envName, envValue) <- envVars
                        , cs envValue == tokenValue
                        ]
            pure $
                Key.fromText source.name
                    .= object
                        ( [ "type" .= source.type_
                          , "baseUrl" .= source.baseUrl
                          , "env" .= source.env
                          , "pollIntervalSeconds" .= source.pollIntervalSeconds
                          , "enabled" .= source.enabled
                          , "config" .= source.config
                          ]
                            <> [ "webhookTokens"
                                    .= object
                                        [ Key.fromText envName .= object ["tokenEnv" .= envName]
                                        | envName <- tokenEnvNames
                                        ]
                               | not (null tokenEnvNames)
                               ]
                        )
        pure (object entries)
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
    exportAssetsConfigs = do
        configs <- query @AssetsConfig |> orderByAsc #name |> fetch
        pure $
            object
                [ Key.fromText config.name
                    .= object
                        ( [ "baseUrl" .= config.baseUrl
                          , "tokenEnv" .= config.tokenEnv
                          , "authMode" .= config.authMode
                          , "defaultSchemaName" .= config.defaultSchemaName
                          , "hostQueryTemplate" .= config.hostQueryTemplate
                          , "attributeNames" .= config.attributeNames
                          , "enabled" .= config.enabled
                          ]
                            <> ["jiraEmailEnv" .= jiraEmailEnv | Just jiraEmailEnv <- [config.jiraEmailEnv]]
                        )
                | config <- configs
                ]
    exportGroupingRules = do
        rules <- query @GroupingRule |> orderByAsc #name |> fetch
        pure $
            object
                [ Key.fromText rule.name
                    .= object
                        [ "position" .= rule.position
                        , "enabled" .= rule.enabled
                        , "match" .= rule.match
                        , "groupKeyTemplate" .= rule.groupKeyTemplate
                        ]
                | rule <- rules
                ]
    -- Steps are stored with UUID references; the export resolves them back
    -- to team names / user emails so the file is node-independent.
    exportEscalationPolicies = do
        policies <- query @EscalationPolicy |> orderByAsc #name |> fetch
        entries <- forM policies \policy -> do
            steps <- case parseMaybe (Aeson.parseJSON @[Aeson.Value]) policy.steps of
                Just values -> mapM resolveStep values
                Nothing -> pure []
            pure $ Key.fromText policy.name .= object ["steps" .= steps]
        pure (object entries)
    resolveStep value = case parseMaybe parseStoredStep value of
        Nothing -> pure (Aeson.object ["unresolvable" .= tshow value])
        Just step -> do
            targetTeam <- forM step.stTargetTeamId \teamIdText -> do
                let teamId = textToId teamIdText :: Id Team
                team <- fetch teamId
                pure team.name
            targetUser <- forM step.stTargetUserId \userIdText -> do
                let userId = textToId userIdText :: Id User
                user <- fetch userId
                pure user.email
            pure $
                Aeson.object
                    ( ["afterSeconds" .= step.stAfterSeconds]
                        <> ["targetTeam" .= team | Just team <- [targetTeam]]
                        <> ["targetUser" .= user | Just user <- [targetUser]]
                        <> ["unlessStatus" .= status | Just status <- [step.stUnlessStatus]]
                    )
    exportNotificationRules = do
        rules <- query @NotificationRule |> orderByAsc #name |> fetch
        entries <- forM rules \rule -> do
            teamName <- forM rule.teamId \teamId -> do
                team <- fetch teamId
                pure team.name
            userEmail <- forM rule.userId \userId -> do
                user <- fetch userId
                pure user.email
            policyName <- forM rule.escalationPolicyId \policyId -> do
                policy <- fetch policyId
                pure policy.name
            pure $
                Key.fromText rule.name
                    .= object
                        ( [ "position" .= rule.position
                          , "enabled" .= rule.enabled
                          , "match" .= rule.match
                          , "severityThreshold" .= rule.severityThreshold
                          , "channel" .= rule.channel
                          , "channelConfig" .= rule.channelConfig
                          , "throttleSeconds" .= rule.throttleSeconds
                          ]
                            <> ["team" .= team | Just team <- [teamName]]
                            <> ["user" .= user | Just user <- [userEmail]]
                            <> ["escalationPolicy" .= policy | Just policy <- [policyName]]
                        )
        pure (object entries)
    exportLlmAgentRoles = do
        roles <- query @LlmAgentRole |> orderByAsc #name |> fetch
        pure $
            object
                [ Key.fromText role.name
                    .= object
                        ( [ "description" .= role.description
                          , "promptTemplateName" .= role.promptTemplateName
                          , "tools" .= role.tools
                          , "enabled" .= role.enabled
                          , "isDefault" .= role.isDefault
                          ]
                        )
                | role <- roles
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

-- | Indented JSON for the provision export download.
renderProvisionJson :: Aeson.Value -> LBS.ByteString
renderProvisionJson = AesonPretty.encodePretty

-- Block-style YAML renderer for the export download. Data.Yaml's encode
-- (via libyaml) renders any multi-line string whose lines have trailing
-- whitespace or \r line endings as a single double-quoted line with \n
-- escapes — unreadable for prompt template bodies. This renderer emits
-- literal blocks (|/|-/|+) for every multi-line string, preserving bytes
-- exactly (trailing spaces included); only strings with \r or control
-- characters fall back to double-quoted style, where they are
-- unrepresentable in a block scalar.
renderProvisionYaml :: Aeson.Value -> ByteString
renderProvisionYaml = cs . Text.unlines . renderNodes 0

renderNodes :: Int -> Aeson.Value -> [Text]
renderNodes indent value = case value of
    Aeson.Object o
        | KeyMap.null o -> [pad "{}"]
        | otherwise -> concatMap entry (KeyMap.toList o)
      where
        entry (key, val) = entryLines indent (Key.toText key) val
    Aeson.Array a
        | null a -> [pad "[]"]
        | otherwise -> concatMap (itemLines indent) a
    _ -> [pad (scalarText value)]
  where
    pad prefix = Text.replicate indent " " <> prefix

entryLines :: Int -> Text -> Aeson.Value -> [Text]
entryLines indent key value = case value of
    Aeson.Object o
        | KeyMap.null o -> [keyPrefix <> ": {}"]
        | otherwise -> (keyPrefix <> ":") : concatMap entry (KeyMap.toList o)
      where
        entry (k, v) = entryLines (indent + 2) (Key.toText k) v
    Aeson.Array a
        | null a -> [keyPrefix <> ": []"]
        | otherwise -> (keyPrefix <> ":") : concatMap (itemLines (indent + 2)) a
    Aeson.String s
        | isLiteralString s -> (keyPrefix <> ": " <> literalHeader s) : literalLines (indent + 2) s
    _ -> [keyPrefix <> ": " <> scalarText value]
  where
    keyPrefix = Text.replicate indent " " <> renderKey key

itemLines :: Int -> Aeson.Value -> [Text]
itemLines indent item = case renderNodes (indent + 2) item of
    [] -> [pad "- null"]
    (firstLine : restLines) -> (pad "- " <> Text.drop (indent + 2) firstLine) : restLines
  where
    pad prefix = Text.replicate indent " " <> prefix

scalarText :: Aeson.Value -> Text
scalarText value = case value of
    Aeson.String s -> renderInlineString s
    Aeson.Bool True -> "true"
    Aeson.Bool False -> "false"
    Aeson.Null -> "null"
    _ -> cs (LBS.toStrict (Aeson.encode value))

renderKey :: Text -> Text
renderKey key
    | isPlainSafe key = key
    | otherwise = renderInlineString key

renderInlineString :: Text -> Text
renderInlineString s
    | Text.null s = "''"
    -- Multi-line strings that reach here are not literal-safe (pure
    -- newlines, \r, control chars); single-quoted folding would corrupt
    -- them, so they must be double-quoted.
    | needsDoubleQuotes s || Text.isInfixOf "\n" s = cs (LBS.toStrict (Aeson.encode s))
    | isPlainSafe s = s
    | otherwise = "'" <> Text.replace "'" "''" s <> "'"

isLiteralString :: Text -> Bool
isLiteralString s =
    Text.isInfixOf "\n" s
        && not (needsDoubleQuotes s)
        -- A block scalar whose content lines are all empty parses back as
        -- Null; keep pure-newline strings double-quoted.
        && not (Text.all (== '\n') s)

-- \r and control characters are unrepresentable in plain/single-quoted and
-- block scalars; JSON escaping doubles as YAML double-quoted style.
needsDoubleQuotes :: Text -> Bool
needsDoubleQuotes = Text.any (\c -> c == '\r' || c == '\x7f' || (c < ' ' && c /= '\t' && c /= '\n'))

isPlainSafe :: Text -> Bool
isPlainSafe s =
    not (Text.null s)
        && not (isSpecialString s)
        && not (Text.any (< ' ') s)
        && not (Text.isPrefixOf " " s || Text.isSuffixOf " " s)
        && Text.head s `notElem` ("-?:,[]{}#&*!|>'\"%@`" :: String)
        && not (Text.isInfixOf ": " s)
        && not (Text.isInfixOf " #" s)
        && not (Text.isSuffixOf ":" s)

literalHeader :: Text -> Text
literalHeader s
    | Text.isSuffixOf "\n\n" s = "|+"
    | Text.isSuffixOf "\n" s = "|"
    | otherwise = "|-"

literalLines :: Int -> Text -> [Text]
literalLines indent s = map renderLine contentLines
  where
    renderLine line = if Text.null line then "" else pad <> line
    pad = Text.replicate indent " "
    contentLines
        | Text.isSuffixOf "\n" s = List.init (Text.splitOn "\n" s)
        | otherwise = Text.splitOn "\n" s
