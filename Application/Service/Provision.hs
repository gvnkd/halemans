module Application.Service.Provision
( ProvisionConfig (..)
, Section (..)
, UserItem (..)
, SourceItem (..)
, WebhookTokenItem (..)
, TeamItem (..)
, MemberItem (..)
, LlmItem (..)
, PromptTemplateItem (..)
, FieldMappingItem (..)
, DashboardItem (..)
, ProvisionError (..)
, parseProvisionConfig
, parseHostGroupsFile
, applyProvisionConfig
) where

import IHP.Prelude
import IHP.ModelSupport (ModelContext, withTransaction, Id' (..), newRecord, createRecord, updateRecord, deleteRecord)
import IHP.HaskellSupport (set)
import IHP.QueryBuilder (query, filterWhere)
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.TypedSql (sqlQueryTyped, sqlExecTyped, typedSql)
import Generated.Types
import Application.Helper.Theme (isValidTheme)
import Application.Helper.DashboardConfig (decodeDashboardConfig)
import Application.Connector.Zabbix (ZabbixGroup)
import Application.Pipeline.Grouping (parseAlertField)
import Application.Service.HostGroups (replaceHostGroupCache)
import Application.Service.PollerControl (ensurePollerForSourceType)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value, FromJSON, parseJSON, (.:), (.:?), (.!=))
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import System.Environment (lookupEnv)
import Control.Exception (Exception, try, SomeException)
import Control.Monad (void)

-- Declarative bootstrap provisioning (design_docs/milestone_7.md). At process
-- start (hooked from Config.hs) the JSON file named by
-- HALEMANS_PROVISION_CONFIG is upserted into users, sources, teams, LLM
-- config, field mappings and dashboards. Every category applies inside one
-- transaction guarded by an advisory lock so racing web/worker boots
-- converge. Parsing is strict: unknown keys and unresolvable references
-- abort startup.

newtype ProvisionError = ProvisionError Text deriving stock (Show)
instance Exception ProvisionError

data Section a = Section
    { strict :: Bool
    , items :: [a]
    } deriving (Eq, Show)

data ProvisionConfig = ProvisionConfig
    { users :: Maybe (Section UserItem)
    , sources :: Maybe (Section SourceItem)
    , teams :: Maybe (Section TeamItem)
    , llm :: Maybe (Section LlmItem)
    , fieldMappings :: Maybe (Section FieldMappingItem)
    , dashboards :: Maybe (Section DashboardItem)
    } deriving (Eq, Show)

data UserItem = UserItem
    { email :: Text
    , displayName :: Text
    , passwordHash :: Text
    , roles :: [Text]
    , settings :: Value
    } deriving (Eq, Show)

data SourceItem = SourceItem
    { sourceType :: Text
    , name :: Text
    , baseUrl :: Text
    , env :: Text
    , pollIntervalSeconds :: Int
    , enabled :: Bool
    , config :: Value
    , webhookTokens :: [WebhookTokenItem]
    , hostGroupsFile :: Maybe Text
    } deriving (Eq, Show)

newtype WebhookTokenItem = WebhookTokenItem
    { tokenEnv :: Text
    } deriving (Eq, Show)

data TeamItem = TeamItem
    { name :: Text
    , description :: Maybe Text
    , hostGroups :: Maybe [Text]
    , defaults :: Maybe Value
    , members :: [MemberItem]
    , defaultDashboardConfig :: Maybe Value
    } deriving (Eq, Show)

data MemberItem = MemberItem
    { email :: Text
    , role :: Text
    } deriving (Eq, Show)

data LlmItem = LlmItem
    { providerName :: Text
    , endpoint :: Text
    , model :: Text
    , apiKeyEnv :: Maybe Text
    , toolsEnabled :: Bool
    , enabled :: Bool
    , promptTemplates :: [PromptTemplateItem]
    } deriving (Eq, Show)

data PromptTemplateItem = PromptTemplateItem
    { name :: Text
    , version :: Int
    , body :: Text
    , active :: Bool
    , notes :: Maybe Text
    } deriving (Eq, Show)

data FieldMappingItem = FieldMappingItem
    { facet :: Text
    , rank :: Int
    , kind :: Text
    , key :: Text
    , enabled :: Bool
    } deriving (Eq, Show)

data DashboardItem = DashboardItem
    { name :: Text
    , userEmail :: Text
    , config :: Value
    , position :: Int
    , isDefault :: Bool
    } deriving (Eq, Show)

-- Parsing (strict: unknown keys rejected at every level, milestone_7.md §2)

rejectUnknownFields :: [Text] -> Aeson.Object -> Parser ()
rejectUnknownFields allowed o =
    case filter (`notElem` allowed) (map Key.toText (KeyMap.keys o)) of
        [] -> pure ()
        (unknown:_) -> fail ("unknown field \"" <> cs unknown <> "\"")

instance FromJSON a => FromJSON (Section a) where
    parseJSON = Aeson.withObject "section" \o -> do
        rejectUnknownFields ["strict", "items"] o
        strict <- o .:? "strict" .!= False
        maybeItems <- o .:? "items"
        items <- case maybeItems of
            Nothing
                | strict -> fail "strict section requires an explicit \"items\" list (use \"items\": [] to empty the category)"
                | otherwise -> pure []
            Just items -> pure items
        pure Section { .. }

instance FromJSON UserItem where
    parseJSON = Aeson.withObject "users item" \o -> do
        rejectUnknownFields ["email", "displayName", "passwordHash", "roles", "settings"] o
        email <- o .: "email"
        displayName <- o .:? "displayName" .!= ""
        passwordHash <- o .: "passwordHash"
        roles <- o .:? "roles" .!= []
        settings <- o .:? "settings" .!= Aeson.object []
        validateTheme settings
        pure UserItem { .. }

validateTheme :: Value -> Parser ()
validateTheme settings = case settings of
    Aeson.Object o -> case KeyMap.lookup "theme" o of
        Nothing -> pure ()
        Just (Aeson.String theme)
            | isValidTheme theme -> pure ()
            | otherwise -> fail ("unknown theme \"" <> cs theme <> "\" (valid: latte frappe macchiato dracula light dark)")
        Just _ -> fail "settings.theme must be a string"
    _ -> fail "settings must be an object"

instance FromJSON WebhookTokenItem where
    parseJSON = Aeson.withObject "webhook token" \o -> do
        rejectUnknownFields ["tokenEnv"] o
        tokenEnv <- o .: "tokenEnv"
        pure WebhookTokenItem { .. }

instance FromJSON SourceItem where
    parseJSON = Aeson.withObject "sources item" \o -> do
        rejectUnknownFields ["type", "name", "baseUrl", "env", "pollIntervalSeconds", "enabled", "config", "webhookTokens", "hostGroupsFile"] o
        sourceType <- o .: "type"
        unless (sourceType `elem` ["zabbix", "grafana", "alertmanager", "webhook"]) do
            fail ("unknown source type \"" <> cs sourceType <> "\"")
        name <- o .: "name"
        baseUrl <- o .:? "baseUrl" .!= ""
        env <- o .:? "env" .!= "dev"
        pollIntervalSeconds <- o .:? "pollIntervalSeconds" .!= 30
        enabled <- o .:? "enabled" .!= True
        config <- o .:? "config" .!= Aeson.object []
        webhookTokens <- o .:? "webhookTokens" .!= []
        hostGroupsFile <- o .:? "hostGroupsFile"
        when (isJust hostGroupsFile && sourceType /= "zabbix") do
            fail "hostGroupsFile is only valid for zabbix sources"
        pure SourceItem { .. }

instance FromJSON MemberItem where
    parseJSON = Aeson.withObject "team member" \o -> do
        rejectUnknownFields ["email", "role"] o
        email <- o .: "email"
        rawRole <- o .:? "role" .!= "member"
        let role = if rawRole == "" then "member" else rawRole
        pure MemberItem { .. }

instance FromJSON TeamItem where
    parseJSON = Aeson.withObject "teams item" \o -> do
        rejectUnknownFields ["name", "description", "hostGroups", "defaults", "members", "defaultDashboardConfig"] o
        name <- o .: "name"
        -- Absent keys stay Nothing so re-provisioning does not clobber
        -- UI-edited values; an explicit key (even [] or "") overwrites.
        description <- o .:? "description"
        hostGroups <- o .:? "hostGroups"
        defaults <- o .:? "defaults"
        members <- o .:? "members" .!= []
        defaultDashboardConfig <- o .:? "defaultDashboardConfig"
        pure TeamItem { .. }

instance FromJSON PromptTemplateItem where
    parseJSON = Aeson.withObject "prompt template" \o -> do
        rejectUnknownFields ["name", "version", "body", "active", "notes"] o
        name <- o .: "name"
        version <- o .: "version"
        body <- o .: "body"
        active <- o .:? "active" .!= False
        notes <- o .:? "notes"
        pure PromptTemplateItem { .. }

instance FromJSON LlmItem where
    parseJSON = Aeson.withObject "llm item" \o -> do
        rejectUnknownFields ["providerName", "endpoint", "model", "apiKeyEnv", "toolsEnabled", "enabled", "promptTemplates"] o
        providerName <- o .: "providerName"
        endpoint <- o .: "endpoint"
        model <- o .: "model"
        apiKeyEnv <- o .:? "apiKeyEnv"
        toolsEnabled <- o .:? "toolsEnabled" .!= False
        enabled <- o .:? "enabled" .!= False
        promptTemplates <- o .:? "promptTemplates" .!= []
        pure LlmItem { .. }

instance FromJSON FieldMappingItem where
    parseJSON = Aeson.withObject "fieldMappings item" \o -> do
        rejectUnknownFields ["facet", "rank", "kind", "key", "enabled"] o
        facet <- o .: "facet"
        rank <- o .: "rank"
        kind <- o .: "kind"
        key <- o .: "key"
        enabled <- o .:? "enabled" .!= True
        unless (kind `elem` ["field", "label", "attr"]) do
            fail ("unknown field mapping kind \"" <> cs kind <> "\" (valid: field label attr)")
        when (kind == "field" && isNothing (parseAlertField key)) do
            fail ("unknown alert field \"" <> cs key <> "\" (valid: env host service check severity status)")
        pure FieldMappingItem { .. }

instance FromJSON DashboardItem where
    parseJSON = Aeson.withObject "dashboards item" \o -> do
        rejectUnknownFields ["name", "userEmail", "config", "position", "isDefault"] o
        name <- o .: "name"
        userEmail <- o .: "userEmail"
        config <- o .:? "config" .!= Aeson.toJSON ([] :: [Value])
        position <- o .:? "position" .!= 0
        isDefault <- o .:? "isDefault" .!= False
        case decodeDashboardConfig config of
            Left err -> fail ("invalid config for dashboard \"" <> cs name <> "\": " <> cs err)
            Right _ -> pure ()
        pure DashboardItem { .. }

instance FromJSON ProvisionConfig where
    parseJSON = Aeson.withObject "provision config" \o -> do
        rejectUnknownFields ["users", "sources", "teams", "llm", "fieldMappings", "dashboards"] o
        users <- o .:? "users"
        sources <- o .:? "sources"
        teams <- o .:? "teams"
        llm <- o .:? "llm"
        fieldMappings <- o .:? "fieldMappings"
        dashboards <- o .:? "dashboards"
        pure ProvisionConfig { .. }

parseProvisionConfig :: LByteString -> Either Text ProvisionConfig
parseProvisionConfig bytes = case Aeson.eitherDecode bytes of
    Left err -> Left ("invalid JSON: " <> cs err)
    Right value -> case parseEither parseJSON value of
        Left err -> Left (cs err)
        Right config -> Right config

-- Entry point (milestone_7.md §3): read + apply, aborting startup on any error.

applyProvisionConfig :: (?modelContext :: ModelContext) => FilePath -> IO ()
applyProvisionConfig path = do
    bytes <- LBS.readFile path
    config <- case parseProvisionConfig bytes of
        Left err -> throwIO $ ProvisionError (cs path <> ": " <> err)
        Right config -> pure config
    applyUsers config.users
    applySources config.sources
    applyTeams config.teams
    applyLlm config.llm
    applyFieldMappings config.fieldMappings
    applyDashboards config.dashboards
    putStrLn ("provision: applied " <> cs path)

-- Category application: one advisory-locked transaction per category
-- (milestone_7.md §3 concurrency note).

withProvisionLock :: (?modelContext :: ModelContext) => Text -> IO () -> IO ()
withProvisionLock category action = withTransaction do
    -- pg_advisory_xact_lock returns void; the SELECT 1 ... IS NULL shape is
    -- the typedSql-compatible spell (see MEMORIES typedSql notes).
    void (sqlQueryTyped [typedSql|
        SELECT 1 WHERE pg_advisory_xact_lock(hashtextextended(${category}, 0)) IS NULL
    |] :: IO [Int])
    action

-- Users (milestone_7.md §4)

applyUsers :: (?modelContext :: ModelContext) => Maybe (Section UserItem) -> IO ()
applyUsers Nothing = pure ()
applyUsers (Just section) = withProvisionLock "users" do
    forM_ section.items upsertUser
    when section.strict (strictDeleteUsers section.items)

upsertUser :: (?modelContext :: ModelContext) => UserItem -> IO ()
upsertUser item = do
    let email = item.email
        passwordHash = item.passwordHash
        displayName = item.displayName
        settings = item.settings
    void $ sqlExecTyped [typedSql|
        INSERT INTO users (email, password_hash, display_name, settings)
        VALUES (${email}, ${passwordHash}, ${displayName}, ${settings})
        ON CONFLICT (email) DO UPDATE SET
            password_hash = EXCLUDED.password_hash,
            display_name = EXCLUDED.display_name,
            settings = users.settings || EXCLUDED.settings
    |]
    forM_ item.roles (assignRole item.email)

assignRole :: (?modelContext :: ModelContext) => Text -> Text -> IO ()
assignRole email roleName = do
    created <- sqlQueryTyped [typedSql|
        INSERT INTO roles (name) VALUES (${roleName})
        ON CONFLICT (name) DO NOTHING
        RETURNING id
    |]
    unless (null (created :: [Id' "roles"])) do
        putStrLn ("provision: role \"" <> roleName <> "\" auto-created with empty privileges; grant privileges in the admin UI")
    void $ sqlExecTyped [typedSql|
        INSERT INTO user_roles (user_id, role_id)
        SELECT u.id, r.id FROM users u, roles r
        WHERE u.email = ${email} AND r.name = ${roleName}
        ON CONFLICT (user_id, role_id) DO NOTHING
    |]

strictDeleteUsers :: (?modelContext :: ModelContext) => [UserItem] -> IO ()
strictDeleteUsers items = do
    let keepEmails = map (.email) items
    allUsers <- query @User |> fetch
    let doomed = filter (\user -> user.email `notElem` keepEmails) allUsers
    forM_ doomed \user -> do
        let userId = get #id user
        result <- try do
            void $ sqlExecTyped [typedSql| DELETE FROM user_roles WHERE user_id = ${userId} |]
            deleteRecord user
        case result of
            Left err -> throwIO $ ProvisionError ("users: cannot delete user \"" <> user.email <> "\": " <> tshow (err :: SomeException))
            Right () -> pure ()

-- Sources (milestone_7.md §5)

applySources :: (?modelContext :: ModelContext) => Maybe (Section SourceItem) -> IO ()
applySources Nothing = pure ()
applySources (Just section) = withProvisionLock "sources" do
    forM_ section.items upsertSource
    when section.strict (strictDeleteSources section.items)

upsertSource :: (?modelContext :: ModelContext) => SourceItem -> IO ()
upsertSource item = do
    validateSourceEnvRefs item
    let sourceType = item.sourceType
        name = item.name
        baseUrl = item.baseUrl
        env = item.env
        pollIntervalSeconds = item.pollIntervalSeconds
        enabled = item.enabled
        config = item.config
    void $ sqlExecTyped [typedSql|
        INSERT INTO sources (type, name, base_url, env, poll_interval_seconds, enabled, config)
        VALUES (${sourceType}, ${name}, ${baseUrl}, ${env}, ${pollIntervalSeconds}, ${enabled}, ${config})
        ON CONFLICT (name) DO UPDATE SET
            type = EXCLUDED.type,
            base_url = EXCLUDED.base_url,
            env = EXCLUDED.env,
            poll_interval_seconds = EXCLUDED.poll_interval_seconds,
            enabled = EXCLUDED.enabled,
            config = EXCLUDED.config
    |]
    forM_ item.webhookTokens \tokenItem -> do
        maybeToken <- lookupEnv (cs tokenItem.tokenEnv)
        case maybeToken of
            Nothing -> throwIO $ ProvisionError ("sources." <> item.name <> ": webhook token env var \"" <> tokenItem.tokenEnv <> "\" is not set")
            Just tokenValue -> do
                let token = cs tokenValue :: Text
                void $ sqlExecTyped [typedSql|
                    INSERT INTO webhook_tokens (source_id, token)
                    SELECT id, ${token} FROM sources WHERE name = ${name}
                    ON CONFLICT (token) DO NOTHING
                |]
    when enabled (ensurePollerForSourceType sourceType)
    forM_ item.hostGroupsFile (applyHostGroupsFile item.name)

-- hostGroupsFile (zabbix sources only): replace the source's
-- zabbix_host_groups cache from a local JSON file instead of hostgroup.get
-- (for tokens without hostgroup.read permission). Accepts either a bare
-- array of {"groupid", "name"} objects or a full hostgroup.get response with
-- a "result" wrapper.
applyHostGroupsFile :: (?modelContext :: ModelContext) => Text -> Text -> IO ()
applyHostGroupsFile sourceName path = do
    readResult <- try (LBS.readFile (cs path))
    bytes <- case readResult of
        Left err -> throwIO $ ProvisionError ("sources." <> sourceName <> ": cannot read hostGroupsFile \"" <> path <> "\": " <> tshow (err :: SomeException))
        Right bytes -> pure bytes
    groups <- case parseHostGroupsFile bytes of
        Left err -> throwIO $ ProvisionError ("sources." <> sourceName <> ": hostGroupsFile \"" <> path <> "\": " <> err)
        Right groups -> pure groups
    maybeSource <- query @Source |> filterWhere (#name, sourceName) |> fetchOneOrNothing
    sourceId <- case maybeSource of
        Just source -> pure (get #id source)
        Nothing -> throwIO $ ProvisionError ("sources." <> sourceName <> ": source row missing after upsert")
    count <- replaceHostGroupCache sourceId groups
    putStrLn ("provision: imported " <> tshow count <> " host groups for \"" <> sourceName <> "\" from " <> path)

parseHostGroupsFile :: LByteString -> Either Text [ZabbixGroup]
parseHostGroupsFile bytes = case Aeson.eitherDecode bytes of
    Left err -> Left ("invalid JSON: " <> cs err)
    Right value -> case parseEither parseHostGroupsValue value of
        Left err -> Left (cs err)
        Right groups -> Right groups

parseHostGroupsValue :: Value -> Parser [ZabbixGroup]
parseHostGroupsValue (Aeson.Object o) = o .: "result"
parseHostGroupsValue value = parseJSON value

-- Every *Env reference in the config jsonb must resolve at provision time
-- (milestone_7.md §2: secrets are env references, never plaintext).
validateSourceEnvRefs :: (?modelContext :: ModelContext) => SourceItem -> IO ()
validateSourceEnvRefs item = case item.config of
    Aeson.Object o -> case KeyMap.lookup "tokenEnv" o of
        Nothing -> pure ()
        Just (Aeson.String envVar) -> do
            maybeValue <- lookupEnv (cs envVar)
            case maybeValue of
                Just _ -> pure ()
                Nothing -> throwIO $ ProvisionError ("sources." <> item.name <> ": tokenEnv \"" <> envVar <> "\" is not set")
        Just _ -> throwIO $ ProvisionError ("sources." <> item.name <> ": config.tokenEnv must be a string")
    _ -> throwIO $ ProvisionError ("sources." <> item.name <> ": config must be an object")

strictDeleteSources :: (?modelContext :: ModelContext) => [SourceItem] -> IO ()
strictDeleteSources items = do
    let keepNames = map (.name) items
    allSources <- query @Source |> fetch
    let doomed = filter (\source -> source.name `notElem` keepNames) allSources
    forM_ doomed \source -> do
        let sourceId = get #id source
        result <- try do
            void $ sqlExecTyped [typedSql| DELETE FROM webhook_tokens WHERE source_id = ${sourceId} |]
            void $ sqlExecTyped [typedSql| DELETE FROM zabbix_host_groups WHERE source_id = ${sourceId} |]
            deleteRecord source
        case result of
            Left err -> throwIO $ ProvisionError ("sources: cannot delete source \"" <> source.name <> "\" (referenced rows must go first; or keep it with \"enabled\": false): " <> tshow (err :: SomeException))
            Right () -> pure ()

-- Teams (milestone_7.md §6)

applyTeams :: (?modelContext :: ModelContext) => Maybe (Section TeamItem) -> IO ()
applyTeams Nothing = pure ()
applyTeams (Just section) = withProvisionLock "teams" do
    forM_ section.items (upsertTeam section.strict)
    when section.strict (strictDeleteTeams section.items)

upsertTeam :: (?modelContext :: ModelContext) => Bool -> TeamItem -> IO ()
upsertTeam strict item = do
    maybeTeam <- query @Team |> filterWhere (#name, item.name) |> fetchOneOrNothing
    team <- case maybeTeam of
        Nothing -> newRecord @Team
            |> set #name item.name
            |> set #description (fromMaybe "" item.description)
            |> set #hostGroups (Aeson.toJSON (fromMaybe [] item.hostGroups))
            |> set #defaults (fromMaybe (Aeson.object []) item.defaults)
            |> set #defaultDashboardConfig item.defaultDashboardConfig
            |> createRecord
        Just team -> do
            let withDescription = case item.description of
                    Just value -> team |> set #description value
                    Nothing -> team
                withHostGroups = case item.hostGroups of
                    Just groups -> withDescription |> set #hostGroups (Aeson.toJSON groups)
                    Nothing -> withDescription
                withDefaults = case item.defaults of
                    Just value -> withHostGroups |> set #defaults value
                    Nothing -> withHostGroups
            case item.defaultDashboardConfig of
                Just dashboardConfig -> withDefaults |> set #defaultDashboardConfig (Just dashboardConfig) |> updateRecord
                Nothing -> updateRecord withDefaults
    applyMembers team item
    when strict (pruneMembers team item)

applyMembers :: (?modelContext :: ModelContext) => Team -> TeamItem -> IO ()
applyMembers team item = do
    let teamId = get #id team
    forM_ item.members \member -> do
        maybeUser <- query @User |> filterWhere (#email, member.email) |> fetchOneOrNothing
        user <- case maybeUser of
            Nothing -> throwIO $ ProvisionError ("teams." <> item.name <> ": member email \"" <> member.email <> "\" does not resolve to any user")
            Just user -> pure user
        let userId = get #id user
            role = member.role
        void $ sqlExecTyped [typedSql|
            INSERT INTO team_members (team_id, user_id, team_role)
            VALUES (${teamId}, ${userId}, ${role})
            ON CONFLICT (team_id, user_id) DO UPDATE SET team_role = EXCLUDED.team_role
        |]

-- The one place strict reaches below the top-level entity: members absent
-- from the config entry are removed from the kept team (milestone_7.md §6).
pruneMembers :: (?modelContext :: ModelContext) => Team -> TeamItem -> IO ()
pruneMembers team item = do
    let teamId = get #id team
        keepEmails = map (.email) item.members
    members <- query @TeamMember |> filterWhere (#teamId, teamId) |> fetch
    forM_ members \member -> do
        user <- fetch member.userId
        when (user.email `notElem` keepEmails) (deleteRecord member)

strictDeleteTeams :: (?modelContext :: ModelContext) => [TeamItem] -> IO ()
strictDeleteTeams items = do
    let keepNames = map (.name) items
    allTeams <- query @Team |> fetch
    let doomed = filter (\team -> team.name `notElem` keepNames) allTeams
    forM_ doomed \team -> do
        let teamId = get #id team
        result <- try do
            void $ sqlExecTyped [typedSql| DELETE FROM team_members WHERE team_id = ${teamId} |]
            deleteRecord team
        case result of
            Left err -> throwIO $ ProvisionError ("teams: cannot delete team \"" <> team.name <> "\": " <> tshow (err :: SomeException))
            Right () -> pure ()

-- LLM config (milestone_7.md §7)

applyLlm :: (?modelContext :: ModelContext) => Maybe (Section LlmItem) -> IO ()
applyLlm Nothing = pure ()
applyLlm (Just section) = withProvisionLock "llm" do
    forM_ section.items upsertLlm
    when section.strict (strictReconcileLlm section.items)

upsertLlm :: (?modelContext :: ModelContext) => LlmItem -> IO ()
upsertLlm item = do
    when item.enabled do
        let providerName = item.providerName
        void $ sqlExecTyped [typedSql|
            UPDATE llm_configs SET enabled = false, updated_at = NOW()
            WHERE enabled AND provider_name <> ${providerName}
        |]
    maybeRow <- query @LlmConfig |> filterWhere (#providerName, item.providerName) |> fetchOneOrNothing
    now <- getCurrentTime
    _ <- case maybeRow of
        Nothing -> newRecord @LlmConfig
            |> set #providerName item.providerName
            |> set #endpoint item.endpoint
            |> set #model item.model
            |> set #apiKeyEnv item.apiKeyEnv
            |> set #toolsEnabled item.toolsEnabled
            |> set #enabled item.enabled
            |> createRecord
        Just row -> row
            |> set #endpoint item.endpoint
            |> set #model item.model
            |> set #apiKeyEnv item.apiKeyEnv
            |> set #toolsEnabled item.toolsEnabled
            |> set #enabled item.enabled
            |> set #updatedAt now
            |> updateRecord
    forM_ item.promptTemplates upsertPromptTemplate

upsertPromptTemplate :: (?modelContext :: ModelContext) => PromptTemplateItem -> IO ()
upsertPromptTemplate item = do
    maybeRow <- query @LlmPromptTemplate
        |> filterWhere (#name, item.name)
        |> filterWhere (#version, item.version)
        |> fetchOneOrNothing
    now <- getCurrentTime
    _ <- case maybeRow of
        Nothing -> newRecord @LlmPromptTemplate
            |> set #name item.name
            |> set #version item.version
            |> set #body item.body
            |> set #active False
            |> set #notes item.notes
            |> createRecord
        Just row -> row
            |> set #body item.body
            |> set #notes item.notes
            |> set #updatedAt now
            |> updateRecord
    when item.active (activatePromptTemplate item.name item.version)

-- Mirrors ActivateLlmTemplateAction: single active row per template name.
activatePromptTemplate :: (?modelContext :: ModelContext) => Text -> Int -> IO ()
activatePromptTemplate name version = do
    void $ sqlExecTyped [typedSql|
        UPDATE llm_prompt_templates SET active = false, updated_at = NOW()
        WHERE name = ${name}
    |]
    void $ sqlExecTyped [typedSql|
        UPDATE llm_prompt_templates SET active = true, updated_at = NOW()
        WHERE name = ${name} AND version = ${version}
    |]

strictReconcileLlm :: (?modelContext :: ModelContext) => [LlmItem] -> IO ()
strictReconcileLlm items = do
    let keepProviders = map (.providerName) items
    allConfigs <- query @LlmConfig |> fetch
    -- Nothing references llm_configs: deletes are always safe (§7).
    forM_ (filter (\row -> row.providerName `notElem` keepProviders) allConfigs) deleteRecord
    -- Templates reconcile per provisioned NAME only; names not mentioned in
    -- the config belong to the UI-managed namespace and stay untouched.
    let provisionedNames = List.nub [template.name | item <- items, template <- item.promptTemplates]
    forM_ provisionedNames \name -> do
        let keepVersions = [template.version | item <- items, template <- item.promptTemplates, template.name == name]
        rows <- query @LlmPromptTemplate |> filterWhere (#name, name) |> fetch
        forM_ (filter (\row -> row.version `notElem` keepVersions) rows) \row -> do
            result <- try (deleteRecord row)
            case result of
                Left err -> throwIO $ ProvisionError ("llm: cannot delete prompt template \"" <> name <> "\" v" <> tshow row.version <> ": " <> tshow (err :: SomeException))
                Right () -> pure ()

-- Field mappings (milestone 9 §2): upsert on the UNIQUE (facet, rank) pair;
-- re-provision updates kind/key/enabled in place.

applyFieldMappings :: (?modelContext :: ModelContext) => Maybe (Section FieldMappingItem) -> IO ()
applyFieldMappings Nothing = pure ()
applyFieldMappings (Just section) = withProvisionLock "fieldMappings" do
    forM_ section.items upsertFieldMapping
    when section.strict (strictDeleteFieldMappings section.items)

upsertFieldMapping :: (?modelContext :: ModelContext) => FieldMappingItem -> IO ()
upsertFieldMapping item = do
    let facet = item.facet
        rank = item.rank
        kind = item.kind
        key = item.key
        enabled = item.enabled
    void $ sqlExecTyped [typedSql|
        INSERT INTO field_mappings (facet, rank, kind, key, enabled)
        VALUES (${facet}, ${rank}, ${kind}, ${key}, ${enabled})
        ON CONFLICT (facet, rank) DO UPDATE SET
            kind = EXCLUDED.kind,
            key = EXCLUDED.key,
            enabled = EXCLUDED.enabled,
            updated_at = NOW()
    |]

strictDeleteFieldMappings :: (?modelContext :: ModelContext) => [FieldMappingItem] -> IO ()
strictDeleteFieldMappings items = do
    let keepPairs = map (\item -> (item.facet, item.rank)) items
    allMappings <- query @FieldMapping |> fetch
    -- Nothing references field_mappings: deletes are always safe.
    forM_ (filter (\mapping -> (mapping.facet, mapping.rank) `notElem` keepPairs) allMappings) deleteRecord

-- Dashboards: upsert by (user email, name). isDefault flips the user's other
-- dashboards off (mirrors dashboards_default_idx unique-WHERE).

applyDashboards :: (?modelContext :: ModelContext) => Maybe (Section DashboardItem) -> IO ()
applyDashboards Nothing = pure ()
applyDashboards (Just section) = withProvisionLock "dashboards" do
    forM_ section.items upsertDashboard
    when section.strict (strictDeleteDashboards section.items)

upsertDashboard :: (?modelContext :: ModelContext) => DashboardItem -> IO ()
upsertDashboard item = do
    maybeUser <- query @User |> filterWhere (#email, item.userEmail) |> fetchOneOrNothing
    user <- case maybeUser of
        Nothing -> throwIO $ ProvisionError ("dashboards." <> item.name <> ": userEmail \"" <> item.userEmail <> "\" does not resolve to any user")
        Just user -> pure user
    let userId = get #id user
    maybeRow <- query @Dashboard
        |> filterWhere (#userId, userId)
        |> filterWhere (#name, item.name)
        |> fetchOneOrNothing
    now <- getCurrentTime
    _ <- case maybeRow of
        Nothing -> newRecord @Dashboard
            |> set #userId userId
            |> set #name item.name
            |> set #config item.config
            |> set #position item.position
            |> set #isDefault item.isDefault
            |> createRecord
        Just row -> row
            |> set #config item.config
            |> set #position item.position
            |> set #isDefault item.isDefault
            |> set #updatedAt now
            |> updateRecord
    when item.isDefault do
        let name = item.name
        void $ sqlExecTyped [typedSql|
            UPDATE dashboards SET is_default = false, updated_at = NOW()
            WHERE user_id = ${userId} AND name <> ${name} AND is_default
        |]

strictDeleteDashboards :: (?modelContext :: ModelContext) => [DashboardItem] -> IO ()
strictDeleteDashboards items = do
    let keepPairs = map (\item -> (item.userEmail, item.name)) items
    allDashboards <- query @Dashboard |> fetch
    -- Nothing references dashboards: deletes are always safe.
    forM_ allDashboards \dashboard -> do
        owner <- fetch dashboard.userId
        when ((owner.email, dashboard.name) `notElem` keepPairs) (deleteRecord dashboard)
