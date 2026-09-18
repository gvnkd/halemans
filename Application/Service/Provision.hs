module Application.Service.Provision (
    ProvisionConfig (..),
    UserItem (..),
    SourceItem (..),
    WebhookTokenItem (..),
    TeamItem (..),
    MemberItem (..),
    LlmItem (..),
    PromptTemplateItem (..),
    FieldMappingItem (..),
    DashboardItem (..),
    JiraConfigItem (..),
    CmdbConfigItem (..),
    AutoAnalyzeItem (..),
    ProvisionError (..),
    parseProvisionConfig,
    parseProvisionConfigYaml,
    parseHostGroupsFile,
    applyProvisionConfig,
) where

import Application.Connector.Zabbix (ZabbixGroup)
import Application.Helper.DashboardConfig (decodeDashboardConfig)
import Application.Helper.Theme (isValidTheme)
import Application.Helper.Timezone (isValidTimezone)
import Application.Pipeline.Grouping (parseAlertField)
import Application.Service.HostGroups (replaceHostGroupCache)
import qualified Application.Service.Llm.AutoAnalyze as AutoAnalyze
import Application.Service.PollerControl (ensurePollerForSourceType)
import Control.Exception (Exception, SomeException, try)
import Control.Monad (void)
import Data.Aeson (FromJSON, Value, parseJSON, (.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (JSONPathElement (..), Parser, parseEither, (<?>))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Yaml as Yaml
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.HaskellSupport (set)
import IHP.ModelSupport (Id' (..), ModelContext, createRecord, deleteRecord, newRecord, updateRecord, withTransaction)
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, query)
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension)
import Text.Read (readMaybe)

-- Declarative bootstrap provisioning (design_docs/milestone_7.md). At process
-- start (hooked from Config.hs) the JSON or YAML file named by
-- HALEMANS_PROVISION_CONFIG is upserted into users, sources, teams, LLM
-- config, field mappings and dashboards. Sections are maps keyed by the
-- entity's natural key (user email, source/team/provider/config name, facet +
-- rank, user email + dashboard name) so duplicates are caught by json/yaml
-- tooling; a single top-level "strict" flag switches every present section to
-- reconcile-delete semantics. Every category applies inside one transaction
-- guarded by an advisory lock so racing web/worker boots converge. Parsing is
-- strict: unknown keys and unresolvable references abort startup.

newtype ProvisionError = ProvisionError Text deriving stock (Show)
instance Exception ProvisionError

data ProvisionConfig = ProvisionConfig
    { strict :: Bool
    , users :: Maybe [UserItem]
    , sources :: Maybe [SourceItem]
    , teams :: Maybe [TeamItem]
    , llm :: Maybe [LlmItem]
    , fieldMappings :: Maybe [FieldMappingItem]
    , dashboards :: Maybe [DashboardItem]
    , jiraConfigs :: Maybe [JiraConfigItem]
    , cmdbConfigs :: Maybe [CmdbConfigItem]
    , autoAnalyze :: Maybe AutoAnalyzeItem
    }
    deriving (Eq, Show)

data UserItem = UserItem
    { email :: Text
    , displayName :: Text
    , passwordHash :: Text
    , roles :: [Text]
    , settings :: Value
    }
    deriving (Eq, Show)

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
    }
    deriving (Eq, Show)

data WebhookTokenItem = WebhookTokenItem
    { wtName :: Text
    , tokenEnv :: Text
    }
    deriving (Eq, Show)

data TeamItem = TeamItem
    { name :: Text
    , description :: Maybe Text
    , hostGroups :: Maybe [Text]
    , defaults :: Maybe Value
    , members :: [MemberItem]
    , defaultDashboardConfig :: Maybe Value
    }
    deriving (Eq, Show)

data MemberItem = MemberItem
    { email :: Text
    , role :: Text
    }
    deriving (Eq, Show)

data LlmItem = LlmItem
    { providerName :: Text
    , endpoint :: Text
    , model :: Text
    , apiKeyEnv :: Maybe Text
    , toolsEnabled :: Bool
    , enabled :: Bool
    , promptTemplates :: [PromptTemplateItem]
    }
    deriving (Eq, Show)

data PromptTemplateItem = PromptTemplateItem
    { name :: Text
    , version :: Int
    , body :: Text
    , active :: Bool
    , notes :: Maybe Text
    }
    deriving (Eq, Show)

data FieldMappingItem = FieldMappingItem
    { facet :: Text
    , rank :: Int
    , kind :: Text
    , key :: Text
    , enabled :: Bool
    }
    deriving (Eq, Show)

data DashboardItem = DashboardItem
    { name :: Text
    , userEmail :: Text
    , config :: Value
    , position :: Int
    , isDefault :: Bool
    }
    deriving (Eq, Show)

data JiraConfigItem = JiraConfigItem
    { jiraConfigName :: Text
    , jiraBaseUrl :: Text
    , jiraTokenEnv :: Text
    , jiraApiVersion :: Text
    , jiraProjects :: [Text]
    , jiraEnabled :: Bool
    }
    deriving (Eq, Show)

data CmdbConfigItem = CmdbConfigItem
    { cmdbConfigName :: Text
    , cmdbBaseUrl :: Text
    , cmdbTokenEnv :: Text
    , cmdbSpaces :: [Text]
    , cmdbEnabled :: Bool
    }
    deriving (Eq, Show)

-- Auto-analysis gate (milestone 10 §5): singleton, not a keyed section — no
-- strict-delete semantics; an absent key leaves the row untouched.
-- environments scopes to effective env names; absent/empty = all envs.
data AutoAnalyzeItem = AutoAnalyzeItem
    { aaItemStatuses :: [Text]
    , aaItemSeverities :: [Text]
    , aaItemEnvironments :: [Text]
    , aaItemEnabled :: Bool
    }
    deriving (Eq, Show)

-- Parsing (strict: unknown keys rejected at every level, milestone_7.md §2)

rejectUnknownFields :: [Text] -> Aeson.Object -> Parser ()
rejectUnknownFields allowed o =
    case filter (`notElem` allowed) (map Key.toText (KeyMap.keys o)) of
        [] -> pure ()
        (unknown : _) -> fail ("unknown field \"" <> cs unknown <> "\"")

-- Sections are maps keyed by the entity's natural key; the key is threaded
-- into the item parser so the item records keep their name/email fields.
parseKeyed :: Text -> (Text -> Aeson.Object -> Parser a) -> Value -> Parser [a]
parseKeyed label parseItem = Aeson.withObject (cs label) \o ->
    forM (KeyMap.toList o) \(key, value) ->
        Aeson.withObject (cs label) (parseItem (Key.toText key)) value <?> Key key

parseSection :: Key.Key -> (Text -> Aeson.Object -> Parser a) -> Aeson.Object -> Parser (Maybe [a])
parseSection name parseItem o = case KeyMap.lookup name o of
    Nothing -> pure Nothing
    Just value -> Just <$> (parseKeyed (Key.toText name) parseItem value <?> Key name)

-- Nested map keys that must be integers (field mapping ranks, prompt
-- template versions).
parseIntKey :: Text -> Text -> Parser Int
parseIntKey label key = case readMaybe (cs key) of
    Just n -> pure n
    Nothing -> fail (cs (label <> " \"" <> key <> "\" is not an integer"))

parseUserItem :: Text -> Aeson.Object -> Parser UserItem
parseUserItem email o = do
    rejectUnknownFields ["displayName", "passwordHash", "roles", "settings"] o
    displayName <- o .:? "displayName" .!= ""
    passwordHash <- o .: "passwordHash"
    roles <- o .:? "roles" .!= []
    settings <- o .:? "settings" .!= Aeson.object []
    validateTheme settings
    validateTimezone settings
    pure UserItem{..}

validateTheme :: Value -> Parser ()
validateTheme settings = case settings of
    Aeson.Object o -> case KeyMap.lookup "theme" o of
        Nothing -> pure ()
        Just (Aeson.String theme)
            | isValidTheme theme -> pure ()
            | otherwise -> fail ("unknown theme \"" <> cs theme <> "\" (valid: latte frappe macchiato dracula light dark)")
        Just _ -> fail "settings.theme must be a string"
    _ -> fail "settings must be an object"

validateTimezone :: Value -> Parser ()
validateTimezone settings = case settings of
    Aeson.Object o -> case KeyMap.lookup "timezone" o of
        Nothing -> pure ()
        Just (Aeson.String timezone)
            | isValidTimezone timezone -> pure ()
            | otherwise -> fail ("unknown timezone \"" <> cs timezone <> "\" (fixed offset like \"UTC+3\" / \"UTC-4\")")
        Just _ -> fail "settings.timezone must be a string"
    _ -> fail "settings must be an object"

parseWebhookTokenItem :: Text -> Aeson.Object -> Parser WebhookTokenItem
parseWebhookTokenItem wtName o = do
    rejectUnknownFields ["tokenEnv"] o
    tokenEnv <- o .: "tokenEnv"
    pure WebhookTokenItem{..}

parseSourceItem :: Text -> Aeson.Object -> Parser SourceItem
parseSourceItem name o = do
    rejectUnknownFields ["type", "baseUrl", "env", "pollIntervalSeconds", "enabled", "config", "webhookTokens", "hostGroupsFile"] o
    sourceType <- o .: "type"
    unless (sourceType `elem` ["zabbix", "grafana", "alertmanager", "webhook"]) do
        fail ("unknown source type \"" <> cs sourceType <> "\"")
    baseUrl <- o .:? "baseUrl" .!= ""
    env <- o .:? "env" .!= "dev"
    pollIntervalSeconds <- o .:? "pollIntervalSeconds" .!= 30
    enabled <- o .:? "enabled" .!= True
    config <- o .:? "config" .!= Aeson.object []
    webhookTokens <- case KeyMap.lookup "webhookTokens" o of
        Nothing -> pure []
        Just value -> parseKeyed "webhookTokens" parseWebhookTokenItem value <?> Key "webhookTokens"
    hostGroupsFile <- o .:? "hostGroupsFile"
    when (isJust hostGroupsFile && sourceType /= "zabbix") do
        fail "hostGroupsFile is only valid for zabbix sources"
    pure SourceItem{..}

parseMemberItem :: Text -> Aeson.Object -> Parser MemberItem
parseMemberItem email o = do
    rejectUnknownFields ["role"] o
    rawRole <- o .:? "role" .!= "member"
    let role = if rawRole == "" then "member" else rawRole
    pure MemberItem{..}

parseTeamItem :: Text -> Aeson.Object -> Parser TeamItem
parseTeamItem name o = do
    rejectUnknownFields ["description", "hostGroups", "defaults", "members", "defaultDashboardConfig"] o
    -- Absent keys stay Nothing so re-provisioning does not clobber
    -- UI-edited values; an explicit key (even [] or "") overwrites.
    description <- o .:? "description"
    hostGroups <- o .:? "hostGroups"
    defaults <- o .:? "defaults"
    members <- case KeyMap.lookup "members" o of
        Nothing -> pure []
        Just value -> parseKeyed "members" parseMemberItem value <?> Key "members"
    defaultDashboardConfig <- o .:? "defaultDashboardConfig"
    pure TeamItem{..}

parsePromptTemplateItem :: Text -> Int -> Aeson.Object -> Parser PromptTemplateItem
parsePromptTemplateItem name version o = do
    rejectUnknownFields ["body", "active", "notes"] o
    body <- o .: "body"
    active <- o .:? "active" .!= False
    notes <- o .:? "notes"
    pure PromptTemplateItem{..}

parsePromptTemplateScope :: Text -> Aeson.Object -> Parser [PromptTemplateItem]
parsePromptTemplateScope name o =
    forM (KeyMap.toList o) \(key, value) -> do
        version <- parseIntKey "prompt template version" (Key.toText key)
        Aeson.withObject "promptTemplates" (parsePromptTemplateItem name version) value <?> Key key

parseLlmItem :: Text -> Aeson.Object -> Parser LlmItem
parseLlmItem providerName o = do
    rejectUnknownFields ["endpoint", "model", "apiKeyEnv", "toolsEnabled", "enabled", "promptTemplates"] o
    endpoint <- o .: "endpoint"
    model <- o .: "model"
    apiKeyEnv <- o .:? "apiKeyEnv"
    toolsEnabled <- o .:? "toolsEnabled" .!= False
    enabled <- o .:? "enabled" .!= False
    promptTemplates <- case KeyMap.lookup "promptTemplates" o of
        Nothing -> pure []
        Just value -> fmap concat (parseKeyed "promptTemplates" parsePromptTemplateScope value) <?> Key "promptTemplates"
    pure LlmItem{..}

parseFieldMappingItem :: Text -> Int -> Aeson.Object -> Parser FieldMappingItem
parseFieldMappingItem facet rank o = do
    rejectUnknownFields ["kind", "key", "enabled"] o
    kind <- o .: "kind"
    key <- o .: "key"
    enabled <- o .:? "enabled" .!= True
    unless (kind `elem` ["field", "label", "attr"]) do
        fail ("unknown field mapping kind \"" <> cs kind <> "\" (valid: field label attr)")
    when (kind == "field" && isNothing (parseAlertField key)) do
        fail ("unknown alert field \"" <> cs key <> "\" (valid: env host service check severity status)")
    pure FieldMappingItem{..}

parseFieldMappingScope :: Text -> Aeson.Object -> Parser [FieldMappingItem]
parseFieldMappingScope facet o =
    forM (KeyMap.toList o) \(key, value) -> do
        rank <- parseIntKey "field mapping rank" (Key.toText key)
        Aeson.withObject "fieldMappings" (parseFieldMappingItem facet rank) value <?> Key key

parseDashboardItem :: Text -> Text -> Aeson.Object -> Parser DashboardItem
parseDashboardItem userEmail name o = do
    rejectUnknownFields ["config", "position", "isDefault"] o
    config <- o .:? "config" .!= Aeson.toJSON ([] :: [Value])
    position <- o .:? "position" .!= 0
    isDefault <- o .:? "isDefault" .!= False
    case decodeDashboardConfig config of
        Left err -> fail ("invalid config for dashboard \"" <> cs name <> "\": " <> cs err)
        Right _ -> pure ()
    pure DashboardItem{..}

parseDashboardScope :: Text -> Aeson.Object -> Parser [DashboardItem]
parseDashboardScope userEmail o =
    forM (KeyMap.toList o) \(key, value) ->
        Aeson.withObject "dashboards" (parseDashboardItem userEmail (Key.toText key)) value <?> Key key

instance FromJSON ProvisionConfig where
    parseJSON = Aeson.withObject "provision config" \o -> do
        rejectUnknownFields ["strict", "users", "sources", "teams", "llm", "fieldMappings", "dashboards", "jiraConfigs", "cmdbConfigs", "autoAnalyze"] o
        strict <- o .:? "strict" .!= False
        users <- parseSection "users" parseUserItem o
        sources <- parseSection "sources" parseSourceItem o
        teams <- parseSection "teams" parseTeamItem o
        llm <- parseSection "llm" parseLlmItem o
        fieldMappings <- fmap concat <$> parseSection "fieldMappings" parseFieldMappingScope o
        dashboards <- fmap concat <$> parseSection "dashboards" parseDashboardScope o
        jiraConfigs <- parseSection "jiraConfigs" parseJiraConfigItem o
        cmdbConfigs <- parseSection "cmdbConfigs" parseCmdbConfigItem o
        autoAnalyze <- o .:? "autoAnalyze"
        pure ProvisionConfig{..}

parseJiraConfigItem :: Text -> Aeson.Object -> Parser JiraConfigItem
parseJiraConfigItem jiraConfigName o = do
    rejectUnknownFields ["baseUrl", "tokenEnv", "apiVersion", "projects", "enabled"] o
    jiraBaseUrl <- o .: "baseUrl"
    jiraTokenEnv <- o .: "tokenEnv"
    jiraApiVersion <- o .:? "apiVersion" .!= "3"
    jiraProjects <- o .:? "projects" .!= []
    jiraEnabled <- o .:? "enabled" .!= True
    unless (jiraApiVersion `elem` ["2", "3"]) do
        fail ("unknown jira apiVersion \"" <> cs jiraApiVersion <> "\" (valid: 2 3)")
    pure JiraConfigItem{..}

parseCmdbConfigItem :: Text -> Aeson.Object -> Parser CmdbConfigItem
parseCmdbConfigItem cmdbConfigName o = do
    rejectUnknownFields ["baseUrl", "tokenEnv", "spaces", "enabled"] o
    cmdbBaseUrl <- o .: "baseUrl"
    cmdbTokenEnv <- o .: "tokenEnv"
    cmdbSpaces <- o .:? "spaces" .!= []
    cmdbEnabled <- o .:? "enabled" .!= True
    pure CmdbConfigItem{..}

instance FromJSON AutoAnalyzeItem where
    parseJSON = Aeson.withObject "autoAnalyze" \o -> do
        rejectUnknownFields ["statuses", "severities", "environments", "enabled"] o
        aaItemStatuses <- o .:? "statuses" .!= ["firing", "ack"]
        aaItemSeverities <- o .:? "severities" .!= AutoAnalyze.allSeverities
        aaItemEnvironments <- o .:? "environments" .!= []
        aaItemEnabled <- o .:? "enabled" .!= True
        forM_ aaItemStatuses \status ->
            unless (status `elem` AutoAnalyze.allStatuses) do
                fail ("unknown alert status \"" <> cs status <> "\" (valid: firing ack stalled resolved)")
        forM_ aaItemSeverities \severity ->
            unless (severity `elem` AutoAnalyze.allSeverities) do
                fail ("unknown severity \"" <> cs severity <> "\" (valid: critical high warning info)")
        pure AutoAnalyzeItem{..}

configFromValue :: Value -> Either Text ProvisionConfig
configFromValue value = case parseEither parseJSON value of
    Left err -> Left (cs err)
    Right config -> Right config

parseProvisionConfig :: LByteString -> Either Text ProvisionConfig
parseProvisionConfig bytes = case Aeson.eitherDecode bytes of
    Left err -> Left ("invalid JSON: " <> cs err)
    Right value -> configFromValue value

parseProvisionConfigYaml :: ByteString -> Either Text ProvisionConfig
parseProvisionConfigYaml bytes = case Yaml.decodeEither' bytes of
    Left err -> Left ("invalid YAML: " <> tshow err)
    Right value -> configFromValue value

-- Entry point (milestone_7.md §3): read + apply, aborting startup on any error.

applyProvisionConfig :: (?modelContext :: ModelContext) => FilePath -> IO ()
applyProvisionConfig path = do
    bytes <- LBS.readFile path
    let parsed
            | takeExtension path `elem` [".yaml", ".yml"] = parseProvisionConfigYaml (LBS.toStrict bytes)
            | otherwise = parseProvisionConfig bytes
    config <- case parsed of
        Left err -> throwIO $ ProvisionError (cs path <> ": " <> err)
        Right config -> pure config
    applyUsers config.strict config.users
    applySources config.strict config.sources
    applyTeams config.strict config.teams
    applyLlm config.strict config.llm
    applyFieldMappings config.strict config.fieldMappings
    applyDashboards config.strict config.dashboards
    applyJiraConfigs config.strict config.jiraConfigs
    applyCmdbConfigs config.strict config.cmdbConfigs
    applyAutoAnalyze config.autoAnalyze
    putStrLn ("provision: applied " <> cs path)

-- Category application: one advisory-locked transaction per category
-- (milestone_7.md §3 concurrency note).

withProvisionLock :: (?modelContext :: ModelContext) => Text -> IO () -> IO ()
withProvisionLock category action = withTransaction do
    -- pg_advisory_xact_lock returns void; the SELECT 1 ... IS NULL shape is
    -- the typedSql-compatible spell (see MEMORIES typedSql notes).
    void
        ( sqlQueryTyped
            [typedSql|
        SELECT 1 WHERE pg_advisory_xact_lock(hashtextextended(${category}, 0)) IS NULL
    |] ::
            IO [Int]
        )
    action

-- Users (milestone_7.md §4)

applyUsers :: (?modelContext :: ModelContext) => Bool -> Maybe [UserItem] -> IO ()
applyUsers _ Nothing = pure ()
applyUsers strict (Just items) = withProvisionLock "users" do
    forM_ items upsertUser
    when strict (strictDeleteUsers items)

upsertUser :: (?modelContext :: ModelContext) => UserItem -> IO ()
upsertUser item = do
    let email = item.email
        passwordHash = item.passwordHash
        displayName = item.displayName
        settings = item.settings
    void $
        sqlExecTyped
            [typedSql|
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
    created <-
        sqlQueryTyped
            [typedSql|
        INSERT INTO roles (name) VALUES (${roleName})
        ON CONFLICT (name) DO NOTHING
        RETURNING id
    |]
    unless (null (created :: [Id' "roles"])) do
        putStrLn ("provision: role \"" <> roleName <> "\" auto-created with empty privileges; grant privileges in the admin UI")
    void $
        sqlExecTyped
            [typedSql|
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

applySources :: (?modelContext :: ModelContext) => Bool -> Maybe [SourceItem] -> IO ()
applySources _ Nothing = pure ()
applySources strict (Just items) = withProvisionLock "sources" do
    forM_ items upsertSource
    when strict (strictDeleteSources items)

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
    void $
        sqlExecTyped
            [typedSql|
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
                void $
                    sqlExecTyped
                        [typedSql|
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

applyTeams :: (?modelContext :: ModelContext) => Bool -> Maybe [TeamItem] -> IO ()
applyTeams _ Nothing = pure ()
applyTeams strict (Just items) = withProvisionLock "teams" do
    forM_ items (upsertTeam strict)
    when strict (strictDeleteTeams items)

upsertTeam :: (?modelContext :: ModelContext) => Bool -> TeamItem -> IO ()
upsertTeam strict item = do
    maybeTeam <- query @Team |> filterWhere (#name, item.name) |> fetchOneOrNothing
    team <- case maybeTeam of
        Nothing ->
            newRecord @Team
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
        void $
            sqlExecTyped
                [typedSql|
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

applyLlm :: (?modelContext :: ModelContext) => Bool -> Maybe [LlmItem] -> IO ()
applyLlm _ Nothing = pure ()
applyLlm strict (Just items) = withProvisionLock "llm" do
    forM_ items upsertLlm
    when strict (strictReconcileLlm items)

upsertLlm :: (?modelContext :: ModelContext) => LlmItem -> IO ()
upsertLlm item = do
    when item.enabled do
        let providerName = item.providerName
        void $
            sqlExecTyped
                [typedSql|
            UPDATE llm_configs SET enabled = false, updated_at = NOW()
            WHERE enabled AND provider_name <> ${providerName}
        |]
    maybeRow <- query @LlmConfig |> filterWhere (#providerName, item.providerName) |> fetchOneOrNothing
    now <- getCurrentTime
    _ <- case maybeRow of
        Nothing ->
            newRecord @LlmConfig
                |> set #providerName item.providerName
                |> set #endpoint item.endpoint
                |> set #model item.model
                |> set #apiKeyEnv item.apiKeyEnv
                |> set #toolsEnabled item.toolsEnabled
                |> set #enabled item.enabled
                |> createRecord
        Just row ->
            row
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
    maybeRow <-
        query @LlmPromptTemplate
            |> filterWhere (#name, item.name)
            |> filterWhere (#version, item.version)
            |> fetchOneOrNothing
    now <- getCurrentTime
    _ <- case maybeRow of
        Nothing ->
            newRecord @LlmPromptTemplate
                |> set #name item.name
                |> set #version item.version
                |> set #body item.body
                |> set #active False
                |> set #notes item.notes
                |> createRecord
        Just row ->
            row
                |> set #body item.body
                |> set #notes item.notes
                |> set #updatedAt now
                |> updateRecord
    when item.active (activatePromptTemplate item.name item.version)

-- Mirrors ActivateLlmTemplateAction: single active row per template name.
activatePromptTemplate :: (?modelContext :: ModelContext) => Text -> Int -> IO ()
activatePromptTemplate name version = do
    void $
        sqlExecTyped
            [typedSql|
        UPDATE llm_prompt_templates SET active = false, updated_at = NOW()
        WHERE name = ${name}
    |]
    void $
        sqlExecTyped
            [typedSql|
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

applyFieldMappings :: (?modelContext :: ModelContext) => Bool -> Maybe [FieldMappingItem] -> IO ()
applyFieldMappings _ Nothing = pure ()
applyFieldMappings strict (Just items) = withProvisionLock "fieldMappings" do
    forM_ items upsertFieldMapping
    when strict (strictDeleteFieldMappings items)

upsertFieldMapping :: (?modelContext :: ModelContext) => FieldMappingItem -> IO ()
upsertFieldMapping item = do
    let facet = item.facet
        rank = item.rank
        kind = item.kind
        key = item.key
        enabled = item.enabled
    void $
        sqlExecTyped
            [typedSql|
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

applyDashboards :: (?modelContext :: ModelContext) => Bool -> Maybe [DashboardItem] -> IO ()
applyDashboards _ Nothing = pure ()
applyDashboards strict (Just items) = withProvisionLock "dashboards" do
    forM_ items upsertDashboard
    when strict (strictDeleteDashboards items)

upsertDashboard :: (?modelContext :: ModelContext) => DashboardItem -> IO ()
upsertDashboard item = do
    maybeUser <- query @User |> filterWhere (#email, item.userEmail) |> fetchOneOrNothing
    user <- case maybeUser of
        Nothing -> throwIO $ ProvisionError ("dashboards." <> item.name <> ": userEmail \"" <> item.userEmail <> "\" does not resolve to any user")
        Just user -> pure user
    let userId = get #id user
    maybeRow <-
        query @Dashboard
            |> filterWhere (#userId, userId)
            |> filterWhere (#name, item.name)
            |> fetchOneOrNothing
    now <- getCurrentTime
    _ <- case maybeRow of
        Nothing ->
            newRecord @Dashboard
                |> set #userId userId
                |> set #name item.name
                |> set #config item.config
                |> set #position item.position
                |> set #isDefault item.isDefault
                |> createRecord
        Just row ->
            row
                |> set #config item.config
                |> set #position item.position
                |> set #isDefault item.isDefault
                |> set #updatedAt now
                |> updateRecord
    when item.isDefault do
        let name = item.name
        void $
            sqlExecTyped
                [typedSql|
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

-- Integration configs (milestone 10): jira_configs/cmdb_configs upsert by
-- name; tokenEnv must resolve to a set env var (secrets stay env
-- references). Nothing references these tables: strict deletes are safe.

applyJiraConfigs :: (?modelContext :: ModelContext) => Bool -> Maybe [JiraConfigItem] -> IO ()
applyJiraConfigs _ Nothing = pure ()
applyJiraConfigs strict (Just items) = withProvisionLock "jiraConfigs" do
    forM_ items upsertJiraConfig
    when strict do
        let keepNames = map (.jiraConfigName) items
        allConfigs <- query @JiraConfig |> fetch
        forM_ (filter (\row -> row.name `notElem` keepNames) allConfigs) deleteRecord

upsertJiraConfig :: (?modelContext :: ModelContext) => JiraConfigItem -> IO ()
upsertJiraConfig item = do
    validateEnvRef "jiraConfigs" item.jiraConfigName item.jiraTokenEnv
    maybeRow <- query @JiraConfig |> filterWhere (#name, item.jiraConfigName) |> fetchOneOrNothing
    now <- getCurrentTime
    let projectsJson = Aeson.toJSON item.jiraProjects
    _ <- case maybeRow of
        Nothing ->
            newRecord @JiraConfig
                |> set #name item.jiraConfigName
                |> set #baseUrl item.jiraBaseUrl
                |> set #tokenEnv item.jiraTokenEnv
                |> set #apiVersion item.jiraApiVersion
                |> set #projects projectsJson
                |> set #enabled item.jiraEnabled
                |> createRecord
        Just row ->
            row
                |> set #baseUrl item.jiraBaseUrl
                |> set #tokenEnv item.jiraTokenEnv
                |> set #apiVersion item.jiraApiVersion
                |> set #projects projectsJson
                |> set #enabled item.jiraEnabled
                |> set #updatedAt now
                |> updateRecord
    pure ()

applyCmdbConfigs :: (?modelContext :: ModelContext) => Bool -> Maybe [CmdbConfigItem] -> IO ()
applyCmdbConfigs _ Nothing = pure ()
applyCmdbConfigs strict (Just items) = withProvisionLock "cmdbConfigs" do
    forM_ items upsertCmdbConfig
    when strict do
        let keepNames = map (.cmdbConfigName) items
        allConfigs <- query @CmdbConfig |> fetch
        forM_ (filter (\row -> row.name `notElem` keepNames) allConfigs) deleteRecord

upsertCmdbConfig :: (?modelContext :: ModelContext) => CmdbConfigItem -> IO ()
upsertCmdbConfig item = do
    validateEnvRef "cmdbConfigs" item.cmdbConfigName item.cmdbTokenEnv
    maybeRow <- query @CmdbConfig |> filterWhere (#name, item.cmdbConfigName) |> fetchOneOrNothing
    now <- getCurrentTime
    let spacesJson = Aeson.toJSON item.cmdbSpaces
    _ <- case maybeRow of
        Nothing ->
            newRecord @CmdbConfig
                |> set #name item.cmdbConfigName
                |> set #baseUrl item.cmdbBaseUrl
                |> set #tokenEnv item.cmdbTokenEnv
                |> set #spaces spacesJson
                |> set #enabled item.cmdbEnabled
                |> createRecord
        Just row ->
            row
                |> set #baseUrl item.cmdbBaseUrl
                |> set #tokenEnv item.cmdbTokenEnv
                |> set #spaces spacesJson
                |> set #enabled item.cmdbEnabled
                |> set #updatedAt now
                |> updateRecord
    pure ()

validateEnvRef :: (?modelContext :: ModelContext) => Text -> Text -> Text -> IO ()
validateEnvRef category name envVar = do
    maybeValue <- lookupEnv (cs envVar)
    when (isNothing maybeValue) do
        throwIO $ ProvisionError (category <> "." <> name <> ": tokenEnv \"" <> envVar <> "\" is not set")

-- Auto-analysis gate (milestone 10 §5): upsert the singleton row; an absent
-- section leaves both the row and the no-row defaults untouched.
applyAutoAnalyze :: (?modelContext :: ModelContext) => Maybe AutoAnalyzeItem -> IO ()
applyAutoAnalyze Nothing = pure ()
applyAutoAnalyze (Just item) = withProvisionLock "autoAnalyze" do
    existing <- query @LlmAutoAnalyzeConfig |> fetch
    now <- getCurrentTime
    let statusesJson = Aeson.toJSON item.aaItemStatuses
        severitiesJson = Aeson.toJSON item.aaItemSeverities
        environmentsJson = Aeson.toJSON item.aaItemEnvironments
    _ <- case existing of
        (row : _) ->
            row
                |> set #statuses statusesJson
                |> set #severities severitiesJson
                |> set #environments environmentsJson
                |> set #enabled item.aaItemEnabled
                |> set #updatedAt now
                |> updateRecord
        [] ->
            createRecord
                ( newRecord @LlmAutoAnalyzeConfig
                    |> set #statuses statusesJson
                    |> set #severities severitiesJson
                    |> set #environments environmentsJson
                    |> set #enabled item.aaItemEnabled
                )
    pure ()
