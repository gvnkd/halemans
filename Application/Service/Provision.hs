module Application.Service.Provision (
    ProvisionConfig (..),
    UserItem (..),
    RoleItem (..),
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
    AssetsConfigItem (..),
    GroupingRuleItem (..),
    NotificationRuleItem (..),
    EscalationPolicyItem (..),
    EscalationStepItem (..),
    LlmAgentRoleItem (..),
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
import Application.Service.Llm.Tools (toolDefinitions)
import Application.Service.PollerControl (ensurePollerForSourceType)
import Control.Exception (Exception, SomeException, try)
import Control.Monad (void)
import Data.Aeson (FromJSON, Value, parseJSON, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (JSONPathElement (..), Parser, parseEither, parseMaybe, (<?>))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import Generated.Types
import IHP.Fetch (fetch, fetchCount, fetchOneOrNothing)
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
    , roles :: Maybe [RoleItem]
    , sources :: Maybe [SourceItem]
    , teams :: Maybe [TeamItem]
    , llm :: Maybe [LlmItem]
    , fieldMappings :: Maybe [FieldMappingItem]
    , dashboards :: Maybe [DashboardItem]
    , jiraConfigs :: Maybe [JiraConfigItem]
    , cmdbConfigs :: Maybe [CmdbConfigItem]
    , assetsConfigs :: Maybe [AssetsConfigItem]
    , groupingRules :: Maybe [GroupingRuleItem]
    , notificationRules :: Maybe [NotificationRuleItem]
    , escalationPolicies :: Maybe [EscalationPolicyItem]
    , llmAgentRoles :: Maybe [LlmAgentRoleItem]
    , autoAnalyze :: Maybe AutoAnalyzeItem
    }
    deriving (Eq, Show)

data UserItem = UserItem
    { email :: Text
    , displayName :: Text
    , passwordHash :: Text
    , roles :: [Text]
    , settings :: Value
    , itemProtected :: Bool
    }
    deriving (Eq, Show)

-- Role definitions (privilege matrix). Applied BEFORE users so the users
-- section's auto-create of unknown roles doesn't shadow provisioned
-- privileges with an empty set.
data RoleItem = RoleItem
    { roleName :: Text
    , rolePrivileges :: [Text]
    , roleProtected :: Bool
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
    , itemProtected :: Bool
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
    , itemProtected :: Bool
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
    , itemProtected :: Bool
    }
    deriving (Eq, Show)

data PromptTemplateItem = PromptTemplateItem
    { name :: Text
    , version :: Int
    , body :: Text
    , active :: Bool
    , notes :: Maybe Text
    , itemProtected :: Bool
    }
    deriving (Eq, Show)

data FieldMappingItem = FieldMappingItem
    { facet :: Text
    , rank :: Int
    , kind :: Text
    , key :: Text
    , enabled :: Bool
    , itemProtected :: Bool
    }
    deriving (Eq, Show)

data DashboardItem = DashboardItem
    { name :: Text
    , userEmail :: Text
    , config :: Value
    , position :: Int
    , isDefault :: Bool
    , itemProtected :: Bool
    }
    deriving (Eq, Show)

data JiraConfigItem = JiraConfigItem
    { jiraConfigName :: Text
    , jiraBaseUrl :: Text
    , jiraTokenEnv :: Text
    , jiraApiVersion :: Text
    , jiraProjects :: [Text]
    , jiraEnabled :: Bool
    , jiraProtected :: Bool
    }
    deriving (Eq, Show)

data CmdbConfigItem = CmdbConfigItem
    { cmdbConfigName :: Text
    , cmdbBaseUrl :: Text
    , cmdbTokenEnv :: Text
    , cmdbSpaces :: [Text]
    , cmdbEnabled :: Bool
    , cmdbProtected :: Bool
    }
    deriving (Eq, Show)

-- Assets info sources (admin → Assets info sources page). tokenEnv must
-- resolve like jira/cmdb configs; authMode bearer|basic mirrors the UI form.
data AssetsConfigItem = AssetsConfigItem
    { acName :: Text
    , acBaseUrl :: Text
    , acTokenEnv :: Text
    , acAuthMode :: Text
    , acJiraEmailEnv :: Maybe Text
    , acDefaultSchemaName :: Text
    , acHostQueryTemplate :: Text
    , acAttributeNames :: Text
    , acEnabled :: Bool
    , acProtected :: Bool
    }
    deriving (Eq, Show)

-- Grouping rules: the match jsonb uses the same shape the UI stores
-- ({"fields": {...}, "labels": {...}, "facets": {...}}); field names are
-- validated like field mappings.
data GroupingRuleItem = GroupingRuleItem
    { grName :: Text
    , grPosition :: Int
    , grEnabled :: Bool
    , grMatch :: Value
    , grGroupKeyTemplate :: Text
    , grProtected :: Bool
    }
    deriving (Eq, Show)

-- Escalation policies: steps reference their target by team NAME or user
-- EMAIL (natural keys — UUIDs don't survive a clean-DB copy) and are
-- resolved to ids at apply time.
data EscalationPolicyItem = EscalationPolicyItem
    { epName :: Text
    , epSteps :: [EscalationStepItem]
    , epProtected :: Bool
    }
    deriving (Eq, Show)

data EscalationStepItem = EscalationStepItem
    { esAfterSeconds :: Int
    , esTargetTeam :: Maybe Text
    , esTargetUser :: Maybe Text
    , esUnlessStatus :: Maybe Text
    }
    deriving (Eq, Show)

-- Notification rules: match shape as in grouping rules (fields+labels);
-- target is team NAME or user EMAIL (XOR); escalationPolicy by name.
data NotificationRuleItem = NotificationRuleItem
    { nrName :: Text
    , nrPosition :: Int
    , nrEnabled :: Bool
    , nrMatch :: Value
    , nrSeverityThreshold :: Text
    , nrTeam :: Maybe Text
    , nrUser :: Maybe Text
    , nrChannel :: Text
    , nrChannelConfig :: Value
    , nrThrottleSeconds :: Int
    , nrEscalationPolicy :: Maybe Text
    , nrProtected :: Bool
    }
    deriving (Eq, Show)

-- LLM agent roles (admin → LLM → Roles): tools whitelist must be a subset
-- of the built-in tool definition names.
data LlmAgentRoleItem = LlmAgentRoleItem
    { arName :: Text
    , arDescription :: Text
    , arPromptTemplateName :: Text
    , arTools :: [Text]
    , arEnabled :: Bool
    , arIsDefault :: Bool
    , arProtected :: Bool
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

-- Every item accepts a per-item "protected" flag (default true): protected
-- items are read-only in the admin UI and badged as provision-managed. The
-- flag is re-asserted on every apply; an item that disappears from a PRESENT
-- section loses protection (it left the provisioned set), while a section
-- absent from the file leaves its rows untouched.
parseProtectedFlag :: Aeson.Object -> Parser Bool
parseProtectedFlag o = o .:? "protected" .!= True

parseUserItem :: Text -> Aeson.Object -> Parser UserItem
parseUserItem email o = do
    rejectUnknownFields ["displayName", "passwordHash", "roles", "settings", "protected"] o
    displayName <- o .:? "displayName" .!= ""
    passwordHash <- o .: "passwordHash"
    roles <- o .:? "roles" .!= []
    settings <- o .:? "settings" .!= Aeson.object []
    itemProtected <- parseProtectedFlag o
    validateTheme settings
    validateTimezone settings
    pure UserItem{..}

parseRoleItem :: Text -> Aeson.Object -> Parser RoleItem
parseRoleItem roleName o = do
    rejectUnknownFields ["privileges", "protected"] o
    rolePrivileges <- o .:? "privileges" .!= []
    roleProtected <- parseProtectedFlag o
    pure RoleItem{..}

-- Validates the rule `match` jsonb shape ({"fields": {...}, "labels":
-- {...}, "facets": {...}}) and that every field key is a known alert field.
validateMatchJson :: Value -> Parser ()
validateMatchJson = \case
    Aeson.Object o -> do
        forM_ ["fields", "labels", "facets"] \sectionKey ->
            case KeyMap.lookup (Key.fromText sectionKey) o of
                Nothing -> pure ()
                Just (Aeson.Object section) ->
                    forM_ (KeyMap.keys section) \fieldKey ->
                        when (sectionKey == "fields") $
                            case parseAlertField (Key.toText fieldKey) of
                                Just _ -> pure ()
                                Nothing -> fail ("unknown alert field \"" <> cs (Key.toText fieldKey) <> "\" in match.fields (valid: env host service check severity status)")
                Just _ -> fail (cs ("match." <> sectionKey <> " must be an object"))
        case filter (`notElem` ["fields", "labels", "facets"]) (map Key.toText (KeyMap.keys o)) of
            [] -> pure ()
            (unknown : _) -> fail (cs ("unknown match section \"" <> unknown <> "\""))
    _ -> fail "match must be an object"

validateTheme :: Value -> Parser ()
validateTheme settings = case settings of
    Aeson.Object o -> case KeyMap.lookup "theme" o of
        Nothing -> pure ()
        Just (Aeson.String theme)
            | isValidTheme theme -> pure ()
            | otherwise -> fail ("unknown theme \"" <> cs theme <> "\" (valid: latte frappe macchiato dracula light dark halemans-dark halemans-light)")
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
    rejectUnknownFields ["type", "baseUrl", "env", "pollIntervalSeconds", "enabled", "config", "webhookTokens", "hostGroupsFile", "protected"] o
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
    itemProtected <- parseProtectedFlag o
    pure SourceItem{..}

parseMemberItem :: Text -> Aeson.Object -> Parser MemberItem
parseMemberItem email o = do
    rejectUnknownFields ["role"] o
    rawRole <- o .:? "role" .!= "member"
    let role = if rawRole == "" then "member" else rawRole
    pure MemberItem{..}

parseTeamItem :: Text -> Aeson.Object -> Parser TeamItem
parseTeamItem name o = do
    rejectUnknownFields ["description", "hostGroups", "defaults", "members", "defaultDashboardConfig", "protected"] o
    -- Absent keys stay Nothing so re-provisioning does not clobber
    -- UI-edited values; an explicit key (even [] or "") overwrites.
    description <- o .:? "description"
    hostGroups <- o .:? "hostGroups"
    defaults <- o .:? "defaults"
    members <- case KeyMap.lookup "members" o of
        Nothing -> pure []
        Just value -> parseKeyed "members" parseMemberItem value <?> Key "members"
    defaultDashboardConfig <- o .:? "defaultDashboardConfig"
    itemProtected <- parseProtectedFlag o
    pure TeamItem{..}

parsePromptTemplateItem :: Text -> Int -> Aeson.Object -> Parser PromptTemplateItem
parsePromptTemplateItem name version o = do
    rejectUnknownFields ["body", "active", "notes", "protected"] o
    body <- o .: "body"
    active <- o .:? "active" .!= False
    notes <- o .:? "notes"
    itemProtected <- parseProtectedFlag o
    pure PromptTemplateItem{..}

parsePromptTemplateScope :: Text -> Aeson.Object -> Parser [PromptTemplateItem]
parsePromptTemplateScope name o =
    forM (KeyMap.toList o) \(key, value) -> do
        version <- parseIntKey "prompt template version" (Key.toText key)
        Aeson.withObject "promptTemplates" (parsePromptTemplateItem name version) value <?> Key key

parseLlmItem :: Text -> Aeson.Object -> Parser LlmItem
parseLlmItem providerName o = do
    rejectUnknownFields ["endpoint", "model", "apiKeyEnv", "toolsEnabled", "enabled", "promptTemplates", "protected"] o
    endpoint <- o .: "endpoint"
    model <- o .: "model"
    apiKeyEnv <- o .:? "apiKeyEnv"
    toolsEnabled <- o .:? "toolsEnabled" .!= False
    enabled <- o .:? "enabled" .!= False
    promptTemplates <- case KeyMap.lookup "promptTemplates" o of
        Nothing -> pure []
        Just value -> fmap concat (parseKeyed "promptTemplates" parsePromptTemplateScope value) <?> Key "promptTemplates"
    itemProtected <- parseProtectedFlag o
    pure LlmItem{..}

parseFieldMappingItem :: Text -> Int -> Aeson.Object -> Parser FieldMappingItem
parseFieldMappingItem facet rank o = do
    rejectUnknownFields ["kind", "key", "enabled", "protected"] o
    kind <- o .: "kind"
    key <- o .: "key"
    enabled <- o .:? "enabled" .!= True
    unless (kind `elem` ["field", "label", "attr"]) do
        fail ("unknown field mapping kind \"" <> cs kind <> "\" (valid: field label attr)")
    when (kind == "field" && isNothing (parseAlertField key)) do
        fail ("unknown alert field \"" <> cs key <> "\" (valid: env host service check severity status)")
    itemProtected <- parseProtectedFlag o
    pure FieldMappingItem{..}

parseFieldMappingScope :: Text -> Aeson.Object -> Parser [FieldMappingItem]
parseFieldMappingScope facet o =
    forM (KeyMap.toList o) \(key, value) -> do
        rank <- parseIntKey "field mapping rank" (Key.toText key)
        Aeson.withObject "fieldMappings" (parseFieldMappingItem facet rank) value <?> Key key

parseDashboardItem :: Text -> Text -> Aeson.Object -> Parser DashboardItem
parseDashboardItem userEmail name o = do
    rejectUnknownFields ["config", "position", "isDefault", "protected"] o
    config <- o .:? "config" .!= Aeson.toJSON ([] :: [Value])
    position <- o .:? "position" .!= 0
    isDefault <- o .:? "isDefault" .!= False
    itemProtected <- parseProtectedFlag o
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
        rejectUnknownFields ["strict", "users", "roles", "sources", "teams", "llm", "fieldMappings", "dashboards", "jiraConfigs", "cmdbConfigs", "assetsConfigs", "groupingRules", "notificationRules", "escalationPolicies", "llmAgentRoles", "autoAnalyze"] o
        strict <- o .:? "strict" .!= False
        users <- parseSection "users" parseUserItem o
        roles <- parseSection "roles" parseRoleItem o
        sources <- parseSection "sources" parseSourceItem o
        teams <- parseSection "teams" parseTeamItem o
        llm <- parseSection "llm" parseLlmItem o
        fieldMappings <- fmap concat <$> parseSection "fieldMappings" parseFieldMappingScope o
        dashboards <- fmap concat <$> parseSection "dashboards" parseDashboardScope o
        jiraConfigs <- parseSection "jiraConfigs" parseJiraConfigItem o
        cmdbConfigs <- parseSection "cmdbConfigs" parseCmdbConfigItem o
        assetsConfigs <- parseSection "assetsConfigs" parseAssetsConfigItem o
        groupingRules <- parseSection "groupingRules" parseGroupingRuleItem o
        notificationRules <- parseSection "notificationRules" parseNotificationRuleItem o
        escalationPolicies <- parseSection "escalationPolicies" parseEscalationPolicyItem o
        llmAgentRoles <- parseSection "llmAgentRoles" parseLlmAgentRoleItem o
        autoAnalyze <- o .:? "autoAnalyze"
        pure ProvisionConfig{..}

parseJiraConfigItem :: Text -> Aeson.Object -> Parser JiraConfigItem
parseJiraConfigItem jiraConfigName o = do
    rejectUnknownFields ["baseUrl", "tokenEnv", "apiVersion", "projects", "enabled", "protected"] o
    jiraBaseUrl <- o .: "baseUrl"
    jiraTokenEnv <- o .: "tokenEnv"
    jiraApiVersion <- o .:? "apiVersion" .!= "3"
    jiraProjects <- o .:? "projects" .!= []
    jiraEnabled <- o .:? "enabled" .!= True
    jiraProtected <- parseProtectedFlag o
    unless (jiraApiVersion `elem` ["2", "3"]) do
        fail ("unknown jira apiVersion \"" <> cs jiraApiVersion <> "\" (valid: 2 3)")
    pure JiraConfigItem{..}

parseCmdbConfigItem :: Text -> Aeson.Object -> Parser CmdbConfigItem
parseCmdbConfigItem cmdbConfigName o = do
    rejectUnknownFields ["baseUrl", "tokenEnv", "spaces", "enabled", "protected"] o
    cmdbBaseUrl <- o .: "baseUrl"
    cmdbTokenEnv <- o .: "tokenEnv"
    cmdbSpaces <- o .:? "spaces" .!= []
    cmdbEnabled <- o .:? "enabled" .!= True
    cmdbProtected <- parseProtectedFlag o
    pure CmdbConfigItem{..}

parseAssetsConfigItem :: Text -> Aeson.Object -> Parser AssetsConfigItem
parseAssetsConfigItem acName o = do
    rejectUnknownFields ["baseUrl", "tokenEnv", "authMode", "jiraEmailEnv", "defaultSchemaName", "hostQueryTemplate", "attributeNames", "enabled", "protected"] o
    acBaseUrl <- o .: "baseUrl"
    acTokenEnv <- o .: "tokenEnv"
    acAuthMode <- o .:? "authMode" .!= "bearer"
    unless (acAuthMode `elem` ["bearer", "basic"]) do
        fail ("unknown assets authMode \"" <> cs acAuthMode <> "\" (valid: bearer basic)")
    acJiraEmailEnv <- o .:? "jiraEmailEnv"
    when (acAuthMode == "basic" && isNothing acJiraEmailEnv) do
        fail "assetsConfigs: authMode basic requires jiraEmailEnv"
    acDefaultSchemaName <- o .:? "defaultSchemaName" .!= ""
    acHostQueryTemplate <- o .:? "hostQueryTemplate" .!= ""
    acAttributeNames <- o .:? "attributeNames" .!= "Owner,Cluster,Database,IP,Datacenter"
    acEnabled <- o .:? "enabled" .!= True
    acProtected <- parseProtectedFlag o
    pure AssetsConfigItem{..}

parseGroupingRuleItem :: Text -> Aeson.Object -> Parser GroupingRuleItem
parseGroupingRuleItem grName o = do
    rejectUnknownFields ["position", "enabled", "match", "groupKeyTemplate", "protected"] o
    grPosition <- o .:? "position" .!= 0
    grEnabled <- o .:? "enabled" .!= True
    grMatch <- o .:? "match" .!= Aeson.object []
    validateMatchJson grMatch
    grGroupKeyTemplate <- o .:? "groupKeyTemplate" .!= ""
    grProtected <- parseProtectedFlag o
    pure GroupingRuleItem{..}

parseEscalationStepItem :: Aeson.Object -> Parser EscalationStepItem
parseEscalationStepItem o = do
    rejectUnknownFields ["afterSeconds", "targetTeam", "targetUser", "unlessStatus"] o
    esAfterSeconds <- o .: "afterSeconds"
    -- 0 is meaningful at runtime (immediately-due tracker, see
    -- Application.Service.Escalation createTracker) even though the UI form
    -- only offers positive delays.
    unless (esAfterSeconds >= 0) do
        fail "escalation step afterSeconds must not be negative"
    esTargetTeam <- o .:? "targetTeam"
    esTargetUser <- o .:? "targetUser"
    when (isJust esTargetTeam && isJust esTargetUser) do
        fail "escalation step targets one of targetTeam or targetUser, not both"
    esUnlessStatus <- o .:? "unlessStatus"
    pure EscalationStepItem{..}

instance FromJSON EscalationStepItem where
    parseJSON = Aeson.withObject "escalation step" parseEscalationStepItem

parseEscalationPolicyItem :: Text -> Aeson.Object -> Parser EscalationPolicyItem
parseEscalationPolicyItem epName o = do
    rejectUnknownFields ["steps", "protected"] o
    epSteps <- o .: "steps"
    when (null epSteps) do
        fail "escalation policy needs at least one step"
    epProtected <- parseProtectedFlag o
    pure EscalationPolicyItem{..}

parseNotificationRuleItem :: Text -> Aeson.Object -> Parser NotificationRuleItem
parseNotificationRuleItem nrName o = do
    rejectUnknownFields ["position", "enabled", "match", "severityThreshold", "team", "user", "channel", "channelConfig", "throttleSeconds", "escalationPolicy", "protected"] o
    nrPosition <- o .:? "position" .!= 0
    nrEnabled <- o .:? "enabled" .!= True
    nrMatch <- o .:? "match" .!= Aeson.object []
    validateMatchJson nrMatch
    nrSeverityThreshold <- o .:? "severityThreshold" .!= "info"
    unless (nrSeverityThreshold `elem` AutoAnalyze.allSeverities) do
        fail ("unknown severity \"" <> cs nrSeverityThreshold <> "\" (valid: critical high warning info)")
    nrTeam <- o .:? "team"
    nrUser <- o .:? "user"
    when (isJust nrTeam && isJust nrUser) do
        fail "notification rule targets one of team or user, not both"
    nrChannel <- o .:? "channel" .!= "browser_push"
    when (Text.null nrChannel) do
        fail "channel must not be empty"
    nrChannelConfig <- o .:? "channelConfig" .!= Aeson.object []
    nrThrottleSeconds <- o .:? "throttleSeconds" .!= 300
    nrEscalationPolicy <- o .:? "escalationPolicy"
    nrProtected <- parseProtectedFlag o
    pure NotificationRuleItem{..}

parseLlmAgentRoleItem :: Text -> Aeson.Object -> Parser LlmAgentRoleItem
parseLlmAgentRoleItem arName o = do
    rejectUnknownFields ["description", "promptTemplateName", "tools", "enabled", "isDefault", "protected"] o
    arDescription <- o .:? "description" .!= ""
    arPromptTemplateName <- o .: "promptTemplateName"
    arTools <- o .:? "tools" .!= []
    let unknownTools = filter (`notElem` knownToolNames) arTools
    unless (null unknownTools) do
        fail ("unknown llm tool(s) " <> cs (Text.intercalate ", " unknownTools) <> " (valid: " <> cs (Text.intercalate ", " knownToolNames) <> ")")
    arEnabled <- o .:? "enabled" .!= True
    arIsDefault <- o .:? "isDefault" .!= False
    arProtected <- parseProtectedFlag o
    pure LlmAgentRoleItem{..}

-- Tool whitelist = the names of the built-in tool definitions (same source
-- the admin UI picker uses).
knownToolNames :: [Text]
knownToolNames =
    [ name
    | definition <- toolDefinitions
    , Just name <- [parseMaybe toolName definition]
    ]
  where
    toolName = Aeson.withObject "tool" \obj -> do
        function <- obj Aeson..: "function"
        function Aeson..: "name"

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
    applyRoles config.strict config.roles
    applyUsers config.strict config.users
    applySources config.strict config.sources
    applyTeams config.strict config.teams
    applyLlm config.strict config.llm
    applyFieldMappings config.strict config.fieldMappings
    applyDashboards config.strict config.dashboards
    applyJiraConfigs config.strict config.jiraConfigs
    applyCmdbConfigs config.strict config.cmdbConfigs
    applyAssetsConfigs config.strict config.assetsConfigs
    applyGroupingRules config.strict config.groupingRules
    applyEscalationPolicies config.strict config.escalationPolicies
    applyNotificationRules config.strict config.notificationRules
    applyLlmAgentRoles config.strict config.llmAgentRoles
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

-- Roles: the privilege matrix itself is provisionable (the auto-create path
-- in the users section creates unknown roles with empty privileges — this
-- section runs first so that never shadows an explicit definition). Strict
-- delete removes user_roles links first.

applyRoles :: (?modelContext :: ModelContext) => Bool -> Maybe [RoleItem] -> IO ()
applyRoles _ Nothing = pure ()
applyRoles strict (Just items) = withProvisionLock "roles" do
    forM_ items upsertRole
    unless strict (unprotectRoles items)
    when strict (strictDeleteRoles items)

upsertRole :: (?modelContext :: ModelContext) => RoleItem -> IO ()
upsertRole item = do
    let roleName = item.roleName
        privileges = item.rolePrivileges
        itemProtected = item.roleProtected
    void $
        sqlExecTyped
            [typedSql|
        INSERT INTO roles (name, privileges, protected)
        VALUES (${roleName}, ${privileges}, ${itemProtected})
        ON CONFLICT (name) DO UPDATE SET
            privileges = EXCLUDED.privileges,
            protected = EXCLUDED.protected
    |]

unprotectRoles :: (?modelContext :: ModelContext) => [RoleItem] -> IO ()
unprotectRoles items =
    let keepNames = map (.roleName) items
     in unprotectRows (query @Role |> fetch) (\role -> role.name `notElem` keepNames) updateRecord

strictDeleteRoles :: (?modelContext :: ModelContext) => [RoleItem] -> IO ()
strictDeleteRoles items = do
    let keepNames = map (.roleName) items
    allRoles <- query @Role |> fetch
    forM_ (filter (\role -> role.name `notElem` keepNames) allRoles) \role -> do
        let roleId = get #id role
        result <- try do
            void $ sqlExecTyped [typedSql| DELETE FROM user_roles WHERE role_id = ${roleId} |]
            deleteRecord role
        case result of
            Left err -> throwIO $ ProvisionError ("roles: cannot delete role \"" <> role.name <> "\": " <> tshow (err :: SomeException))
            Right () -> pure ()

-- Users (milestone_7.md §4)

-- Un-protect pass (per present section, non-strict): rows whose natural key
-- left the provisioned set keep their content but lose the protection flag
-- so the admin UI can edit them again. Rows never provisioned are already
-- unprotected, so a blanket clear is a no-op for them.
unprotectRows :: (?modelContext :: ModelContext, HasField "protected" record Bool, SetField "protected" record Bool) => IO [record] -> (record -> Bool) -> (record -> IO record) -> IO ()
unprotectRows fetchAll isKeyedOut updateRow = do
    rows <- fetchAll
    forM_ (filter (\row -> isKeyedOut row && get #protected row) rows) \row -> do
        void (updateRow (set #protected False row))

applyUsers :: (?modelContext :: ModelContext) => Bool -> Maybe [UserItem] -> IO ()
applyUsers _ Nothing = pure ()
applyUsers strict (Just items) = withProvisionLock "users" do
    forM_ items upsertUser
    unless strict (unprotectUsers items)
    when strict (strictDeleteUsers items)

unprotectUsers :: (?modelContext :: ModelContext) => [UserItem] -> IO ()
unprotectUsers items =
    let keepEmails = map (.email) items
     in unprotectRows (query @User |> fetch) (\user -> user.email `notElem` keepEmails) updateRecord

upsertUser :: (?modelContext :: ModelContext) => UserItem -> IO ()
upsertUser item = do
    let email = item.email
        passwordHash = item.passwordHash
        displayName = item.displayName
        settings = item.settings
        itemProtected = item.itemProtected
    void $
        sqlExecTyped
            [typedSql|
        INSERT INTO users (email, password_hash, display_name, settings, protected)
        VALUES (${email}, ${passwordHash}, ${displayName}, ${settings}, ${itemProtected})
        ON CONFLICT (email) DO UPDATE SET
            password_hash = EXCLUDED.password_hash,
            display_name = EXCLUDED.display_name,
            settings = users.settings || EXCLUDED.settings,
            protected = EXCLUDED.protected
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
    unless strict (unprotectSources items)
    when strict (strictDeleteSources items)

unprotectSources :: (?modelContext :: ModelContext) => [SourceItem] -> IO ()
unprotectSources items =
    let keepNames = map (.name) items
     in unprotectRows (query @Source |> fetch) (\source -> source.name `notElem` keepNames) updateRecord

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
        itemProtected = item.itemProtected
    void $
        sqlExecTyped
            [typedSql|
        INSERT INTO sources (type, name, base_url, env, poll_interval_seconds, enabled, config, protected)
        VALUES (${sourceType}, ${name}, ${baseUrl}, ${env}, ${pollIntervalSeconds}, ${enabled}, ${config}, ${itemProtected})
        ON CONFLICT (name) DO UPDATE SET
            type = EXCLUDED.type,
            base_url = EXCLUDED.base_url,
            env = EXCLUDED.env,
            poll_interval_seconds = EXCLUDED.poll_interval_seconds,
            enabled = EXCLUDED.enabled,
            config = EXCLUDED.config,
            protected = EXCLUDED.protected
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
            void $ sqlExecTyped [typedSql| DELETE FROM metric_cache WHERE source_id = ${sourceId} |]
            deleteRecord source
        case result of
            Left err -> throwIO $ ProvisionError ("sources: cannot delete source \"" <> source.name <> "\" (referenced rows must go first; or keep it with \"enabled\": false): " <> tshow (err :: SomeException))
            Right () -> pure ()

-- Teams (milestone_7.md §6)

applyTeams :: (?modelContext :: ModelContext) => Bool -> Maybe [TeamItem] -> IO ()
applyTeams _ Nothing = pure ()
applyTeams strict (Just items) = withProvisionLock "teams" do
    forM_ items (upsertTeam strict)
    unless strict (unprotectTeams items)
    when strict (strictDeleteTeams items)

unprotectTeams :: (?modelContext :: ModelContext) => [TeamItem] -> IO ()
unprotectTeams items =
    let keepNames = map (.name) items
     in unprotectRows (query @Team |> fetch) (\team -> team.name `notElem` keepNames) updateRecord

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
                |> set #protected item.itemProtected
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
                withProtected = withDefaults |> set #protected item.itemProtected
            case item.defaultDashboardConfig of
                Just dashboardConfig -> withProtected |> set #defaultDashboardConfig (Just dashboardConfig) |> updateRecord
                Nothing -> updateRecord withProtected
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
    unless strict do
        unprotectLlm items
        -- Template un-protect is name-scoped like the strict reconcile:
        -- versions of a provisioned NAME that left the file lose
        -- protection; names never provisioned stay in the UI namespace.
        let provisionedNames = List.nub [template.name | item <- items, template <- item.promptTemplates]
        forM_ provisionedNames \name -> do
            let keepVersions = [template.version | item <- items, template <- item.promptTemplates, template.name == name]
            unprotectRows
                (query @LlmPromptTemplate |> filterWhere (#name, name) |> fetch)
                (\row -> row.version `notElem` keepVersions)
                updateRecord
    when strict (strictReconcileLlm items)

unprotectLlm :: (?modelContext :: ModelContext) => [LlmItem] -> IO ()
unprotectLlm items =
    let keepNames = map (.providerName) items
     in unprotectRows (query @LlmConfig |> fetch) (\row -> row.providerName `notElem` keepNames) updateRecord

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
                |> set #protected item.itemProtected
                |> createRecord
        Just row ->
            row
                |> set #endpoint item.endpoint
                |> set #model item.model
                |> set #apiKeyEnv item.apiKeyEnv
                |> set #toolsEnabled item.toolsEnabled
                |> set #enabled item.enabled
                |> set #protected item.itemProtected
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
                |> set #protected item.itemProtected
                |> createRecord
        Just row ->
            row
                |> set #body item.body
                |> set #notes item.notes
                |> set #protected item.itemProtected
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
    unless strict (unprotectFieldMappings items)
    when strict (strictDeleteFieldMappings items)

unprotectFieldMappings :: (?modelContext :: ModelContext) => [FieldMappingItem] -> IO ()
unprotectFieldMappings items =
    let keepPairs = map (\item -> (item.facet, item.rank)) items
     in unprotectRows
            (query @FieldMapping |> fetch)
            (\mapping -> (mapping.facet, mapping.rank) `notElem` keepPairs)
            updateRecord

upsertFieldMapping :: (?modelContext :: ModelContext) => FieldMappingItem -> IO ()
upsertFieldMapping item = do
    let facet = item.facet
        rank = item.rank
        kind = item.kind
        key = item.key
        enabled = item.enabled
        itemProtected = item.itemProtected
    void $
        sqlExecTyped
            [typedSql|
        INSERT INTO field_mappings (facet, rank, kind, key, enabled, protected)
        VALUES (${facet}, ${rank}, ${kind}, ${key}, ${enabled}, ${itemProtected})
        ON CONFLICT (facet, rank) DO UPDATE SET
            kind = EXCLUDED.kind,
            key = EXCLUDED.key,
            enabled = EXCLUDED.enabled,
            protected = EXCLUDED.protected,
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
    unless strict (unprotectDashboards items)
    when strict (strictDeleteDashboards items)

unprotectDashboards :: (?modelContext :: ModelContext) => [DashboardItem] -> IO ()
unprotectDashboards items = do
    let keepPairs = map (\item -> (item.userEmail, item.name)) items
    allDashboards <- query @Dashboard |> fetch
    forM_ allDashboards \dashboard -> do
        owner <- fetch dashboard.userId
        when ((owner.email, dashboard.name) `notElem` keepPairs && get #protected dashboard) do
            void (updateRecord (set #protected False dashboard))

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
                |> set #protected item.itemProtected
                |> createRecord
        Just row ->
            row
                |> set #config item.config
                |> set #position item.position
                |> set #isDefault item.isDefault
                |> set #protected item.itemProtected
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
    unless strict do
        let keepNames = map (.jiraConfigName) items
        unprotectRows (query @JiraConfig |> fetch) (\row -> row.name `notElem` keepNames) updateRecord
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
                |> set #protected item.jiraProtected
                |> createRecord
        Just row ->
            row
                |> set #baseUrl item.jiraBaseUrl
                |> set #tokenEnv item.jiraTokenEnv
                |> set #apiVersion item.jiraApiVersion
                |> set #projects projectsJson
                |> set #enabled item.jiraEnabled
                |> set #protected item.jiraProtected
                |> set #updatedAt now
                |> updateRecord
    pure ()

applyCmdbConfigs :: (?modelContext :: ModelContext) => Bool -> Maybe [CmdbConfigItem] -> IO ()
applyCmdbConfigs _ Nothing = pure ()
applyCmdbConfigs strict (Just items) = withProvisionLock "cmdbConfigs" do
    forM_ items upsertCmdbConfig
    unless strict do
        let keepNames = map (.cmdbConfigName) items
        unprotectRows (query @CmdbConfig |> fetch) (\row -> row.name `notElem` keepNames) updateRecord
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
                |> set #protected item.cmdbProtected
                |> createRecord
        Just row ->
            row
                |> set #baseUrl item.cmdbBaseUrl
                |> set #tokenEnv item.cmdbTokenEnv
                |> set #spaces spacesJson
                |> set #enabled item.cmdbEnabled
                |> set #protected item.cmdbProtected
                |> set #updatedAt now
                |> updateRecord
    pure ()

-- Assets info sources (milestone 8 §8). tokenEnv resolves like jira/cmdb.
-- Strict delete clears the config's cache rows first (assets_objects,
-- asset_alert_links, assets_icon_cache all FK to the config).

applyAssetsConfigs :: (?modelContext :: ModelContext) => Bool -> Maybe [AssetsConfigItem] -> IO ()
applyAssetsConfigs _ Nothing = pure ()
applyAssetsConfigs strict (Just items) = withProvisionLock "assetsConfigs" do
    forM_ items upsertAssetsConfig
    unless strict do
        let keepNames = map (.acName) items
        unprotectRows (query @AssetsConfig |> fetch) (\row -> row.name `notElem` keepNames) updateRecord
    when strict do
        let keepNames = map (.acName) items
        allConfigs <- query @AssetsConfig |> fetch
        forM_ (filter (\row -> row.name `notElem` keepNames) allConfigs) \config -> do
            let configId = get #id config
            void $ sqlExecTyped [typedSql| DELETE FROM asset_alert_links WHERE assets_object_id IN (SELECT id FROM assets_objects WHERE config_id = ${configId}) |]
            void $ sqlExecTyped [typedSql| DELETE FROM assets_objects WHERE config_id = ${configId} |]
            void $ sqlExecTyped [typedSql| DELETE FROM assets_icon_cache WHERE config_id = ${configId} |]
            deleteRecord config

upsertAssetsConfig :: (?modelContext :: ModelContext) => AssetsConfigItem -> IO ()
upsertAssetsConfig item = do
    validateEnvRef "assetsConfigs" item.acName item.acTokenEnv
    maybeRow <- query @AssetsConfig |> filterWhere (#name, item.acName) |> fetchOneOrNothing
    now <- getCurrentTime
    _ <- case maybeRow of
        Nothing ->
            newRecord @AssetsConfig
                |> set #name item.acName
                |> set #baseUrl item.acBaseUrl
                |> set #tokenEnv item.acTokenEnv
                |> set #authMode item.acAuthMode
                |> set #jiraEmailEnv item.acJiraEmailEnv
                |> set #defaultSchemaName item.acDefaultSchemaName
                |> set #hostQueryTemplate item.acHostQueryTemplate
                |> set #attributeNames item.acAttributeNames
                |> set #enabled item.acEnabled
                |> set #protected item.acProtected
                |> createRecord
        Just row ->
            row
                |> set #baseUrl item.acBaseUrl
                |> set #tokenEnv item.acTokenEnv
                |> set #authMode item.acAuthMode
                |> set #jiraEmailEnv item.acJiraEmailEnv
                |> set #defaultSchemaName item.acDefaultSchemaName
                |> set #hostQueryTemplate item.acHostQueryTemplate
                |> set #attributeNames item.acAttributeNames
                |> set #enabled item.acEnabled
                |> set #protected item.acProtected
                |> set #updatedAt now
                |> updateRecord
    pure ()

-- Grouping rules (milestone 2 §4). Version bumps ONLY when the content
-- actually changed, so idempotent re-provisioning doesn't invalidate
-- existing groups every boot.

applyGroupingRules :: (?modelContext :: ModelContext) => Bool -> Maybe [GroupingRuleItem] -> IO ()
applyGroupingRules _ Nothing = pure ()
applyGroupingRules strict (Just items) = withProvisionLock "groupingRules" do
    forM_ items upsertGroupingRule
    unless strict (unprotectGroupingRules items)
    when strict do
        let keepNames = map (.grName) items
        allRules <- query @GroupingRule |> fetch
        -- alert_groups holds no FK to grouping_rules: deletes are safe.
        forM_ (filter (\rule -> rule.name `notElem` keepNames) allRules) deleteRecord

unprotectGroupingRules :: (?modelContext :: ModelContext) => [GroupingRuleItem] -> IO ()
unprotectGroupingRules items =
    let keepNames = map (.grName) items
     in unprotectRows (query @GroupingRule |> fetch) (\rule -> rule.name `notElem` keepNames) updateRecord

upsertGroupingRule :: (?modelContext :: ModelContext) => GroupingRuleItem -> IO ()
upsertGroupingRule item = do
    maybeRow <- query @GroupingRule |> filterWhere (#name, item.grName) |> fetchOneOrNothing
    case maybeRow of
        Nothing -> do
            _ <-
                newRecord @GroupingRule
                    |> set #name item.grName
                    |> set #position item.grPosition
                    |> set #enabled item.grEnabled
                    |> set #version (1 :: Int)
                    |> set #match item.grMatch
                    |> set #groupKeyTemplate item.grGroupKeyTemplate
                    |> set #protected item.grProtected
                    |> createRecord
            pure ()
        Just row -> do
            let contentChanged =
                    row.position /= item.grPosition
                        || row.enabled /= item.grEnabled
                        || row.match /= item.grMatch
                        || row.groupKeyTemplate /= item.grGroupKeyTemplate
            _ <-
                row
                    |> set #position item.grPosition
                    |> set #enabled item.grEnabled
                    |> set #match item.grMatch
                    |> set #groupKeyTemplate item.grGroupKeyTemplate
                    |> set #version (if contentChanged then row.version + 1 else row.version)
                    |> set #protected item.grProtected
                    |> updateRecord
            pure ()

-- Escalation policies (milestone 2 §6). Steps reference targets by natural
-- key (team name / user email) in the file and are resolved to the UUIDs the
-- runtime stores at apply time.

applyEscalationPolicies :: (?modelContext :: ModelContext) => Bool -> Maybe [EscalationPolicyItem] -> IO ()
applyEscalationPolicies _ Nothing = pure ()
applyEscalationPolicies strict (Just items) = withProvisionLock "escalationPolicies" do
    forM_ items upsertEscalationPolicy
    unless strict (unprotectEscalationPolicies items)
    when strict do
        let keepNames = map (.epName) items
        allPolicies <- query @EscalationPolicy |> fetch
        forM_ (filter (\policy -> policy.name `notElem` keepNames) allPolicies) \policy -> do
            let policyId = get #id policy
            void $ sqlExecTyped [typedSql| UPDATE notification_rules SET escalation_policy_id = NULL WHERE escalation_policy_id = ${policyId} |]
            void $ sqlExecTyped [typedSql| DELETE FROM escalation_trackers WHERE policy_id = ${policyId} |]
            deleteRecord policy

unprotectEscalationPolicies :: (?modelContext :: ModelContext) => [EscalationPolicyItem] -> IO ()
unprotectEscalationPolicies items =
    let keepNames = map (.epName) items
     in unprotectRows (query @EscalationPolicy |> fetch) (\policy -> policy.name `notElem` keepNames) updateRecord

upsertEscalationPolicy :: (?modelContext :: ModelContext) => EscalationPolicyItem -> IO ()
upsertEscalationPolicy item = do
    steps <- resolveSteps item.epName item.epSteps
    maybeRow <- query @EscalationPolicy |> filterWhere (#name, item.epName) |> fetchOneOrNothing
    case maybeRow of
        Nothing -> do
            _ <-
                newRecord @EscalationPolicy
                    |> set #name item.epName
                    |> set #steps steps
                    |> set #protected item.epProtected
                    |> createRecord
            pure ()
        Just row -> do
            _ <-
                row
                    |> set #steps steps
                    |> set #protected item.epProtected
                    |> updateRecord
            pure ()

resolveSteps :: (?modelContext :: ModelContext) => Text -> [EscalationStepItem] -> IO Value
resolveSteps policyName steps = do
    resolved <- forM steps \step -> do
        teamId <- forM step.esTargetTeam \teamName -> do
            maybeTeam <- query @Team |> filterWhere (#name, teamName) |> fetchOneOrNothing
            case maybeTeam of
                Just team -> pure (tshow (get #id team))
                Nothing -> throwIO $ ProvisionError ("escalationPolicies." <> policyName <> ": targetTeam \"" <> teamName <> "\" does not resolve to any team")
        userId <- forM step.esTargetUser \userEmail -> do
            maybeUser <- query @User |> filterWhere (#email, userEmail) |> fetchOneOrNothing
            case maybeUser of
                Just user -> pure (tshow (get #id user))
                Nothing -> throwIO $ ProvisionError ("escalationPolicies." <> policyName <> ": targetUser \"" <> userEmail <> "\" does not resolve to any user")
        pure $
            Aeson.object
                [ "after_seconds" .= step.esAfterSeconds
                , "target_team_id" .= teamId
                , "target_user_id" .= userId
                , "unless_status" .= step.esUnlessStatus
                ]
    pure (Aeson.toJSON resolved)

-- Notification rules (milestone 2 §5). Target: team name or user email
-- (XOR); escalationPolicy by name. Applied after teams/escalationPolicies so
-- the references resolve.

applyNotificationRules :: (?modelContext :: ModelContext) => Bool -> Maybe [NotificationRuleItem] -> IO ()
applyNotificationRules _ Nothing = pure ()
applyNotificationRules strict (Just items) = withProvisionLock "notificationRules" do
    forM_ items upsertNotificationRule
    unless strict (unprotectNotificationRules items)
    when strict do
        let keepNames = map (.nrName) items
        allRules <- query @NotificationRule |> fetch
        forM_ (filter (\rule -> rule.name `notElem` keepNames) allRules) \rule -> do
            let ruleId = get #id rule
            void $ sqlExecTyped [typedSql| DELETE FROM escalation_trackers WHERE rule_id = ${ruleId} |]
            void $ sqlExecTyped [typedSql| DELETE FROM push_notification_jobs WHERE rule_id = ${ruleId} |]
            deleteRecord rule

unprotectNotificationRules :: (?modelContext :: ModelContext) => [NotificationRuleItem] -> IO ()
unprotectNotificationRules items =
    let keepNames = map (.nrName) items
     in unprotectRows (query @NotificationRule |> fetch) (\rule -> rule.name `notElem` keepNames) updateRecord

upsertNotificationRule :: (?modelContext :: ModelContext) => NotificationRuleItem -> IO ()
upsertNotificationRule item = do
    teamId <- forM item.nrTeam \teamName -> do
        maybeTeam <- query @Team |> filterWhere (#name, teamName) |> fetchOneOrNothing
        case maybeTeam of
            Just team -> pure (get #id team)
            Nothing -> throwIO $ ProvisionError ("notificationRules." <> item.nrName <> ": team \"" <> teamName <> "\" does not resolve to any team")
    userId <- forM item.nrUser \userEmail -> do
        maybeUser <- query @User |> filterWhere (#email, userEmail) |> fetchOneOrNothing
        case maybeUser of
            Just user -> pure (get #id user)
            Nothing -> throwIO $ ProvisionError ("notificationRules." <> item.nrName <> ": user \"" <> userEmail <> "\" does not resolve to any user")
    policyId <- forM item.nrEscalationPolicy \policyName -> do
        maybePolicy <- query @EscalationPolicy |> filterWhere (#name, policyName) |> fetchOneOrNothing
        case maybePolicy of
            Just policy -> pure (get #id policy)
            Nothing -> throwIO $ ProvisionError ("notificationRules." <> item.nrName <> ": escalationPolicy \"" <> policyName <> "\" does not resolve to any policy")
    maybeRow <- query @NotificationRule |> filterWhere (#name, item.nrName) |> fetchOneOrNothing
    case maybeRow of
        Nothing -> do
            _ <-
                newRecord @NotificationRule
                    |> set #name item.nrName
                    |> set #position item.nrPosition
                    |> set #enabled item.nrEnabled
                    |> set #match item.nrMatch
                    |> set #severityThreshold item.nrSeverityThreshold
                    |> set #teamId teamId
                    |> set #userId userId
                    |> set #channel item.nrChannel
                    |> set #channelConfig item.nrChannelConfig
                    |> set #throttleSeconds item.nrThrottleSeconds
                    |> set #escalationPolicyId policyId
                    |> set #protected item.nrProtected
                    |> createRecord
            pure ()
        Just row -> do
            _ <-
                row
                    |> set #position item.nrPosition
                    |> set #enabled item.nrEnabled
                    |> set #match item.nrMatch
                    |> set #severityThreshold item.nrSeverityThreshold
                    |> set #teamId teamId
                    |> set #userId userId
                    |> set #channel item.nrChannel
                    |> set #channelConfig item.nrChannelConfig
                    |> set #throttleSeconds item.nrThrottleSeconds
                    |> set #escalationPolicyId policyId
                    |> set #protected item.nrProtected
                    |> updateRecord
            pure ()

-- LLM agent roles (milestone 8 §7). isDefault mirrors SetDefaultLlmRole:
-- exactly one default, and a disabled role never stays default.

applyLlmAgentRoles :: (?modelContext :: ModelContext) => Bool -> Maybe [LlmAgentRoleItem] -> IO ()
applyLlmAgentRoles _ Nothing = pure ()
applyLlmAgentRoles strict (Just items) = withProvisionLock "llmAgentRoles" do
    forM_ items upsertLlmAgentRole
    unless strict (unprotectLlmAgentRoles items)
    when strict do
        let keepNames = map (.arName) items
        allRoles <- query @LlmAgentRole |> fetch
        forM_ (filter (\role -> role.name `notElem` keepNames) allRoles) \role -> do
            references <-
                query @LlmAnalysis
                    |> filterWhere (#agentRoleId, Just (get #id role))
                    |> fetchCount
            -- Runtime analyses reference the role: leave the row (it is
            -- unprotected by the pass above) instead of aborting the boot.
            when (references == 0) (deleteRecord role)

unprotectLlmAgentRoles :: (?modelContext :: ModelContext) => [LlmAgentRoleItem] -> IO ()
unprotectLlmAgentRoles items =
    let keepNames = map (.arName) items
     in unprotectRows (query @LlmAgentRole |> fetch) (\role -> role.name `notElem` keepNames) updateRecord

upsertLlmAgentRole :: (?modelContext :: ModelContext) => LlmAgentRoleItem -> IO ()
upsertLlmAgentRole item = do
    maybeRow <- query @LlmAgentRole |> filterWhere (#name, item.arName) |> fetchOneOrNothing
    now <- getCurrentTime
    let enabled = item.arEnabled
        isDefault = item.arIsDefault && item.arEnabled
    -- Single-default is enforced by a partial unique index, so the other
    -- rows must be cleared BEFORE this row claims the flag (the whole
    -- section runs in one transaction via withProvisionLock).
    when isDefault do
        let roleName = item.arName
        void $
            sqlExecTyped
                [typedSql|
            UPDATE llm_agent_roles SET is_default = false, updated_at = NOW()
            WHERE name <> ${roleName}
        |]
    case maybeRow of
        Nothing ->
            void $
                createRecord
                    ( newRecord @LlmAgentRole
                        |> set #name item.arName
                        |> set #description item.arDescription
                        |> set #promptTemplateName item.arPromptTemplateName
                        |> set #tools (Aeson.toJSON item.arTools)
                        |> set #enabled enabled
                        |> set #isDefault isDefault
                        |> set #protected item.arProtected
                    )
        Just existing ->
            void $
                updateRecord
                    ( existing
                        |> set #description item.arDescription
                        |> set #promptTemplateName item.arPromptTemplateName
                        |> set #tools (Aeson.toJSON item.arTools)
                        |> set #enabled enabled
                        |> set #isDefault (isDefault || (existing.isDefault && enabled))
                        |> set #protected item.arProtected
                        |> set #updatedAt now
                    )
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
