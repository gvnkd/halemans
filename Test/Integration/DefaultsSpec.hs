module Test.Integration.DefaultsSpec (spec) where

import Control.Exception (finally)
import Control.Monad (void)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Generated.Types
import IHP.Fetch (fetch)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
import Test.Hspec

import Application.Service.Agent.Core (defaultAgentTemplateBody, internalAgentTemplateName)
import Application.Service.Defaults (ensureDefaults)
import Application.Service.Jira.Related (defaultJiraRelatedTemplateBody, relatedTemplateName)
import Application.Service.Llm.Prompt (defaultEnrichmentTemplateBody, enrichmentTemplateName)
import Application.Service.Mattermost.Render (
    defaultDetailsTemplateBody,
    defaultRootTemplateBody,
    mattermostDetailsTemplateName,
    mattermostRootTemplateName,
 )

-- Boot-time default provisioning (Application.Service.Defaults). The suite
-- shares one DB and runs in random order, so these examples are strictly
-- snapshot-compare (never wipe): pre-existing rows must come out byte-identical,
-- and only rows this spec created are removed again in `finally` — templates
-- by their 'built-in default' notes marker (FK-guarded against llm_analyses,
-- the only referencing table), channels/roles/retention by missing-at-snapshot
-- plus a user_roles FK guard on roles.

spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = describe "boot-time default provisioning" do
    it "seeds missing built-in rows from the code constants" $ withRestoredDefaults do
        snap <- takeSnapshot
        ensureDefaults
        templates <- query @LlmPromptTemplate |> fetch
        forM_ templateNames \name -> do
            let before = fromMaybe [] (lookup name (snapTemplates snap))
                now = [tplSig t | t <- templates, t.name == name]
            if null before
                then now `shouldBe` [(1, defaultBodyFor name, True, Just "built-in default")]
                else now `shouldBe` before
        channels <- query @NotificationChannel |> fetch
        let channelMap = Map.fromList [(c.name, c.type_) | c <- channels]
        forM_ defaultChannels \(name, channelType) ->
            Map.lookup name channelMap `shouldBe` Just channelType
        unless (snapRetention snap) do
            retentions <- query @RetentionConfig |> fetch
            [(r.rawEventsDays, r.enabled) | r <- retentions] `shouldBe` [(30, True)]
        roles <- query @Role |> fetch
        let roleMap = Map.fromList [(r.name, r.privileges) | r <- roles]
        forM_ defaultRoles \(name, privileges) ->
            case lookup name (snapRoles snap) of
                Just before -> Map.lookup name roleMap `shouldBe` Just before
                Nothing -> Map.lookup name roleMap `shouldBe` Just privileges

    it "is idempotent and never touches existing rows" $ withRestoredDefaults do
        snap <- takeSnapshot
        let missing = [name | name <- templateNames, null (fromMaybe [] (lookup name (snapTemplates snap)))]
            chosen = listToMaybe missing
        forM_ chosen \name -> void do
            newRecord @LlmPromptTemplate
                |> set #name name
                |> set #version 1
                |> set #body customBody
                |> set #active True
                |> createRecord
        ensureDefaults
        ensureDefaults
        templates <- query @LlmPromptTemplate |> fetch
        forM_ templateNames \name -> do
            let before = fromMaybe [] (lookup name (snapTemplates snap))
                expected =
                    if chosen == Just name
                        then before ++ [(1, customBody, True, Nothing)]
                        else
                            if null before
                                then before ++ [(1, defaultBodyFor name, True, Just "built-in default")]
                                else before
                now = [tplSig t | t <- templates, t.name == name]
            now `shouldBe` expected

customBody :: Text
customBody = "CUSTOM TEMPLATE BODY"

tplSig :: LlmPromptTemplate -> (Int, Text, Bool, Maybe Text)
tplSig t = (t.version, t.body, t.active, t.notes)

templateNames :: [Text]
templateNames =
    [ mattermostRootTemplateName
    , mattermostDetailsTemplateName
    , internalAgentTemplateName
    , relatedTemplateName
    , enrichmentTemplateName
    ]

defaultBodyFor :: Text -> Text
defaultBodyFor name = fromMaybe (error "unknown template name") (lookup name bodies)
  where
    bodies =
        [ (mattermostRootTemplateName, defaultRootTemplateBody)
        , (mattermostDetailsTemplateName, defaultDetailsTemplateBody)
        , (internalAgentTemplateName, defaultAgentTemplateBody)
        , (relatedTemplateName, defaultJiraRelatedTemplateBody)
        , (enrichmentTemplateName, defaultEnrichmentTemplateBody)
        ]

defaultChannels :: [(Text, Text)]
defaultChannels =
    [ ("browser_push", "browser_push")
    , ("email", "email")
    ]

defaultRoles :: [(Text, [Text])]
defaultRoles =
    [ ("admin", ["view", "ack", "close", "escalate", "manage_blackouts", "manage_rules", "manage_users", "manage_sources", "admin"])
    , ("sre", ["view", "ack", "close", "escalate"])
    , ("viewer", ["view"])
    ]

data Snapshot = Snapshot
    { snapTemplates :: [(Text, [(Int, Text, Bool, Maybe Text)])]
    , snapChannels :: [Text]
    , snapRoles :: [(Text, [Text])]
    , snapRetention :: Bool
    }

takeSnapshot :: (?modelContext :: ModelContext) => IO Snapshot
takeSnapshot = do
    templates <- query @LlmPromptTemplate |> fetch
    channels <- query @NotificationChannel |> fetch
    roles <- query @Role |> fetch
    [retentionExists] <- sqlQueryTyped [typedSql| SELECT EXISTS (SELECT 1 FROM retention_configs) |] :: IO [Bool]
    pure
        Snapshot
            { snapTemplates =
                [ (name, [tplSig t | t <- templates, t.name == name])
                | name <- templateNames
                ]
            , snapChannels = [c.name | c <- channels]
            , snapRoles = [(r.name, r.privileges) | r <- roles]
            , snapRetention = retentionExists
            }

withRestoredDefaults :: ((?modelContext :: ModelContext) => IO ()) -> ((?modelContext :: ModelContext, ?context :: FrameworkConfig) => IO ())
withRestoredDefaults action = do
    snap <- takeSnapshot
    action `finally` restoreDefaults snap

restoreDefaults :: (?modelContext :: ModelContext) => Snapshot -> IO ()
restoreDefaults snap = do
    void do
        sqlExecTyped
            [typedSql|
        DELETE FROM llm_prompt_templates t
        WHERE t.notes = 'built-in default'
          AND NOT EXISTS (SELECT 1 FROM llm_analyses a WHERE a.prompt_template_id = t.id)
    |]
    forM_ (map fst defaultChannels List.\\ snapChannels snap) \name -> void do
        sqlExecTyped [typedSql| DELETE FROM notification_channels WHERE name = ${name} |]
    forM_ (map fst defaultRoles List.\\ map fst (snapRoles snap)) \name -> void do
        sqlExecTyped
            [typedSql|
        DELETE FROM roles r
        WHERE r.name = ${name}
          AND NOT EXISTS (SELECT 1 FROM user_roles ur WHERE ur.role_id = r.id)
    |]
    unless (snapRetention snap) do
        void (sqlExecTyped [typedSql| DELETE FROM retention_configs |])
