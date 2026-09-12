module Test.Integration.ProvisioningSpec (spec) where

import Control.Exception (SomeException, finally, try)
import Control.Monad (replicateM_, void)
import Data.Aeson (object)
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseMaybe)
import Data.Int (Int64)
import qualified Data.Text as Text
import Data.UUID.V4 (nextRandom)
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.FrameworkConfig (FrameworkConfig, buildFrameworkConfig)
import IHP.Job.Types (Job (..))
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
import qualified Network.Wreq as Wreq
import System.Environment (getEnv, lookupEnv, setEnv, unsetEnv)
import System.Process (callProcess, readProcess)
import Test.Hspec

import qualified Application.Connector.Grafana as Grafana
import Application.Helper.DashboardConfig (DashboardCard (..), FacetRef (..), MatchClause (..), MatchOp (..), decodeDashboardConfig)
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest)
import Application.Job.AutoClose (autoCloseResolved, closeStalledAlerts, stallStaleAlerts, unackExpiredAcks, unsuppressExpired)
import Application.Job.EnrichAlert ()
import Application.Job.Escalation (runDueTrackers)
import Application.Job.FacetBackfill ()
import Application.Job.LlmAnalysis ()
import Application.Job.PollZabbix ()
import Application.Job.Retention ()
import Application.Job.SourceHealth (checkSilence)
import Application.Pipeline.Actions (ackAlert, closeAlert, unackAlert)
import Application.Pipeline.Grouping (AlertField (..), facetValue)
import Application.Service.AlertList (AlertListFilters (..), defaultAlertListFilters, effectiveEnvNames, listAlerts)
import Application.Service.Api.Alerts (AlertDetail (..), AlertFilters (..), alertDetail, defaultFilters, listAlertsPage)
import Application.Service.Api.Auth (AuthDecision (..), authorizeToken)
import Application.Service.Api.Cursor (decodeCursor)
import Application.Service.Api.Metrics (collectMetrics)
import Application.Service.Api.Token (hashToken, newApiToken, resolveToken)
import Application.Service.Assets.Attrs (objectAttributes)
import Application.Service.DashboardCards (CardGroup (..), CardSummary (..), ExpandedCard (..), expandDashboardCards, runCardQuery, runCardQueryGroups, runCardSummary)
import Application.Service.Jira.DbConfig (syncOpenLinks)
import Application.Service.Llm (LlmProviderConfig (..), ToolCall (..))
import Application.Service.Llm.DbConfig (currentLlmConfig)
import Application.Service.Llm.ToolCache (cachedToolCall)
import Application.Service.Llm.Tools (executeToolCall)
import Application.Service.Notify (currentOnCall, resolveRuleTargets)
import Application.Service.PollerControl (ensurePollerForSourceType)
import Application.Service.Provision (ProvisionError (..), applyProvisionConfig)
import Application.Service.Reconcile (lastAckWasExternal, mirrorExternalAck, mirrorExternalUnack)
import Application.Service.SourceHealth (healthFingerprint, reconcileFingerprint, recordFailure, recordReconcileFailure, recordReconcileSuccess, recordSuccess)
import Application.Service.WriteBack (executeAttempt)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (status401, status403)
import Test.Integration.Setup
import Web.View.Dashboard.Index (EnvCard (..), computeEnvCards)

-- Milestone 7: declarative provisioning (design_docs/milestone_7.md §9).
-- Keep-lists for strict tests are built from current DB rows so the specs are
-- order-independent and safe against a populated dev DB.
m7Spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
m7Spec = describe "provisioning (milestone 7)" do
    it "applies a full config idempotently (users, sources, teams, llm)" do
        suffix <- tshow <$> nextRandom
        setEnv "M7_TEST_HOOK_TOKEN" ("tok-" <> cs suffix)
        let email = "m7-" <> suffix <> "@dev"
            sourceName = "m7-src-" <> suffix
            teamName = "m7-team-" <> suffix
            provider = "m7-llm-" <> suffix
            templateName = "m7_tmpl_" <> Text.replace "-" "_" suffix
            token = "tok-" <> suffix
            config =
                object
                    [ "users"
                        .= object
                            [ "items"
                                .= [ object
                                        [ "email" .= email
                                        , "passwordHash" .= ("sha256|17|a|b" :: Text)
                                        , "displayName" .= ("M7 " <> suffix)
                                        , "roles" .= (["m7-role-" <> suffix] :: [Text])
                                        , "settings" .= object ["theme" .= ("latte" :: Text)]
                                        ]
                                   ]
                            ]
                    , "sources"
                        .= object
                            [ "items"
                                .= [ object
                                        [ "type" .= ("webhook" :: Text)
                                        , "name" .= sourceName
                                        , "enabled" .= False
                                        , "webhookTokens" .= [object ["tokenEnv" .= ("M7_TEST_HOOK_TOKEN" :: Text)]]
                                        ]
                                   ]
                            ]
                    , "teams"
                        .= object
                            [ "items"
                                .= [ object
                                        [ "name" .= teamName
                                        , "description" .= ("m7 team " <> suffix)
                                        , "hostGroups" .= (["Linux servers"] :: [Text])
                                        , "members" .= [object ["email" .= email, "role" .= ("lead" :: Text)]]
                                        ]
                                   ]
                            ]
                    , "llm"
                        .= object
                            [ "items"
                                .= [ object
                                        [ "providerName" .= provider
                                        , "endpoint" .= ("http://m7.example" :: Text)
                                        , "model" .= ("m7-model" :: Text)
                                        , "enabled" .= False
                                        , "promptTemplates"
                                            .= [ object
                                                    [ "name" .= templateName
                                                    , "version" .= (1 :: Int)
                                                    , "body" .= ("body one" :: Text)
                                                    , "active" .= True
                                                    ]
                                               ]
                                        ]
                                   ]
                            ]
                    ]
        m7Apply config
        m7Apply config
        users <- query @User |> filterWhere (#email, email) |> fetch
        length users `shouldBe` 1
        user <- case users of
            [user] -> pure user
            _ -> expectationFailure "expected exactly one provisioned user" >> error "unreachable"
        user.displayName `shouldBe` "M7 " <> suffix
        roles <-
            sqlQueryTyped
                [typedSql|
            SELECT r.name FROM user_roles ur JOIN roles r ON r.id = ur.role_id
            JOIN users u ON u.id = ur.user_id WHERE u.email = ${email}
        |]
        roles `shouldBe` ["m7-role-" <> suffix]
        sources <- query @Source |> filterWhere (#name, sourceName) |> fetch
        source <- case sources of
            [source] -> pure source
            _ -> expectationFailure "expected exactly one provisioned source" >> error "unreachable"
        hookToken <- query @WebhookToken |> filterWhere (#token, token) |> fetchOneOrNothing
        fmap (get #sourceId) hookToken `shouldBe` Just (get #id source)
        teams <- query @Team |> filterWhere (#name, teamName) |> fetch
        team <- case teams of
            [team] -> pure team
            _ -> expectationFailure "expected exactly one provisioned team" >> error "unreachable"
        members <- query @TeamMember |> filterWhere (#teamId, get #id team) |> fetch
        map (get #teamRole) members `shouldBe` ["lead"]
        llmConfigs <- query @LlmConfig |> filterWhere (#providerName, provider) |> fetch
        length llmConfigs `shouldBe` 1
        templates <- query @LlmPromptTemplate |> filterWhere (#name, templateName) |> fetch
        map (\t -> (t.version, t.active)) templates `shouldBe` [(1, True)]

    it "re-applies changed password_hash, enabled and member role in place" do
        suffix <- tshow <$> nextRandom
        let email = "m7-" <> suffix <> "@dev"
            sourceName = "m7-src-" <> suffix
            teamName = "m7-team-" <> suffix
            config hash enabled role =
                object
                    [ "users" .= object ["items" .= [object ["email" .= email, "passwordHash" .= hash]]]
                    , "sources" .= object ["items" .= [object ["type" .= ("webhook" :: Text), "name" .= sourceName, "enabled" .= enabled]]]
                    , "teams" .= object ["items" .= [object ["name" .= teamName, "members" .= [object ["email" .= email, "role" .= role]]]]]
                    ]
        m7Apply (config ("hash-one" :: Text) False ("member" :: Text))
        m7Apply (config ("hash-two" :: Text) True ("lead" :: Text))
        user <- query @User |> filterWhere (#email, email) |> fetchOneOrNothing >>= maybe (error "user missing") pure
        user.passwordHash `shouldBe` "hash-two"
        source <- query @Source |> filterWhere (#name, sourceName) |> fetchOneOrNothing >>= maybe (error "source missing") pure
        source.enabled `shouldBe` True
        team <- query @Team |> filterWhere (#name, teamName) |> fetchOneOrNothing >>= maybe (error "team missing") pure
        members <- query @TeamMember |> filterWhere (#teamId, get #id team) |> fetch
        map (get #teamRole) members `shouldBe` ["lead"]

    it "merges user settings instead of replacing them" do
        suffix <- tshow <$> nextRandom
        let email = "m7-" <> suffix <> "@dev"
            config =
                object
                    [ "users"
                        .= object
                            [ "items"
                                .= [ object
                                        ["email" .= email, "passwordHash" .= ("x" :: Text), "settings" .= object ["theme" .= ("frappe" :: Text)]]
                                   ]
                            ]
                    ]
        m7Apply config
        let patch = object ["ui_note" .= ("kept" :: Text)]
        void $ sqlExecTyped [typedSql| UPDATE users SET settings = settings || ${patch} WHERE email = ${email} |]
        m7Apply config
        user <- query @User |> filterWhere (#email, email) |> fetchOneOrNothing >>= maybe (error "user missing") pure
        payloadText "theme" user.settings `shouldBe` Just "frappe"
        payloadText "ui_note" user.settings `shouldBe` Just "kept"

    it "re-provision without team keys preserves UI-set host_groups, description and defaults" do
        suffix <- tshow <$> nextRandom
        let teamName = "m7-team-" <> suffix
            uiGroups = Aeson.toJSON (["UI group"] :: [Text])
            uiDescription = "ui-edited " <> suffix :: Text
        m7Apply
            ( object
                [ "teams"
                    .= object
                        [ "items"
                            .= [ object
                                    [ "name" .= teamName
                                    , "description" .= ("original" :: Text)
                                    , "hostGroups" .= (["Linux servers"] :: [Text])
                                    , "defaults" .= object ["k" .= ("v" :: Text)]
                                    ]
                               ]
                        ]
                ]
            )
        -- Simulate UI edits on top of the provisioned values.
        void $
            sqlExecTyped
                [typedSql|
            UPDATE teams SET host_groups = ${uiGroups}, description = ${uiDescription}
            WHERE name = ${teamName}
        |]
        m7Apply (object ["teams" .= object ["items" .= [object ["name" .= teamName]]]])
        team <- query @Team |> filterWhere (#name, teamName) |> fetchOneOrNothing >>= maybe (error "team missing") pure
        get #description team `shouldBe` uiDescription
        get #hostGroups team `shouldBe` uiGroups
        payloadText "k" (get #defaults team) `shouldBe` Just "v"
        -- An explicit empty list still clears the groups.
        m7Apply
            ( object
                [ "teams"
                    .= object
                        [ "items"
                            .= [ object
                                    ["name" .= teamName, "hostGroups" .= ([] :: [Text])]
                               ]
                        ]
                ]
            )
        cleared <- query @Team |> filterWhere (#name, teamName) |> fetchOneOrNothing >>= maybe (error "team missing") pure
        get #hostGroups cleared `shouldBe` Aeson.toJSON ([] :: [Text])

    it "aborts on an unresolvable team member email" do
        suffix <- tshow <$> nextRandom
        m7Apply
            ( object
                [ "teams"
                    .= object
                        [ "items"
                            .= [ object
                                    ["name" .= ("m7-team-" <> suffix), "members" .= [object ["email" .= ("m7-missing-" <> suffix <> "@dev")]]]
                               ]
                        ]
                ]
            )
            `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf "does not resolve to any user" msg

    it "aborts on an unset tokenEnv reference" do
        suffix <- tshow <$> nextRandom
        unsetEnv "M7_MISSING_TOKEN"
        m7Apply
            ( object
                [ "sources"
                    .= object
                        [ "items"
                            .= [ object
                                    [ "type" .= ("zabbix" :: Text)
                                    , "name" .= ("m7-src-" <> suffix)
                                    , "config" .= object ["tokenEnv" .= ("M7_MISSING_TOKEN" :: Text)]
                                    ]
                               ]
                        ]
                ]
            )
            `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf "M7_MISSING_TOKEN" msg

    it "imports zabbix host groups from a local file, replacing the cache" do
        suffix <- tshow <$> nextRandom
        let sourceName = "m7-zbx-" <> suffix
            groupsPath :: Text
            groupsPath = "/tmp/halemans-m7-groups-" <> cs suffix <> ".json"
            config =
                object
                    [ "sources"
                        .= object
                            [ "items"
                                .= [ object
                                        [ "type" .= ("zabbix" :: Text)
                                        , "name" .= sourceName
                                        , "hostGroupsFile" .= groupsPath
                                        ]
                                   ]
                            ]
                    ]
        LBS.writeFile
            (cs groupsPath)
            ( Aeson.encode
                [ object ["groupid" .= ("2" :: Text), "name" .= ("Linux servers" :: Text)]
                , object ["groupid" .= ("5" :: Text), "name" .= ("Databases" :: Text)]
                ]
            )
        m7Apply config
        source <- query @Source |> filterWhere (#name, sourceName) |> fetchOneOrNothing >>= maybe (error "source missing") pure
        rows <- query @ZabbixHostGroup |> filterWhere (#sourceId, get #id source) |> orderByAsc #name |> fetch
        map (\g -> (g.groupId, g.name)) rows `shouldBe` [("5", "Databases"), ("2", "Linux servers")]
        -- A hostgroup.get response dump works verbatim and re-apply replaces.
        LBS.writeFile
            (cs groupsPath)
            ( Aeson.encode $
                object
                    [ "jsonrpc" .= ("2.0" :: Text)
                    , "result" .= [object ["groupid" .= ("7" :: Text), "name" .= ("Hypervisors" :: Text)]]
                    , "id" .= (1 :: Int)
                    ]
            )
        m7Apply config
        rows' <- query @ZabbixHostGroup |> filterWhere (#sourceId, get #id source) |> fetch
        map (\g -> (g.groupId, g.name)) rows' `shouldBe` [("7", "Hypervisors")]

    it "aborts when hostGroupsFile is unreadable" do
        suffix <- tshow <$> nextRandom
        m7Apply
            ( object
                [ "sources"
                    .= object
                        [ "items"
                            .= [ object
                                    [ "type" .= ("zabbix" :: Text)
                                    , "name" .= ("m7-zbx-" <> suffix)
                                    , "hostGroupsFile" .= ("/tmp/halemans-m7-no-such-" <> suffix)
                                    ]
                               ]
                        ]
                ]
            )
            `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf "cannot read hostGroupsFile" msg

    it "currentLlmConfig prefers the enabled DB row, env is the fallback" do
        oldEndpoint <- lookupEnv "LLM_ENDPOINT"
        oldModel <- lookupEnv "LLM_MODEL"
        flip finally (restoreEnv "LLM_ENDPOINT" oldEndpoint >> restoreEnv "LLM_MODEL" oldModel) do
            setEnv "LLM_ENDPOINT" "http://m7-env.example"
            setEnv "LLM_MODEL" "env-model"
            void $ sqlExecTyped [typedSql| DELETE FROM llm_configs |]
            fromEnv <- currentLlmConfig
            fmap (.endpoint) fromEnv `shouldBe` Just "http://m7-env.example"
            suffix <- tshow <$> nextRandom
            let provider = "m7-llm-" <> suffix
            m7Apply
                ( object
                    [ "llm"
                        .= object
                            [ "items"
                                .= [ object
                                        [ "providerName" .= provider
                                        , "endpoint" .= ("http://m7-db.example" :: Text)
                                        , "model" .= ("m7-db-model" :: Text)
                                        , "enabled" .= True
                                        ]
                                   ]
                            ]
                    ]
                )
            fromDb <- currentLlmConfig
            fmap (.endpoint) fromDb `shouldBe` Just "http://m7-db.example"
            fmap (.providerName) fromDb `shouldBe` Just provider

    it "prompt template provisioning swaps the active version" do
        suffix <- tshow <$> nextRandom
        let provider = "m7-llm-" <> suffix
            templateName = "m7_tmpl_" <> Text.replace "-" "_" suffix
            config version =
                object
                    [ "llm"
                        .= object
                            [ "items"
                                .= [ object
                                        [ "providerName" .= provider
                                        , "endpoint" .= ("http://m7.example" :: Text)
                                        , "model" .= ("m" :: Text)
                                        , "promptTemplates"
                                            .= [ object
                                                    [ "name" .= templateName
                                                    , "version" .= version
                                                    , "body" .= ("body" :: Text)
                                                    , "active" .= True
                                                    ]
                                               ]
                                        ]
                                   ]
                            ]
                    ]
        m7Apply (config (1 :: Int))
        m7Apply (config (2 :: Int))
        m7Apply (config (2 :: Int))
        templates <-
            query @LlmPromptTemplate
                |> filterWhere (#name, templateName)
                |> orderByAsc #version
                |> fetch
        map (\t -> (t.version, t.active)) templates `shouldBe` [(1, False), (2, True)]

    it "strict teams deletes absent teams and prunes members of kept teams" do
        suffix <- tshow <$> nextRandom
        let doomedName = "m7-doomed-" <> suffix
            keepName = "m7-keep-" <> suffix
        user1 <- m7User ("m7-a-" <> suffix <> "@dev")
        user2 <- m7User ("m7-b-" <> suffix <> "@dev")
        doomed <- newRecord @Team |> set #name doomedName |> createRecord
        void $ newRecord @TeamMember |> set #teamId (get #id doomed) |> set #userId (get #id user1) |> createRecord
        keep <- newRecord @Team |> set #name keepName |> createRecord
        void $ newRecord @TeamMember |> set #teamId (get #id keep) |> set #userId (get #id user1) |> set #teamRole "lead" |> createRecord
        void $ newRecord @TeamMember |> set #teamId (get #id keep) |> set #userId (get #id user2) |> createRecord
        keepItems <- m7TeamKeepItems [doomedName, keepName]
        let keepItem = object ["name" .= keepName, "members" .= [object ["email" .= get #email user1, "role" .= ("lead" :: Text)]]]
        m7Apply (object ["teams" .= object ["strict" .= True, "items" .= (keepItems <> [keepItem])]])
        query @Team |> filterWhere (#name, doomedName) |> fetch `shouldReturn` []
        members <- query @TeamMember |> filterWhere (#teamId, get #id keep) |> fetch
        map (get #userId) members `shouldBe` [get #id user1]

    it "strict llm deletes absent providers and unreferenced template versions" do
        suffix <- tshow <$> nextRandom
        let templateName = "m7_strict_" <> Text.replace "-" "_" suffix
        keepProviders <- m7LlmKeepItems
        void $
            newRecord @LlmConfig
                |> set #providerName ("m7-doomed-llm-" <> suffix)
                |> set #endpoint "http://doomed.example"
                |> set #model "m"
                |> createRecord
        v1 <- newRecord @LlmPromptTemplate |> set #name templateName |> set #version 1 |> set #body "one" |> createRecord
        void $ newRecord @LlmPromptTemplate |> set #name templateName |> set #version 2 |> set #body "two" |> createRecord
        m7Apply
            ( object
                [ "llm"
                    .= object
                        [ "strict" .= True
                        , "items"
                            .= ( keepProviders
                                    <> [ object
                                            [ "providerName" .= ("m7-strict-llm-" <> suffix)
                                            , "endpoint" .= ("http://kept.example" :: Text)
                                            , "model" .= ("m" :: Text)
                                            , "promptTemplates" .= [object ["name" .= templateName, "version" .= (1 :: Int), "body" .= ("one" :: Text), "active" .= True]]
                                            ]
                                       ]
                               )
                        ]
                ]
            )
        query @LlmConfig |> filterWhere (#providerName, "m7-doomed-llm-" <> suffix) |> fetch `shouldReturn` []
        templates <- query @LlmPromptTemplate |> filterWhere (#name, templateName) |> fetch
        map (get #id) templates `shouldBe` [get #id v1]

    it "strict llm template delete aborts when an analysis references the version" do
        suffix <- tshow <$> nextRandom
        let templateName = "m7_fk_" <> Text.replace "-" "_" suffix
        v1 <- newRecord @LlmPromptTemplate |> set #name templateName |> set #version 1 |> set #body "one" |> createRecord
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        void $
            newRecord @LlmAnalysis
                |> set #alertId alertId
                |> set #promptTemplateId (Just (get #id v1))
                |> set #promptHash fp
                |> createRecord
        keepProviders <- m7LlmKeepItems
        m7Apply
            ( object
                [ "llm"
                    .= object
                        [ "strict" .= True
                        , "items"
                            .= ( keepProviders
                                    <> [ object
                                            [ "providerName" .= ("m7-strict-llm-" <> suffix)
                                            , "endpoint" .= ("http://kept.example" :: Text)
                                            , "model" .= ("m" :: Text)
                                            , "promptTemplates" .= [object ["name" .= templateName, "version" .= (2 :: Int), "body" .= ("two" :: Text)]]
                                            ]
                                       ]
                               )
                        ]
                ]
            )
            `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf "cannot delete prompt template" msg

    it "strict users deletes unreferenced users and aborts on alert-history references" do
        suffix <- tshow <$> nextRandom
        doomedPlain <- m7User ("m7-doomed-" <> suffix <> "@dev")
        doomedReferenced <- m7User ("m7-fk-" <> suffix <> "@dev")
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        void $
            newRecord @AlertEvent
                |> set #alertId alertId
                |> set #userId (Just (get #id doomedReferenced))
                |> set #kind "external"
                |> createRecord
        -- Transactional: the FK-blocked delete rolls the whole category back,
        -- so even the unreferenced doomed user survives this apply.
        keepWithoutReferenced <- m7UserKeepItems [get #email doomedReferenced]
        m7Apply (object ["users" .= object ["strict" .= True, "items" .= keepWithoutReferenced]])
            `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf ("cannot delete user \"m7-fk-" <> suffix <> "@dev\"") msg
        surviving <- query @User |> filterWhere (#email, get #email doomedPlain) |> fetch
        map (get #id) surviving `shouldBe` [get #id doomedPlain]
        -- Without the referenced user in scope the plain one is deleted.
        keepWithoutPlain <- m7UserKeepItems [get #email doomedPlain]
        m7Apply (object ["users" .= object ["strict" .= True, "items" .= keepWithoutPlain]])
        query @User |> filterWhere (#email, get #email doomedPlain) |> fetch `shouldReturn` []
        kept <- query @User |> filterWhere (#email, get #email doomedReferenced) |> fetch
        map (get #id) kept `shouldBe` [get #id doomedReferenced]

    it "strict sources delete aborts when alerts reference the source" do
        suffix <- tshow <$> nextRandom
        doomed <- integrationSource "webhook" ("m7-doomed-src-" <> suffix) "" (object [])
        fp <- freshFingerprint
        Just _ <- ingest doomed (testEvent fp Firing)
        -- The keep-list replays existing sources whose config tokenEnv refs
        -- (fixture zabbix/grafana) must resolve at apply time (§5).
        oldZabbix <- lookupEnv "ZABBIX_TOKEN"
        oldGrafana <- lookupEnv "GRAFANA_TOKEN"
        flip finally (restoreEnv "ZABBIX_TOKEN" oldZabbix >> restoreEnv "GRAFANA_TOKEN" oldGrafana) do
            setEnv "ZABBIX_TOKEN" "m7-dummy-zabbix"
            setEnv "GRAFANA_TOKEN" "m7-dummy-grafana"
            keepItems <- m7SourceKeepItems [get #name doomed]
            m7Apply (object ["sources" .= object ["strict" .= True, "items" .= keepItems]])
                `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf ("cannot delete source \"m7-doomed-src-" <> suffix <> "\"") msg

    it "provisions field mappings and dashboards idempotently" do
        suffix <- tshow <$> nextRandom
        let email = "m7-" <> suffix <> "@dev"
            facet = "m7facet-" <> suffix
            dashName = "m7-dash-" <> suffix
            config enabled =
                object
                    [ "users" .= object ["items" .= [object ["email" .= email, "passwordHash" .= ("x" :: Text)]]]
                    , "fieldMappings"
                        .= object
                            [ "items"
                                .= [ object
                                        [ "facet" .= facet
                                        , "rank" .= (42 :: Int)
                                        , "kind" .= ("field" :: Text)
                                        , "key" .= ("env" :: Text)
                                        , "enabled" .= enabled
                                        ]
                                   ]
                            ]
                    , "dashboards"
                        .= object
                            [ "items"
                                .= [ object
                                        [ "name" .= dashName
                                        , "userEmail" .= email
                                        , "isDefault" .= True
                                        , "config"
                                            .= [ object
                                                    [ "title" .= ("probe" :: Text)
                                                    , "match" .= [object ["facet" .= ("field:env" :: Text), "op" .= ("=" :: Text), "value" .= ("prod" :: Text)]]
                                                    , "groupBy" .= ("field:host" :: Text)
                                                    ]
                                               ]
                                        ]
                                   ]
                            ]
                    ]
        m7Apply (config False)
        m7Apply (config True)
        mappings <- query @FieldMapping |> filterWhere (#facet, facet) |> fetch
        map (\mapping -> (mapping.rank, mapping.enabled)) mappings `shouldBe` [(42, True)]
        dashboards <- query @Dashboard |> filterWhere (#name, dashName) |> fetch
        map (.isDefault) dashboards `shouldBe` [True]

    it "dashboard provisioning rejects an unresolvable userEmail" do
        suffix <- tshow <$> nextRandom
        m7Apply
            ( object
                [ "dashboards"
                    .= object
                        [ "items"
                            .= [ object
                                    ["name" .= ("m7-dash-" <> suffix), "userEmail" .= ("m7-ghost-" <> suffix <> "@dev")]
                               ]
                        ]
                ]
            )
            `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf "does not resolve to any user" msg

    it "strict fieldMappings/dashboards delete only unlisted rows" do
        suffix <- tshow <$> nextRandom
        let email = "m7-" <> suffix <> "@dev"
            doomedFacet = "m7doomed-" <> suffix
            doomedDash = "m7-doomed-dash-" <> suffix
        owner <- m7User email
        void $
            sqlExecTyped
                [typedSql|
            INSERT INTO field_mappings (facet, rank, kind, key, enabled)
            VALUES (${doomedFacet}, 7, 'field', 'env', true)
        |]
        _ <-
            newRecord @Dashboard
                |> set #userId (get #id owner)
                |> set #name doomedDash
                |> createRecord
        mappings <- query @FieldMapping |> fetch
        let keepMappings =
                [ object
                    [ "facet" .= mapping.facet
                    , "rank" .= mapping.rank
                    , "kind" .= mapping.kind
                    , "key" .= mapping.key
                    , "enabled" .= mapping.enabled
                    ]
                | mapping <- mappings
                , mapping.facet /= doomedFacet
                ]
        dashboards <- query @Dashboard |> fetch
        keepDashboards <- fmap catMaybes $ forM dashboards \dashboard -> do
            dashOwner <- fetch dashboard.userId
            pure $
                if dashboard.name == doomedDash
                    then Nothing
                    else
                        Just
                            ( object
                                [ "name" .= dashboard.name
                                , "userEmail" .= dashOwner.email
                                , "config" .= dashboard.config
                                , "position" .= dashboard.position
                                , "isDefault" .= dashboard.isDefault
                                ]
                            )
        m7Apply
            ( object
                [ "fieldMappings" .= object ["strict" .= True, "items" .= keepMappings]
                , "dashboards" .= object ["strict" .= True, "items" .= keepDashboards]
                ]
            )
        query @FieldMapping |> filterWhere (#facet, doomedFacet) |> fetch `shouldReturn` []
        query @Dashboard |> filterWhere (#name, doomedDash) |> fetch `shouldReturn` []
        remainingMappings <- query @FieldMapping |> fetch
        length remainingMappings `shouldBe` length keepMappings
        remainingDashboards <- query @Dashboard |> fetch
        length remainingDashboards `shouldBe` length keepDashboards

-- | Provisioning (m7).
spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = m7Spec
