module Test.ProvisionSpec where

import Application.Connector.Zabbix (ZabbixGroup (..))
import Application.Service.Provision
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import IHP.Prelude
import Test.Hspec

-- Unit coverage for the provision config parser: round-trip of valid
-- fixtures in the map-keyed format, unknown-field rejection, required-field
-- errors naming the path, global strict default, YAML/JSON equivalence.

spec :: Spec
spec = describe "Application.Service.Provision" do
    describe "parseProvisionConfig" do
        it "parses the empty config" do
            parseProvisionConfig "{}" `shouldBe` Right (ProvisionConfig False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

        it "parses a full config" do
            let json =
                    Aeson.encode $
                        object
                            [ "strict" .= True
                            , "users"
                                .= object
                                    [ Key.fromText "ops@example.com"
                                        .= object
                                            [ "displayName" .= ("Ops" :: Text)
                                            , "passwordHash" .= ("sha256|17|salt|hash" :: Text)
                                            , "roles" .= (["admin"] :: [Text])
                                            , "settings" .= object ["theme" .= ("dark" :: Text)]
                                            ]
                                    ]
                            , "sources"
                                .= object
                                    [ Key.fromText "zabbix-prod"
                                        .= object
                                            [ "type" .= ("zabbix" :: Text)
                                            , "baseUrl" .= ("https://zabbix.example" :: Text)
                                            , "env" .= ("prod" :: Text)
                                            , "pollIntervalSeconds" .= (30 :: Int)
                                            , "enabled" .= True
                                            , "config" .= object ["tokenEnv" .= ("ZABBIX_TOKEN" :: Text)]
                                            , "webhookTokens"
                                                .= object
                                                    [ Key.fromText "am-hook" .= object ["tokenEnv" .= ("HALEMANS_AM_HOOK_TOKEN" :: Text)]
                                                    ]
                                            ]
                                    ]
                            , "teams"
                                .= object
                                    [ Key.fromText "sre"
                                        .= object
                                            [ "description" .= ("Site reliability engineering" :: Text)
                                            , "hostGroups" .= (["Linux servers"] :: [Text])
                                            , "defaults" .= object []
                                            , "members"
                                                .= object
                                                    [ Key.fromText "ops@example.com" .= object ["role" .= ("lead" :: Text)]
                                                    ]
                                            , "defaultDashboardConfig" .= [object ["env" .= ("prod" :: Text)]]
                                            ]
                                    ]
                            , "llm"
                                .= object
                                    [ Key.fromText "default"
                                        .= object
                                            [ "endpoint" .= ("http://127.0.0.1:18084" :: Text)
                                            , "model" .= ("qwen" :: Text)
                                            , "apiKeyEnv" .= ("LLM_API_KEY" :: Text)
                                            , "toolsEnabled" .= False
                                            , "enabled" .= True
                                            , "promptTemplates"
                                                .= object
                                                    [ Key.fromText "alert_enrichment"
                                                        .= object
                                                            [ Key.fromText "1"
                                                                .= object
                                                                    [ "body" .= ("..." :: Text)
                                                                    , "active" .= True
                                                                    , "notes" .= ("provisioned" :: Text)
                                                                    ]
                                                            ]
                                                    ]
                                            ]
                                    ]
                            ]
            case parseProvisionConfig json of
                Left err -> expectationFailure (cs err)
                Right config -> do
                    config.strict `shouldBe` True
                    let [user] = fromMaybe [] config.users
                    user.email `shouldBe` "ops@example.com"
                    user.roles `shouldBe` ["admin"]
                    let [source] = fromMaybe [] config.sources
                    source.name `shouldBe` "zabbix-prod"
                    source.sourceType `shouldBe` "zabbix"
                    map (.tokenEnv) source.webhookTokens `shouldBe` ["HALEMANS_AM_HOOK_TOKEN"]
                    let [team] = fromMaybe [] config.teams
                    team.name `shouldBe` "sre"
                    map (.role) team.members `shouldBe` ["lead"]
                    team.hostGroups `shouldBe` Just ["Linux servers"]
                    let [llm] = fromMaybe [] config.llm
                    llm.providerName `shouldBe` "default"
                    llm.enabled `shouldBe` True
                    map (\template -> (template.name, template.version)) llm.promptTemplates `shouldBe` [("alert_enrichment", 1)]

        it "parses YAML identically to JSON" do
            let json =
                    "{\"users\": {\"a@b.c\": {\"passwordHash\": \"x\", \"roles\": [\"admin\"], \"settings\": {\"theme\": \"dark\"}}}}"
                yaml :: Text
                yaml =
                    "users:\n\
                    \  a@b.c:\n\
                    \    passwordHash: x\n\
                    \    roles:\n\
                    \      - admin\n\
                    \    settings:\n\
                    \      theme: dark\n"
            parseProvisionConfigYaml (cs yaml) `shouldBe` parseProvisionConfig json

        it "rejects invalid JSON" do
            case parseProvisionConfig "{ not json" of
                Left err -> err `shouldSatisfy` ("invalid JSON:" `isPrefixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects invalid YAML" do
            case parseProvisionConfigYaml "users:\n  a@b.c:\n    passwordHash: [unclosed" of
                Left err -> err `shouldSatisfy` ("invalid YAML:" `isPrefixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects unknown top-level keys" do
            case parseProvisionConfig "{\"userz\": {}}" of
                Left err -> err `shouldSatisfy` ("unknown field \"userz\"" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects unknown per-entity fields" do
            let json = "{\"users\": {\"a@b.c\": {\"passwordHash\": \"x\", \"paswordHash2\": \"y\"}}}"
            case parseProvisionConfig json of
                Left err -> do
                    err `shouldSatisfy` ("unknown field \"paswordHash2\"" `isInfixOf`)
                    err `shouldSatisfy` ("users" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "names the path of a missing required field" do
            let json = "{\"users\": {\"a@b.c\": {\"displayName\": \"no hash\"}}}"
            case parseProvisionConfig json of
                Left err -> do
                    err `shouldSatisfy` ("users" `isInfixOf`)
                    err `shouldSatisfy` ("passwordHash" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "defaults strict to false" do
            let json = "{\"users\": {}}"
            case parseProvisionConfig json of
                Right config -> do
                    config.strict `shouldBe` False
                    config.users `shouldBe` Just []
                Left err -> expectationFailure (cs err)

        it "parses the global strict flag" do
            let json = "{\"strict\": true, \"sources\": {}}"
            case parseProvisionConfig json of
                Right config -> do
                    config.strict `shouldBe` True
                    config.sources `shouldBe` Just []
                Left err -> expectationFailure (cs err)

        it "rejects unknown source types" do
            let json = "{\"sources\": {\"x\": {\"type\": \"prometheus\"}}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("unknown source type" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects invalid themes in user settings" do
            let json = "{\"users\": {\"a@b.c\": {\"passwordHash\": \"x\", \"settings\": {\"theme\": \"solarized\"}}}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("unknown theme" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "applies entity defaults" do
            let json = "{\"sources\": {\"hook\": {\"type\": \"webhook\"}}}"
            case parseProvisionConfig json of
                Right config -> do
                    let [source] = fromMaybe [] config.sources
                    source.name `shouldBe` "hook"
                    source.baseUrl `shouldBe` ""
                    source.env `shouldBe` "dev"
                    source.pollIntervalSeconds `shouldBe` 30
                    source.enabled `shouldBe` True
                Left err -> expectationFailure (cs err)

        it "normalizes empty member roles to member" do
            let json = "{\"teams\": {\"t\": {\"members\": {\"a@b.c\": {\"role\": \"\"}}}}}"
            case parseProvisionConfig json of
                Right config -> do
                    let [team] = fromMaybe [] config.teams
                    map (.role) team.members `shouldBe` ["member"]
                Left err -> expectationFailure (cs err)

        it "leaves absent team fields as Nothing so re-provision preserves DB values" do
            let json = "{\"teams\": {\"t\": {}}}"
            case parseProvisionConfig json of
                Right config -> do
                    let [team] = fromMaybe [] config.teams
                    team.description `shouldBe` Nothing
                    team.hostGroups `shouldBe` Nothing
                    team.defaults `shouldBe` Nothing
                Left err -> expectationFailure (cs err)

        it "parses hostGroupsFile on a zabbix source" do
            let json = "{\"sources\": {\"z\": {\"type\": \"zabbix\", \"hostGroupsFile\": \"groups.json\"}}}"
            case parseProvisionConfig json of
                Right config -> do
                    let [source] = fromMaybe [] config.sources
                    source.hostGroupsFile `shouldBe` Just "groups.json"
                Left err -> expectationFailure (cs err)

        it "rejects hostGroupsFile on non-zabbix sources" do
            let json = "{\"sources\": {\"g\": {\"type\": \"grafana\", \"hostGroupsFile\": \"groups.json\"}}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("hostGroupsFile" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "parses field mappings and dashboards" do
            let json =
                    Aeson.encode $
                        object
                            [ "fieldMappings"
                                .= object
                                    [ Key.fromText "Environments"
                                        .= object
                                            [ Key.fromText "50"
                                                .= object
                                                    [ "kind" .= ("attr" :: Text)
                                                    , "key" .= ("Environments" :: Text)
                                                    ]
                                            ]
                                    ]
                            , "dashboards"
                                .= object
                                    [ Key.fromText "ops@example.com"
                                        .= object
                                            [ Key.fromText "Service matrix"
                                                .= object
                                                    [ "position" .= (60 :: Int)
                                                    , "config"
                                                        .= [ object
                                                                [ "title" .= ("ETCD / EU / PROD" :: Text)
                                                                , "match" .= [object ["facet" .= ("attr:Service" :: Text), "op" .= ("=" :: Text), "value" .= ("ETCD" :: Text)]]
                                                                , "groupBy" .= ("field:host" :: Text)
                                                                , "limit" .= (20 :: Int)
                                                                ]
                                                           ]
                                                    ]
                                            ]
                                    ]
                            ]
            case parseProvisionConfig json of
                Left err -> expectationFailure (cs err)
                Right config -> do
                    let [mapping] = fromMaybe [] config.fieldMappings
                    mapping.facet `shouldBe` "Environments"
                    mapping.rank `shouldBe` 50
                    mapping.enabled `shouldBe` True
                    let [dashboard] = fromMaybe [] config.dashboards
                    dashboard.name `shouldBe` "Service matrix"
                    dashboard.userEmail `shouldBe` "ops@example.com"
                    dashboard.isDefault `shouldBe` False

        it "rejects non-integer field mapping rank keys" do
            let json = "{\"fieldMappings\": {\"env\": {\"first\": {\"kind\": \"field\", \"key\": \"env\"}}}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("not an integer" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects non-integer prompt template version keys" do
            let json = "{\"llm\": {\"p\": {\"endpoint\": \"http://e\", \"model\": \"m\", \"promptTemplates\": {\"t\": {\"v1\": {\"body\": \"b\"}}}}}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("not an integer" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects unknown field mapping kinds" do
            let json = "{\"fieldMappings\": {\"env\": {\"1\": {\"kind\": \"cmdb\", \"key\": \"env\"}}}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("unknown field mapping kind" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects field-kind mappings with an unknown alert field" do
            let json = "{\"fieldMappings\": {\"env\": {\"1\": {\"kind\": \"field\", \"key\": \"bogus\"}}}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("unknown alert field" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects dashboards with an undecodable config" do
            let json = "{\"dashboards\": {\"a@b.c\": {\"d\": {\"config\": [{\"match\": [{\"facet\": \"bogus:x\", \"op\": \"=\", \"value\": \"y\"}]}]}}}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("invalid config for dashboard" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "parses jira and cmdb config sections (milestone 10)" do
            let json =
                    Aeson.encode $
                        object
                            [ "jiraConfigs"
                                .= object
                                    [ Key.fromText "jira-prod"
                                        .= object
                                            [ "baseUrl" .= ("https://jira.example" :: Text)
                                            , "tokenEnv" .= ("JIRA_TOKEN" :: Text)
                                            , "projects" .= (["OPS", "SRE"] :: [Text])
                                            ]
                                    ]
                            , "cmdbConfigs"
                                .= object
                                    [ Key.fromText "confluence-prod"
                                        .= object
                                            [ "baseUrl" .= ("https://confluence.example" :: Text)
                                            , "tokenEnv" .= ("CONFLUENCE_TOKEN" :: Text)
                                            ]
                                    ]
                            ]
            case parseProvisionConfig json of
                Left err -> expectationFailure (cs err)
                Right config -> do
                    let [jira] = fromMaybe [] config.jiraConfigs
                    jira.jiraConfigName `shouldBe` "jira-prod"
                    jira.jiraProjects `shouldBe` ["OPS", "SRE"]
                    jira.jiraApiVersion `shouldBe` "3"
                    jira.jiraEnabled `shouldBe` True
                    let [cmdb] = fromMaybe [] config.cmdbConfigs
                    cmdb.cmdbConfigName `shouldBe` "confluence-prod"
                    cmdb.cmdbSpaces `shouldBe` []

        it "rejects an unknown jira apiVersion" do
            let json = "{\"jiraConfigs\": {\"j\": {\"baseUrl\": \"https://j\", \"tokenEnv\": \"JIRA_TOKEN\", \"apiVersion\": \"4\"}}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("unknown jira apiVersion" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "parses the autoAnalyze gate with defaults" do
            case parseProvisionConfig "{\"autoAnalyze\": {}}" of
                Left err -> expectationFailure (cs err)
                Right config -> do
                    let Just gate = config.autoAnalyze
                    gate.aaItemStatuses `shouldBe` ["firing", "ack"]
                    gate.aaItemSeverities `shouldBe` ["critical", "high", "warning", "info"]
                    gate.aaItemEnvironments `shouldBe` []
                    gate.aaItemEnabled `shouldBe` True

        it "parses the autoAnalyze env scope" do
            case parseProvisionConfig "{\"autoAnalyze\": {\"environments\": [\"dev\", \"staging\"]}}" of
                Left err -> expectationFailure (cs err)
                Right config -> do
                    let Just gate = config.autoAnalyze
                    gate.aaItemEnvironments `shouldBe` ["dev", "staging"]

        it "rejects unknown statuses in autoAnalyze" do
            case parseProvisionConfig "{\"autoAnalyze\": {\"statuses\": [\"firing\", \"bogus\"]}}" of
                Left err -> err `shouldSatisfy` ("unknown alert status" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

    describe "parseHostGroupsFile" do
        it "parses a bare array of groups" do
            parseHostGroupsFile "[{\"groupid\": \"2\", \"name\": \"Linux servers\"}]"
                `shouldBe` Right [ZabbixGroup "2" "Linux servers"]

        it "parses a full hostgroup.get response" do
            parseHostGroupsFile "{\"jsonrpc\": \"2.0\", \"result\": [{\"groupid\": \"4\", \"name\": \"Hypervisors\"}], \"id\": 1}"
                `shouldBe` Right [ZabbixGroup "4" "Hypervisors"]

        it "rejects invalid JSON" do
            case parseHostGroupsFile "nope" of
                Left err -> err `shouldSatisfy` ("invalid JSON:" `isPrefixOf`)
                Right _ -> expectationFailure "expected parse failure"
