module Test.ProvisionSpec where

import Test.Hspec
import IHP.Prelude
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Application.Service.Provision
import Application.Connector.Zabbix (ZabbixGroup (..))

-- Unit coverage for the milestone 7 provision config parser (§9): round-trip
-- of valid fixtures, unknown-field rejection, required-field errors naming
-- the path, strict defaults/validation.

spec :: Spec
spec = describe "Application.Service.Provision" do
    describe "parseProvisionConfig" do
        it "parses the empty config" do
            parseProvisionConfig "{}" `shouldBe` Right (ProvisionConfig Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

        it "parses a full config" do
            let json = Aeson.encode $ object
                    [ "users" .= object ["strict" .= True, "items" .= [object
                        [ "email" .= ("ops@example.com" :: Text)
                        , "displayName" .= ("Ops" :: Text)
                        , "passwordHash" .= ("sha256|17|salt|hash" :: Text)
                        , "roles" .= (["admin"] :: [Text])
                        , "settings" .= object ["theme" .= ("dark" :: Text)]
                        ]]]
                    , "sources" .= object ["items" .= [object
                        [ "type" .= ("zabbix" :: Text)
                        , "name" .= ("zabbix-prod" :: Text)
                        , "baseUrl" .= ("https://zabbix.example" :: Text)
                        , "env" .= ("prod" :: Text)
                        , "pollIntervalSeconds" .= (30 :: Int)
                        , "enabled" .= True
                        , "config" .= object ["tokenEnv" .= ("ZABBIX_TOKEN" :: Text)]
                        , "webhookTokens" .= [object ["tokenEnv" .= ("HALEMANS_AM_HOOK_TOKEN" :: Text)]]
                        ]]]
                    , "teams" .= object ["items" .= [object
                        [ "name" .= ("sre" :: Text)
                        , "description" .= ("Site reliability engineering" :: Text)
                        , "hostGroups" .= (["Linux servers"] :: [Text])
                        , "defaults" .= object []
                        , "members" .= [object ["email" .= ("ops@example.com" :: Text), "role" .= ("lead" :: Text)]]
                        , "defaultDashboardConfig" .= [object ["env" .= ("prod" :: Text)]]
                        ]]]
                    , "llm" .= object ["items" .= [object
                        [ "providerName" .= ("default" :: Text)
                        , "endpoint" .= ("http://127.0.0.1:18084" :: Text)
                        , "model" .= ("qwen" :: Text)
                        , "apiKeyEnv" .= ("LLM_API_KEY" :: Text)
                        , "toolsEnabled" .= False
                        , "enabled" .= True
                        , "promptTemplates" .= [object
                            [ "name" .= ("alert_enrichment" :: Text)
                            , "version" .= (1 :: Int)
                            , "body" .= ("..." :: Text)
                            , "active" .= True
                            , "notes" .= ("provisioned" :: Text)
                            ]]
                        ]]]
                    ]
            case parseProvisionConfig json of
                Left err -> expectationFailure (cs err)
                Right config -> do
                    fmap (.strict) config.users `shouldBe` Just True
                    fmap (.strict) config.sources `shouldBe` Just False
                    let [user] = maybe [] (.items) config.users
                    user.email `shouldBe` "ops@example.com"
                    user.roles `shouldBe` ["admin"]
                    let [source] = maybe [] (.items) config.sources
                    source.sourceType `shouldBe` "zabbix"
                    map (.tokenEnv) source.webhookTokens `shouldBe` ["HALEMANS_AM_HOOK_TOKEN"]
                    let [team] = maybe [] (.items) config.teams
                    map (.role) team.members `shouldBe` ["lead"]
                    team.hostGroups `shouldBe` Just ["Linux servers"]
                    let [llm] = maybe [] (.items) config.llm
                    llm.enabled `shouldBe` True
                    map (.version) llm.promptTemplates `shouldBe` [1]

        it "rejects invalid JSON" do
            case parseProvisionConfig "{ not json" of
                Left err -> err `shouldSatisfy` ("invalid JSON:" `isPrefixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects unknown top-level keys" do
            case parseProvisionConfig "{\"userz\": {}}" of
                Left err -> err `shouldSatisfy` ("unknown field \"userz\"" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects unknown per-entity fields" do
            let json = "{\"users\": {\"items\": [{\"email\": \"a@b.c\", \"passwordHash\": \"x\", \"paswordHash2\": \"y\"}]}}"
            case parseProvisionConfig json of
                Left err -> do
                    err `shouldSatisfy` ("unknown field \"paswordHash2\"" `isInfixOf`)
                    err `shouldSatisfy` ("users" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "names the path of a missing required field" do
            let json = "{\"teams\": {\"items\": [{\"description\": \"no name\"}]}}"
            case parseProvisionConfig json of
                Left err -> do
                    err `shouldSatisfy` ("teams" `isInfixOf`)
                    err `shouldSatisfy` ("name" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "defaults strict to false" do
            let json = "{\"users\": {\"items\": []}}"
            case parseProvisionConfig json of
                Right config -> fmap (.strict) config.users `shouldBe` Just False
                Left err -> expectationFailure (cs err)

        it "strict: true with omitted items is a config error" do
            let json = "{\"sources\": {\"strict\": true}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("items" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "strict: true with empty items is allowed" do
            let json = "{\"sources\": {\"strict\": true, \"items\": []}}"
            case parseProvisionConfig json of
                Right config -> fmap (.items) config.sources `shouldBe` Just []
                Left err -> expectationFailure (cs err)

        it "rejects unknown source types" do
            let json = "{\"sources\": {\"items\": [{\"type\": \"prometheus\", \"name\": \"x\"}]}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("unknown source type" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects invalid themes in user settings" do
            let json = "{\"users\": {\"items\": [{\"email\": \"a@b.c\", \"passwordHash\": \"x\", \"settings\": {\"theme\": \"solarized\"}}]}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("unknown theme" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "applies entity defaults" do
            let json = "{\"sources\": {\"items\": [{\"type\": \"webhook\", \"name\": \"hook\"}]}}"
            case parseProvisionConfig json of
                Right config -> do
                    let [source] = maybe [] (.items) config.sources
                    source.baseUrl `shouldBe` ""
                    source.env `shouldBe` "dev"
                    source.pollIntervalSeconds `shouldBe` 30
                    source.enabled `shouldBe` True
                Left err -> expectationFailure (cs err)

        it "normalizes empty member roles to member" do
            let json = "{\"teams\": {\"items\": [{\"name\": \"t\", \"members\": [{\"email\": \"a@b.c\", \"role\": \"\"}]}]}}"
            case parseProvisionConfig json of
                Right config -> do
                    let [team] = maybe [] (.items) config.teams
                    map (.role) team.members `shouldBe` ["member"]
                Left err -> expectationFailure (cs err)

        it "leaves absent team fields as Nothing so re-provision preserves DB values" do
            let json = "{\"teams\": {\"items\": [{\"name\": \"t\"}]}}"
            case parseProvisionConfig json of
                Right config -> do
                    let [team] = maybe [] (.items) config.teams
                    team.description `shouldBe` Nothing
                    team.hostGroups `shouldBe` Nothing
                    team.defaults `shouldBe` Nothing
                Left err -> expectationFailure (cs err)

        it "parses hostGroupsFile on a zabbix source" do
            let json = "{\"sources\": {\"items\": [{\"type\": \"zabbix\", \"name\": \"z\", \"hostGroupsFile\": \"groups.json\"}]}}"
            case parseProvisionConfig json of
                Right config -> do
                    let [source] = maybe [] (.items) config.sources
                    source.hostGroupsFile `shouldBe` Just "groups.json"
                Left err -> expectationFailure (cs err)

        it "rejects hostGroupsFile on non-zabbix sources" do
            let json = "{\"sources\": {\"items\": [{\"type\": \"grafana\", \"name\": \"g\", \"hostGroupsFile\": \"groups.json\"}]}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("hostGroupsFile" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "parses field mappings and dashboards" do
            let json = Aeson.encode $ object
                    [ "fieldMappings" .= object ["items" .= [object
                        [ "facet" .= ("Environments" :: Text)
                        , "rank" .= (50 :: Int)
                        , "kind" .= ("attr" :: Text)
                        , "key" .= ("Environments" :: Text)
                        ]]]
                    , "dashboards" .= object ["items" .= [object
                        [ "name" .= ("Service matrix" :: Text)
                        , "userEmail" .= ("ops@example.com" :: Text)
                        , "position" .= (60 :: Int)
                        , "config" .= [object
                            [ "title" .= ("ETCD / EU / PROD" :: Text)
                            , "match" .= [object ["facet" .= ("attr:Service" :: Text), "op" .= ("=" :: Text), "value" .= ("ETCD" :: Text)]]
                            , "groupBy" .= ("field:host" :: Text)
                            , "limit" .= (20 :: Int)
                            ]]
                        ]]]
                    ]
            case parseProvisionConfig json of
                Left err -> expectationFailure (cs err)
                Right config -> do
                    let [mapping] = maybe [] (.items) config.fieldMappings
                    mapping.facet `shouldBe` "Environments"
                    mapping.enabled `shouldBe` True
                    let [dashboard] = maybe [] (.items) config.dashboards
                    dashboard.userEmail `shouldBe` "ops@example.com"
                    dashboard.isDefault `shouldBe` False

        it "rejects unknown field mapping kinds" do
            let json = "{\"fieldMappings\": {\"items\": [{\"facet\": \"env\", \"rank\": 1, \"kind\": \"cmdb\", \"key\": \"env\"}]}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("unknown field mapping kind" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects field-kind mappings with an unknown alert field" do
            let json = "{\"fieldMappings\": {\"items\": [{\"facet\": \"env\", \"rank\": 1, \"kind\": \"field\", \"key\": \"bogus\"}]}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("unknown alert field" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "rejects dashboards with an undecodable config" do
            let json = "{\"dashboards\": {\"items\": [{\"name\": \"d\", \"userEmail\": \"a@b.c\", \"config\": [{\"match\": [{\"facet\": \"bogus:x\", \"op\": \"=\", \"value\": \"y\"}]}]}]}}"
            case parseProvisionConfig json of
                Left err -> err `shouldSatisfy` ("invalid config for dashboard" `isInfixOf`)
                Right _ -> expectationFailure "expected parse failure"

        it "parses jira and cmdb config sections (milestone 10)" do
            let json = Aeson.encode $ object
                    [ "jiraConfigs" .= object ["items" .= [object
                        [ "name" .= ("jira-prod" :: Text)
                        , "baseUrl" .= ("https://jira.example" :: Text)
                        , "tokenEnv" .= ("JIRA_TOKEN" :: Text)
                        , "projects" .= (["OPS", "SRE"] :: [Text])
                        ]]]
                    , "cmdbConfigs" .= object ["items" .= [object
                        [ "name" .= ("confluence-prod" :: Text)
                        , "baseUrl" .= ("https://confluence.example" :: Text)
                        , "tokenEnv" .= ("CONFLUENCE_TOKEN" :: Text)
                        ]]]
                    ]
            case parseProvisionConfig json of
                Left err -> expectationFailure (cs err)
                Right config -> do
                    let [jira] = maybe [] (.items) config.jiraConfigs
                    jira.jiraProjects `shouldBe` ["OPS", "SRE"]
                    jira.jiraApiVersion `shouldBe` "3"
                    jira.jiraEnabled `shouldBe` True
                    let [cmdb] = maybe [] (.items) config.cmdbConfigs
                    cmdb.cmdbSpaces `shouldBe` []

        it "rejects an unknown jira apiVersion" do
            let json = "{\"jiraConfigs\": {\"items\": [{\"name\": \"j\", \"baseUrl\": \"https://j\", \"tokenEnv\": \"JIRA_TOKEN\", \"apiVersion\": \"4\"}]}}"
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
