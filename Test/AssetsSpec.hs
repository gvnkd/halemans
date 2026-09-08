module Test.AssetsSpec where

import Test.Hspec
import IHP.Prelude
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Application.Service.Assets.Types
import Application.Service.Assets.Aql
import Application.Service.Assets.Errors
import Application.Service.Llm.Prompt
import Application.Service.Llm.Roles (toolsForRole, roleToolNames, templateNameForRole)
import Generated.Types
import IHP.ModelSupport (newRecord)
import IHP.Record (set)

spec :: Spec
spec = describe "Milestone 8 assets subsystem" do
    describe "Types.envelopeParser (mixed envelopes, assets-api.md §8.1)" do
        let decodeWith :: [Text] -> Text -> Maybe [Aeson.Value]
            decodeWith keys raw = Aeson.decode (cs raw) >>= parseMaybe (envelopeParser keys)
        it "accepts a bare array" do
            length <$> decodeWith ["objectschemas"] "[1,2,3]" `shouldBe` Just 3
        it "accepts the objectschemas wrapper" do
            length <$> decodeWith ["objectschemas", "values"] "{\"objectschemas\": [1,2]}" `shouldBe` Just 2
        it "accepts the objectEntries wrapper" do
            length <$> decodeWith ["values", "objectEntries"] "{\"objectEntries\": [1]}" `shouldBe` Just 1
        it "fails when no known wrapper key is present" do
            decodeWith ["objectschemas"] "{\"other\": [1]}" `shouldBe` Nothing
        it "decodes a wrapped schema list with Int ids (§8.2)" do
            let raw :: Text
                raw = "{\"objectschemas\": [{\"id\": 110, \"name\": \"Capacity CMDB\", \"objectSchemaKey\": \"CHCMDB\"}]}"
                decoded = Aeson.decode (cs raw) >>= parseMaybe (envelopeParser ["objectschemas"])
            fmap (map schemaName) decoded `shouldBe` Just ["Capacity CMDB"]
            fmap (map schemaId) decoded `shouldBe` Just [110]

    describe "Types.flattenAttributes" do
        it "maps attribute names to display values incl. status" do
            let raw = Aeson.object
                    [ "id" Aeson..= (5 :: Int)
                    , "objectTypeAttribute" Aeson..= Aeson.object ["id" Aeson..= (5 :: Int), "name" Aeson..= ("Status" :: Text)]
                    , "objectAttributeValues" Aeson..= [Aeson.object
                        [ "status" Aeson..= Aeson.object ["id" Aeson..= (1 :: Int), "name" Aeson..= ("Active" :: Text)]
                        , "displayValue" Aeson..= ("Active" :: Text)]]
                    ]
                parsed = Aeson.fromJSON raw :: Aeson.Result ObjectAttribute
            case parsed of
                Aeson.Error err -> expectationFailure err
                Aeson.Success attribute -> do
                    flattenAttributes [attribute] `shouldBe` [("Status", "Active")]
                    case attribute.attrValues of
                        (firstValue:_) -> firstValue.valueStatusId `shouldBe` Just (1 :: Int64)
                        [] -> expectationFailure "no attribute values parsed"

    describe "Aql escaper (§5.4)" do
        it "escapes double quotes" do
            quoteAql "15\"" `shouldBe` "\"15\\\"\""
        it "escapes backslashes" do
            quoteAql "a\\b" `shouldBe` "\"a\\\\b\""
        it "passes Unicode/Cyrillic through" do
            quoteAql "Объект" `shouldBe` "\"Объект\""
        it "builds schema equality with the display name" do
            aqlText (schemaEq "Capacity CMDB") `shouldBe` "objectSchema = \"Capacity CMDB\""
        it "builds AND chains" do
            aqlText (andAql [schemaEq "S", attrLike "Name" "web"])
                `shouldBe` "objectSchema = \"S\" AND \"Name\" like \"web\""
        it "quotes attribute names with spaces" do
            aqlText (attrEq "Operating System" "Ubuntu")
                `shouldBe` "\"Operating System\" = \"Ubuntu\""
        it "fills the {host} template placeholder through the escaper" do
            aqlText (fillHostTemplate "objectSchema = \"S\" AND Name like \"{host}\"" "evil\"host")
                `shouldBe` "objectSchema = \"S\" AND Name like \"evil\\\"host\""

    describe "Errors.classifyResponse (§3)" do
        let shapeA404 :: Text
            shapeA404 = "{\"errorMessages\":[\"NotFoundInsightException: Не удалось найти элемент «Объект» с идентификатором «1»\"],\"errors\":{}}"
            shapeB404 :: Text
            shapeB404 = "<?xml version=\"1.0\"?><status><status-code>404</status-code></status>"
        it "maps 401 to AuthFailed" do
            classifyResponse Nothing 401 "" `shouldBe` AuthFailed
        it "maps shape-A 404 with the stable prefix to NotFound" do
            classifyResponse (Just 42) 404 (cs shapeA404) `shouldBe` NotFound 42
        it "maps shape-B XML 404 to Upstream (not NotFound)" do
            classifyResponse Nothing 404 (cs shapeB404) `shouldSatisfy` \case
                Upstream 404 _ -> True
                _ -> False
        it "maps any 3xx to Redirected (login fallback)" do
            classifyResponse Nothing 302 "<html>login</html>" `shouldBe` Redirected
        it "maps other statuses to Upstream with a body excerpt" do
            classifyResponse Nothing 500 "boom" `shouldBe` Upstream 500 "boom"

    describe "Types.hasMorePages (§4.4 pagination)" do
        let pageResult toIndex total = ObjectListResult [] total 1 toIndex
        it "continues while toIndex < totalFilterCount" do
            hasMorePages (pageResult 5 10) `shouldBe` True
        it "stops at the last page" do
            hasMorePages (pageResult 10 10) `shouldBe` False
        it "decodes the ObjectListResult shape" do
            let raw :: Text
                raw = "{\"objectEntries\": [], \"totalFilterCount\": 7, \"startIndex\": 1, \"toIndex\": 5}"
                decoded = Aeson.decode (cs raw) :: Maybe ObjectListResult
            fmap listTotalFilterCount decoded `shouldBe` Just 7
            fmap listToIndex decoded `shouldBe` Just 5

    describe "Roles.toolsForRole (milestone_8.md §7)" do
        let roleWith tools = newRecord @LlmAgentRole |> set #tools tools
        it "no role yields the full built-in tool set" do
            length (toolsForRole Nothing) `shouldBe` 3
        it "a role filters tool definitions by name" do
            let role = roleWith (Aeson.toJSON ["cmdb_lookup" :: Text])
            length (toolsForRole (Just role)) `shouldBe` 1
        it "an empty tools array yields no tools" do
            let role = roleWith (Aeson.toJSON ([] :: [Text]))
            toolsForRole (Just role) `shouldBe` []
        it "roleToolNames parses the jsonb array" do
            roleToolNames (roleWith (Aeson.toJSON ["assets_lookup" :: Text])) `shouldBe` ["assets_lookup"]
        it "templateNameForRole falls back to alert_enrichment" do
            templateNameForRole Nothing `shouldBe` "alert_enrichment"

    describe "Prompt assets excerpt (milestone_8.md §6)" do
        let template = Text.intercalate "\n"
                [ "Title: {{alert.title}}"
                , "Events: {{events}}"
                , "Assets: {{assets_excerpt}}"
                , "CMDB: {{cmdb_excerpt}}"
                ]
        it "renders the assets_excerpt binding" do
            let inputs = emptyInputs { piTitle = "t", piAssetsExcerpt = "host-1 [Host] | Owner: team-sre" }
            "host-1 [Host] | Owner: team-sre" `Text.isInfixOf` fitPrompt 100 template inputs `shouldBe` True
        it "truncates assets after events but before cmdb" do
            let inputs = emptyInputs
                    { piTitle = "t"
                    , piEvents = Text.replicate 100 "e"
                    , piAssetsExcerpt = Text.replicate 100 "a"
                    , piCmdbExcerpt = Text.replicate 100 "c"
                    }
                rendered = fitPrompt 90 template inputs
            -- events survive, assets and cmdb are shrunk away first
            "ee" `Text.isInfixOf` rendered `shouldBe` True
            Text.length rendered `shouldSatisfy` (<= charBudgetForTokens 90)

    describe "Prompt granular asset slots" do
        it "renders assets.count/labels/types bindings" do
            let inputs = emptyInputs
                    { piAssetsCount = "2"
                    , piAssetsLabels = "host-1, host-2"
                    , piAssetsTypes = "Host"
                    }
            renderTemplate "n={{assets.count}} l={{assets.labels}} t={{assets.types}}" (bindingsFor inputs)
                `shouldBe` "n=2 l=host-1, host-2 t=Host"
        it "renders per-attribute slots" do
            let inputs = emptyInputs { piAssetsAttrs = [("Owner", "team-sre"), ("Cluster", "eu-1")] }
            renderTemplate "owner={{assets.attr.Owner}} on {{assets.attr.Cluster}}" (bindingsFor inputs)
                `shouldBe` "owner=team-sre on eu-1"
        it "templateSlotNames lists the static slots and the attr pattern" do
            let expected = ["alert.title", "assets_excerpt", "assets.count", "assets.labels", "assets.types", "assets.attr.<AttributeName>"]
            templateSlotNames `shouldSatisfy` \names -> all (`elem` names) expected
        it "granular asset fields shrink under budget pressure" do
            let inputs = emptyInputs
                    { piTitle = "t"
                    , piAssetsExcerpt = Text.replicate 50 "a"
                    , piAssetsLabels = Text.replicate 50 "l"
                    , piAssetsTypes = Text.replicate 50 "y"
                    , piCmdbExcerpt = Text.replicate 50 "c"
                    }
                rendered = fitPrompt 30 "T: {{alert.title}} A: {{assets_excerpt}} L: {{assets.labels}} Y: {{assets.types}} C: {{cmdb_excerpt}}" inputs
            Text.length rendered `shouldSatisfy` (<= charBudgetForTokens 30)

    describe "Prompt.collectAssetAttrs" do
        let objectWith attrs = newRecord @AssetsObject |> set #attributes (Aeson.object attrs)
        it "aggregates distinct values per attribute across objects" do
            let objects =
                    [ objectWith ["Owner" Aeson..= ("team-sre" :: Text), "Cluster" Aeson..= ("eu-1" :: Text)]
                    , objectWith ["Owner" Aeson..= ("team-ops" :: Text), "Cluster" Aeson..= ("eu-1" :: Text)]
                    ]
            -- attributes arrive in Aeson KeyMap order, not insertion order
            sortOn fst (collectAssetAttrs objects) `shouldBe` [("Cluster", "eu-1"), ("Owner", "team-sre, team-ops")]
        it "drops empty values and brace-containing names" do
            let objects = [objectWith ["Owner" Aeson..= ("" :: Text), "bad}name" Aeson..= ("x" :: Text)]]
            collectAssetAttrs objects `shouldBe` [("Owner", "")]
