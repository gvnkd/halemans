module Test.FacetsSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport (newRecord)
import Generated.Types
import Data.Aeson (object, (.=))
import qualified Data.Aeson.Key as Key
import Application.Service.Facets (resolveFacets)

-- Facet resolution (milestone_9.md §3): precedence, fallback, verbatim
-- whitelisted attributes, mapping-wins on collision.

alert :: Alert
alert = newRecord @Alert
    |> set #severity "high"
    |> set #status "firing"
    |> set #env (Just "zabbix-prod")
    |> set #host (Just "dev-host-01")
    |> set #labels (object ["team" .= ("label-team" :: Text)])

mapping :: Text -> Int -> Text -> Text -> FieldMapping
mapping facet rank kind key = newRecord @FieldMapping
    |> set #facet facet
    |> set #rank rank
    |> set #kind kind
    |> set #key key
    |> set #enabled True

objectWith :: [(Text, Text)] -> AssetsObject
objectWith attrs = newRecord @AssetsObject
    |> set #attributes (object [Key.fromText name .= value | (name, value) <- attrs])

spec :: Spec
spec = describe "Application.Service.Facets.resolveFacets" do
    it "field mappings read alert columns" do
        resolveFacets [mapping "env" 100 "field" "env"] [] [] alert
            `shouldBe` [("env", "zabbix-prod")]
    it "label mappings read alerts.labels" do
        resolveFacets [mapping "team" 100 "label" "team"] [] [] alert
            `shouldBe` [("team", "label-team")]
    it "attr mappings read the linked object's attributes" do
        let object = objectWith [("Environments", "PROD")]
        resolveFacets [mapping "env" 50 "attr" "Environments"] [] [object] alert
            `shouldBe` [("env", "PROD")]
    it "lower rank wins; fallback when the higher-priority source is absent" do
        let mappings =
                [ mapping "env" 50 "attr" "Environments"
                , mapping "env" 100 "field" "env"
                ]
            object = objectWith [("Environments", "PROD")]
        -- override beats the source-reported env
        resolveFacets mappings [] [object] alert `shouldBe` [("env", "PROD")]
        -- attribute absent: falls back to the alert field
        resolveFacets mappings [] [] alert `shouldBe` [("env", "zabbix-prod")]
        -- attribute present but empty: also falls back
        resolveFacets mappings [] [objectWith [("Environments", "")]] alert `shouldBe` [("env", "zabbix-prod")]
    it "unknown fields/kinds are skipped, not fatal" do
        let mappings =
                [ mapping "env" 10 "field" "bogus"
                , mapping "env" 20 "weird" "env"
                , mapping "env" 100 "field" "env"
                ]
        resolveFacets mappings [] [] alert `shouldBe` [("env", "zabbix-prod")]
    it "disabled mappings never win" do
        let disabled = mapping "env" 1 "attr" "Environments" |> set #enabled False
            object = objectWith [("Environments", "PROD")]
        resolveFacets [disabled, mapping "env" 100 "field" "env"] [] [object] alert
            `shouldBe` [("env", "zabbix-prod")]
    it "whitelisted attributes copy verbatim as facets" do
        let object = objectWith [("Service", "PostgreSQL"), ("DB Cluster", "ibstaffcopdb01"), ("Other", "x")]
        resolveFacets [] ["Service", "DB Cluster"] [object] alert
            `shouldBe` [("Service", "PostgreSQL"), ("DB Cluster", "ibstaffcopdb01")]
    it "mapping-derived facets win on key collision with verbatim attributes" do
        let object = objectWith [("Service", "PostgreSQL")]
        resolveFacets [mapping "Service" 100 "field" "severity"] ["Service"] [object] alert
            `shouldBe` [("Service", "high")]
    it "attr mappings take the first element of a comma-separated value" do
        let object = objectWith [("Environments", "PROD,TEST")]
        resolveFacets [mapping "env" 50 "attr" "Environments"] [] [object] alert
            `shouldBe` [("env", "PROD")]
    it "attr mappings strip whitespace around the first element" do
        let object = objectWith [("Environments", " PROD , TEST ")]
        resolveFacets [mapping "env" 50 "attr" "Environments"] [] [object] alert
            `shouldBe` [("env", "PROD")]
    it "verbatim copies keep the full comma-separated value" do
        let object = objectWith [("Environments", "PROD,TEST")]
        resolveFacets [] ["Environments"] [object] alert
            `shouldBe` [("Environments", "PROD,TEST")]
    it "multi-object attributes are first-wins" do
        let first = objectWith [("DB Cluster", "ibstaffcopdb01")]
            second = objectWith [("DB Cluster", "ibstaffcopdb02")]
        resolveFacets [mapping "cluster" 100 "attr" "DB Cluster"] [] [first, second] alert
            `shouldBe` [("cluster", "ibstaffcopdb01")]
    it "objects lacking the attribute are skipped for verbatim facets" do
        let with = objectWith [("Service", "PostgreSQL")]
            without = objectWith [("Name", "n")]
        resolveFacets [] ["Service"] [without, with] alert
            `shouldBe` [("Service", "PostgreSQL")]
