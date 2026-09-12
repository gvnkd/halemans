module Test.GroupingSpec where

import Application.Pipeline.Grouping
import Data.Aeson (object, (.=))
import Generated.Types
import IHP.ModelSupport (newRecord)
import IHP.Prelude
import Test.Hspec

alert :: Alert
alert =
    newRecord @Alert
        |> set #fingerprint "test:fp"
        |> set #title "t"
        |> set #severity "critical"
        |> set #status "firing"
        |> set #env (Just "dev")
        |> set #host (Just "dev-host-01")
        |> set #service Nothing
        |> set #checkName (Just "cpu")
        |> set #labels (object ["team" .= ("infra" :: Text), "component" .= ("db-primary" :: Text)])

spec :: Spec
spec = describe "Application.Pipeline.Grouping" do
    describe "globMatch" do
        it "matches literal" do
            globMatch "abc" "abc" `shouldBe` True
            globMatch "abc" "abd" `shouldBe` False
        it "star matches any run" do
            globMatch "db-*" "db-primary" `shouldBe` True
            globMatch "*-primary" "db-primary" `shouldBe` True
            globMatch "db*" "db" `shouldBe` True
            globMatch "db-*" "web-01" `shouldBe` False
        it "question matches exactly one char" do
            globMatch "a?c" "abc" `shouldBe` True
            globMatch "a?c" "ac" `shouldBe` False
            globMatch "a?c" "abbc" `shouldBe` False

    describe "matchExprFromJSON" do
        it "parses fields and labels, ignoring unknown keys" do
            let expr =
                    matchExprFromJSON
                        ( object
                            [ "fields" .= object ["env" .= ("dev" :: Text), "bogus" .= ("x" :: Text)]
                            , "labels" .= object ["component" .= ("db-*" :: Text)]
                            ]
                        )
            expr `shouldBe` MatchExpr [(FieldEnv, "dev")] [("component", "db-*")] []
        it "parses empty object as empty match" do
            matchExprFromJSON (object []) `shouldBe` emptyMatch

    describe "matchAlert" do
        it "empty match matches everything" do
            matchAlert emptyMatch alert `shouldBe` True
        it "field equals on present subject" do
            matchAlert (MatchExpr [(FieldEnv, "dev"), (FieldHost, "dev-host-01")] [] []) alert `shouldBe` True
            matchAlert (MatchExpr [(FieldEnv, "prod")] [] []) alert `shouldBe` False
        it "missing subject never equals" do
            matchAlert (MatchExpr [(FieldService, "api")] [] []) alert `shouldBe` False
        it "severity/status are fields too" do
            matchAlert (MatchExpr [(FieldSeverity, "critical"), (FieldStatus, "firing")] [] []) alert `shouldBe` True
        it "label globs" do
            matchAlert (MatchExpr [] [("component", "db-*")] []) alert `shouldBe` True
            matchAlert (MatchExpr [] [("component", "web-*")] []) alert `shouldBe` False
            matchAlert (MatchExpr [] [("absent", "*")] []) alert `shouldBe` False
        it "conjunction: one failing clause fails all" do
            matchAlert (MatchExpr [(FieldEnv, "dev")] [("component", "web-*")] []) alert `shouldBe` False

    describe "renderTemplate" do
        it "renders subject placeholders" do
            renderTemplate "{env}/{host}" alert `shouldBe` "dev/dev-host-01"
        it "missing subject renders as dash" do
            renderTemplate "{env}/{service}" alert `shouldBe` "dev/-"
        it "label placeholder" do
            renderTemplate "{label:team}/{check}" alert `shouldBe` "infra/cpu"
            renderTemplate "{label:absent}" alert `shouldBe` "-"
        it "unknown placeholder renders as dash" do
            renderTemplate "{nope}" alert `shouldBe` "-"
        it "keeps literal text and unbalanced braces" do
            renderTemplate "group-{env}-x" alert `shouldBe` "group-dev-x"
            renderTemplate "no placeholders" alert `shouldBe` "no placeholders"
            renderTemplate "{env" alert `shouldBe` "{env"

    describe "effectiveFieldText (facet override of raw fields)" do
        let overridden = alert |> set #facets (object ["env" .= ("prod" :: Text), "host" .= ("edge-01" :: Text), "severity" .= ("info" :: Text)])
        it "facet named env/host/service wins over the raw column" do
            effectiveFieldText FieldEnv overridden `shouldBe` Just "prod"
            effectiveFieldText FieldHost overridden `shouldBe` Just "edge-01"
        it "raw column is the fallback when the facet is absent" do
            effectiveFieldText FieldEnv alert `shouldBe` Just "dev"
            effectiveFieldText FieldService overridden `shouldBe` Nothing
        it "check/severity/status are never overridden" do
            effectiveFieldText FieldSeverity overridden `shouldBe` Just "critical"
            effectiveFieldText FieldStatus overridden `shouldBe` Just "firing"
            effectiveFieldText FieldCheck overridden `shouldBe` Just "cpu"
        it "effectiveFieldSql only covers overridable fields" do
            effectiveFieldSql "alerts" FieldEnv `shouldBe` Just "coalesce(nullif(alerts.facets ->> 'env', ''), alerts.env)"
            effectiveFieldSql "alerts" FieldSeverity `shouldBe` Nothing

    describe "severity ordering" do
        it "critical > high > warning > info" do
            map severityRank ["critical", "high", "warning", "info"] `shouldBe` [3, 2, 1, 0]
        it "severityAtLeast threshold" do
            severityAtLeast "high" "critical" `shouldBe` True
            severityAtLeast "high" "high" `shouldBe` True
            severityAtLeast "high" "warning" `shouldBe` False
            severityAtLeast "info" "info" `shouldBe` True

    describe "facets (milestone 9)" do
        let faceted = alert |> set #facets (object ["DB Cluster" .= ("ibstaffcopdb01" :: Text), "env" .= ("PROD" :: Text)])
        it "facet globs read the materialized facets map" do
            matchAlert (MatchExpr [] [] [("DB Cluster", "ib*")]) faceted `shouldBe` True
            matchAlert (MatchExpr [] [] [("DB Cluster", "pg*")]) faceted `shouldBe` False
            matchAlert (MatchExpr [] [] [("absent", "*")]) faceted `shouldBe` False
        it "matchExprFromJSON parses the facets key, ignoring unknowns" do
            let expr = matchExprFromJSON (object ["facets" .= object ["DB Cluster" .= ("ib*" :: Text)]])
            expr `shouldBe` MatchExpr [] [] [("DB Cluster", "ib*")]
        it "{facet:name} placeholder renders from the facets map" do
            renderTemplate "{facet:DB Cluster}/{check}" faceted `shouldBe` "ibstaffcopdb01/cpu"
            renderTemplate "{facet:absent}" faceted `shouldBe` "-"
        it "field placeholders and field-equals use the effective (facet-overridden) value" do
            renderTemplate "{env}/{host}" faceted `shouldBe` "PROD/dev-host-01"
            matchAlert (MatchExpr [(FieldEnv, "PROD")] [] []) faceted `shouldBe` True
            matchAlert (MatchExpr [(FieldEnv, "dev")] [] []) faceted `shouldBe` False
        it "ruleReferencesFacets detects facet globs and facet placeholders" do
            let ruleWithGlob =
                    newRecord @GroupingRule
                        |> set #match (object ["facets" .= object ["DB Cluster" .= ("ib*" :: Text)]])
                        |> set #groupKeyTemplate "{env}/{host}"
                ruleWithTemplate =
                    newRecord @GroupingRule
                        |> set #match (object [])
                        |> set #groupKeyTemplate "db-{facet:DB Cluster}"
                plain =
                    newRecord @GroupingRule
                        |> set #match (object ["fields" .= object ["env" .= ("dev" :: Text)]])
                        |> set #groupKeyTemplate "{env}/{host}"
            ruleReferencesFacets ruleWithGlob `shouldBe` True
            ruleReferencesFacets ruleWithTemplate `shouldBe` True
            ruleReferencesFacets plain `shouldBe` False
