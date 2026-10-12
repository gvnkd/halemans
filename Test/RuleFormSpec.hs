module Test.RuleFormSpec where

import Application.Helper.RuleForm
import Application.Pipeline.Grouping (AlertField (..), MatchExpr (..), matchExprFromJSON)
import Data.Aeson (object, (.=))
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Application.Helper.RuleForm" do
    describe "parseMatchFormFacets field clauses" do
        it "parses equals, in and not-in side by side" do
            let match =
                    parseMatchFormFacets
                        "env=dev, status in (firing, closed), severity not in (info)"
                        "component=db-*"
                        ""
            matchExprFromJSON match
                `shouldBe` MatchExpr
                    [(FieldEnv, "dev")]
                    [(FieldStatus, ["firing", "closed"])]
                    [(FieldSeverity, ["info"])]
                    [("component", "db-*")]
                    []
        it "drops unknown field names and malformed chunks" do
            let match = parseMatchFormFacets "bogus=x, env, host=web-01" "" ""
            matchExprFromJSON match `shouldBe` MatchExpr [(FieldHost, "web-01")] [] [] [] []
        it "an in-list may contain commas without splitting chunks" do
            let match = parseMatchFormFacets "title in (a, b), env=prod" "" ""
            matchExprFromJSON match
                `shouldBe` MatchExpr [(FieldEnv, "prod")] [(FieldTitle, ["a", "b"])] [] [] []
        it "equals still wins when the value contains ' in '" do
            let match = parseMatchFormFacets "title=foo in bar" "" ""
            matchExprFromJSON match `shouldBe` MatchExpr [(FieldTitle, "foo in bar")] [] [] [] []
    describe "matchFieldsText round-trip" do
        it "renders every clause kind parseably" do
            let match =
                    object
                        [ "fields"
                            .= object
                                [ "env" .= ("dev" :: Text)
                                , "status" .= object ["in" .= (["firing", "closed"] :: [Text])]
                                , "severity" .= object ["not-in" .= (["info"] :: [Text])]
                                ]
                        ]
                rendered = matchFieldsText match
            rendered `shouldBe` "env=dev, status in (firing, closed), severity not in (info)"
            matchExprFromJSON (parseMatchFormFacets rendered "" "") `shouldBe` matchExprFromJSON match
