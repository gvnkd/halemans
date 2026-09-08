module Test.DashboardConfigSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport (newRecord)
import Generated.Types
import qualified Data.Aeson as Aeson
import Data.Aeson (object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import Application.Helper.DashboardConfig
import Application.Pipeline.Grouping (AlertField (..))

spec :: Spec
spec = describe "Application.Helper.DashboardConfig" do
    let legacyCards =
            [ legacyCard "dev" ["firing"] ["critical", "high"]
            , legacyCard "prod" [] []
            ]
    describe "legacy M3 cards" do
        it "round-trips through JSON" do
            decodeDashboardConfig (encodeDashboardConfig legacyCards) `shouldBe` Right legacyCards
        it "decodes legacy shape into match clauses" do
            decodeDashboardConfig (Aeson.toJSON [object ["env" .= ("dev" :: Text)]])
                `shouldBe` Right [legacyCard "dev" [] []]
        it "re-encodes legacy cards in the legacy shape" do
            let raw = Aeson.toJSON [object
                    [ "env" .= ("dev" :: Text)
                    , "filters" .= object ["status" .= (["firing"] :: [Text]), "severity" .= ([] :: [Text])]
                    ]]
            fmap encodeDashboardConfig (decodeDashboardConfig raw) `shouldBe` Right raw
        it "rejects non-card JSON" do
            decodeDashboardConfig (Aeson.toJSON (42 :: Int)) `shouldSatisfy` isLeft
        it "rejects objects with neither env nor v2 keys usefully" do
            -- a bare object decodes as an empty v2 card (tolerant), which is
            -- intentional; nonsense scalars still fail
            decodeDashboardConfig (Aeson.toJSON ["nope" :: Text]) `shouldSatisfy` isLeft
    describe "v2 cards (milestone 9)" do
        let v2json = Aeson.toJSON [object
                [ "title" .= ("PostgreSQL clusters" :: Text)
                , "match" .= [ object ["facet" .= ("attr:Service" :: Text), "op" .= ("=" :: Text), "value" .= ("PostgreSQL" :: Text)]
                             , object ["facet" .= ("field:severity" :: Text), "op" .= ("in" :: Text), "values" .= (["critical", "high"] :: [Text])]
                             ]
                , "groupBy" .= ("attr:DB Cluster" :: Text)
                , "limit" .= (50 :: Int)
                ]]
        it "decodes the v2 schema" do
            decodeDashboardConfig v2json `shouldBe` Right
                [ DashboardCard
                    { cardTitle = Just "PostgreSQL clusters"
                    , cardMatch =
                        [ MatchClause (FacetAttr "Service") OpEq "PostgreSQL" []
                        , MatchClause (FacetField FieldSeverity) OpIn "" ["critical", "high"]
                        ]
                    , cardGroupBy = Just (FacetAttr "DB Cluster")
                    , cardLimit = 50
                    , cardLegacy = False
                    , cardExtras = mempty
                    }
                ]
        it "round-trips v2 cards through JSON" do
            case decodeDashboardConfig v2json of
                Left err -> expectationFailure (cs err)
                Right cards -> encodeDashboardConfig cards `shouldBe` v2json
        it "defaults op to = and limit to 100" do
            let raw = Aeson.toJSON [object ["match" .= [object ["facet" .= ("label:team" :: Text), "value" .= ("dba" :: Text)]]]]
            decodeDashboardConfig raw `shouldBe` Right
                [ DashboardCard Nothing [MatchClause (FacetLabel "team") OpEq "dba" []] Nothing 100 False mempty ]
        it "preserves unknown keys on round-trip" do
            let raw = Aeson.toJSON [object
                    [ "match" .= ([] :: [Int])
                    , "kcl" .= object ["generator" .= ("dashboards.k" :: Text)]
                    ]]
            case decodeDashboardConfig raw of
                Left err -> expectationFailure (cs err)
                Right cards -> case cards of
                    [card] -> cardExtras card
                        `shouldBe` KeyMap.fromList [("kcl", Aeson.toJSON (object ["generator" .= ("dashboards.k" :: Text)]))]
                    _ -> expectationFailure "expected one card"
        it "rejects malformed clauses naming the card index" do
            let bad op extra = Aeson.toJSON [object
                    [ "title" .= ("ok" :: Text) ]
                    , object (["match" .= [object (["facet" .= ("attr:Service" :: Text), "op" .= (op :: Text)] ++ extra)]]) ]
            case decodeDashboardConfig (bad "=" []) of
                Left err -> err `shouldSatisfy` (\e -> "card 1" `isInfixOfText` e)
                Right _ -> expectationFailure "missing value accepted"
            decodeDashboardConfig (bad "~~" ["value" .= ("x" :: Text)]) `shouldSatisfy` isLeft
            decodeDashboardConfig (Aeson.toJSON [object ["match" .= [object ["facet" .= ("bogus:x" :: Text), "value" .= ("y" :: Text)]]]])
                `shouldSatisfy` isLeft
            decodeDashboardConfig (Aeson.toJSON [object ["match" .= [object ["facet" .= ("field:nope" :: Text), "value" .= ("y" :: Text)]]]])
                `shouldSatisfy` isLeft
            decodeDashboardConfig (Aeson.toJSON [object ["match" .= [object ["facet" .= ("field:severity" :: Text), "op" .= ("in" :: Text), "values" .= ([] :: [Text])]]]])
                `shouldSatisfy` isLeft
            decodeDashboardConfig (Aeson.toJSON [object ["limit" .= (0 :: Int)]]) `shouldSatisfy` isLeft
            decodeDashboardConfig (Aeson.toJSON [object ["groupBy" .= ("bogus:x" :: Text)]]) `shouldSatisfy` isLeft
    describe "matchCardAlert" do
        let alert = newRecord @Alert
                |> set #severity "critical"
                |> set #status "firing"
                |> set #env (Just "dev")
                |> set #labels (object ["team" .= ("infra" :: Text)])
                |> set #facets (object ["Service" .= ("PostgreSQL" :: Text), "DB Cluster" .= ("ibstaffcopdb01" :: Text)])
        it "matches field/label/attr clauses as conjunction" do
            let card clauses = DashboardCard Nothing clauses Nothing 100 False mempty
            matchCardAlert (card [MatchClause (FacetAttr "Service") OpEq "PostgreSQL" []]) alert `shouldBe` True
            matchCardAlert (card [MatchClause (FacetAttr "Service") OpEq "MySQL" []]) alert `shouldBe` False
            matchCardAlert (card [MatchClause (FacetField FieldEnv) OpEq "dev" [], MatchClause (FacetAttr "DB Cluster") OpGlob "ib*" []]) alert `shouldBe` True
            matchCardAlert (card [MatchClause (FacetLabel "team") OpEq "infra" []]) alert `shouldBe` True
            matchCardAlert (card [MatchClause (FacetField FieldSeverity) OpIn "" ["critical", "high"]]) alert `shouldBe` True
            matchCardAlert (card [MatchClause (FacetAttr "Service") OpNe "MySQL" []]) alert `shouldBe` True
            matchCardAlert (card [MatchClause (FacetAttr "Absent") OpNe "MySQL" []]) alert `shouldBe` False
            matchCardAlert (card [MatchClause (FacetAttr "Absent") OpGlob "*" []]) alert `shouldBe` False
    describe "globToLike" do
        it "translates glob specials and escapes LIKE specials" do
            globToLike "db-*" `shouldBe` "db-%"
            globToLike "a?c" `shouldBe` "a_c"
            globToLike "100%_\\*" `shouldBe` "100\\%\\_\\\\%"

legacyCard :: Text -> [Text] -> [Text] -> DashboardCard
legacyCard env statuses severities = DashboardCard
    { cardTitle = Nothing
    , cardMatch = [MatchClause (FacetField FieldEnv) OpEq env []]
        ++ [MatchClause (FacetField FieldStatus) OpIn "" statuses | not (null statuses)]
        ++ [MatchClause (FacetField FieldSeverity) OpIn "" severities | not (null severities)]
    , cardGroupBy = Nothing
    , cardLimit = 100
    , cardLegacy = True
    , cardExtras = mempty
    }

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isInfixOfText :: Text -> Text -> Bool
isInfixOfText = Text.isInfixOf
