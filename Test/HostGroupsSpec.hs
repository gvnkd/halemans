module Test.HostGroupsSpec where

import Application.Connector.Zabbix (ZabbixGroup (..))
import Application.Service.HostGroups
import Data.Aeson (object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import Generated.Types
import IHP.ModelSupport (newRecord)
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Application.Service.HostGroups" do
    describe "hostGroupScope" do
        it "defaults to all when the key is absent" do
            hostGroupScope (newRecord @Source) `shouldBe` ScopeAll
        it "reads teams scope from source config" do
            let source = newRecord @Source |> set #config (object ["hostGroupScope" .= ("teams" :: Text)])
            hostGroupScope source `shouldBe` ScopeTeams
        it "treats unknown values as all" do
            let source = newRecord @Source |> set #config (object ["hostGroupScope" .= ("bogus" :: Text)])
            hostGroupScope source `shouldBe` ScopeAll

    describe "teamHostGroups" do
        it "is empty by default" do
            teamHostGroups (newRecord @Team) `shouldBe` []
        it "reads names from the jsonb array" do
            let team = newRecord @Team |> set #hostGroups (toJSON (["Linux servers", "Databases"] :: [Text]))
            teamHostGroups team `shouldBe` ["Linux servers", "Databases"]

    describe "teamHostGroupNames" do
        it "unions across teams without duplicates" do
            let teamA = newRecord @Team |> set #hostGroups (toJSON (["a", "b"] :: [Text]))
                teamB = newRecord @Team |> set #hostGroups (toJSON (["b", "c"] :: [Text]))
            teamHostGroupNames [teamA, teamB] `shouldBe` ["a", "b", "c"]

    describe "parseHostGroupsInput" do
        it "splits on commas and trims whitespace" do
            parseHostGroupsInput " Linux servers , Databases " `shouldBe` ["Linux servers", "Databases"]
        it "splits on newlines and drops empty parts" do
            parseHostGroupsInput "a\n\nb,\nc" `shouldBe` ["a", "b", "c"]
        it "parses empty input to an empty list" do
            parseHostGroupsInput "" `shouldBe` []

    describe "ZabbixGroup" do
        it "decodes hostgroup.get result rows" do
            Aeson.decode "{\"groupid\":\"5\",\"name\":\"Linux servers\"}" `shouldBe` Just (ZabbixGroup "5" "Linux servers")
