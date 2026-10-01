module Test.AlertScopeSpec where

import Application.Service.AlertScope (alertScopeBypassFromSettings, alertVisibleWith, groupsIntersect)
import Data.Aeson (object, toJSON, (.=))
import Generated.Types
import IHP.ModelSupport (newRecord)
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Application.Service.AlertScope" do
    describe "alertScopeBypassFromSettings" do
        it "defaults to false when the key is absent" do
            alertScopeBypassFromSettings (object []) `shouldBe` False
        it "reads the bypass flag from settings" do
            alertScopeBypassFromSettings (object ["alertScopeBypass" .= True]) `shouldBe` True
            alertScopeBypassFromSettings (object ["alertScopeBypass" .= False]) `shouldBe` False

    describe "groupsIntersect" do
        it "is true on any shared group" do
            groupsIntersect ["a" :: Text, "b"] ["b", "c"] `shouldBe` True
        it "is false on disjoint groups" do
            groupsIntersect ["a" :: Text] ["c" :: Text] `shouldBe` False
        it "is false when the user scope is empty" do
            groupsIntersect [] ["c" :: Text] `shouldBe` False

    describe "alertVisibleWith" do
        it "shows a zabbix alert when a host group intersects the scope" do
            alertVisibleWith ["G1" :: Text] True (zabbixAlert ["G1", "G2"]) `shouldBe` True
        it "hides a zabbix alert outside the scope" do
            alertVisibleWith ["G1" :: Text] True (zabbixAlert ["G3"]) `shouldBe` False
        it "hides a zabbix alert with no groups from everyone" do
            alertVisibleWith ["G1" :: Text] True (zabbixAlert []) `shouldBe` False
        it "shows non-zabbix alerts regardless of groups" do
            alertVisibleWith ["G1" :: Text] False (zabbixAlert []) `shouldBe` True
        it "always shows halemans internal alerts" do
            let internal = newRecord @Alert |> set #fingerprint ("halemans:ungrouped-host:h" :: Text) |> set #hostGroups (toJSON ([] :: [Text]))
            alertVisibleWith ["G1" :: Text] True internal `shouldBe` True

zabbixAlert :: [Text] -> Alert
zabbixAlert groups =
    newRecord @Alert
        |> set
        #fingerprint ("zabbix:trigger:42" :: Text)
        |> set
        #hostGroups (toJSON groups)
