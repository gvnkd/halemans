module Test.AgentSpec where

import Application.Service.Agent.Tools (agentToolDefinitions, requiredPrivilegeFor)
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
import IHP.Prelude
import Test.Hspec

-- Pure shape checks on the agent tool registry (internal API milestone);
-- execution and RBAC gating are covered in Test.Integration.AgentSpec.
spec :: Spec
spec = describe "Application.Service.Agent.Tools" do
    describe "tool catalog" do
        it "tags tools with the privilege they need (RBAC at the executor)" do
            requiredPrivilegeFor "search_alerts" `shouldBe` Just "view"
            requiredPrivilegeFor "ack_alert" `shouldBe` Just "ack"
            requiredPrivilegeFor "close_alert" `shouldBe` Just "close"
            requiredPrivilegeFor "create_blackout" `shouldBe` Just "manage_blackouts"
            requiredPrivilegeFor "create_team" `shouldBe` Just "manage_users"
            requiredPrivilegeFor "create_escalation_policy" `shouldBe` Just "manage_rules"
            requiredPrivilegeFor "list_sources" `shouldBe` Just "manage_sources"
            requiredPrivilegeFor "get_profile" `shouldBe` Nothing
            requiredPrivilegeFor "create_dashboard" `shouldBe` Nothing
        it "names are unique and cover the P1-P3 subsystems" do
            let names = [name | tool <- agentToolDefinitions, Just (String name) <- [functionField "name" tool]]
            length names `shouldSatisfy` (>= 30)
            length (nub names) `shouldBe` length names
        it "marks required parameters" do
            requiredOf "validate_dashboard" `shouldBe` ["name", "config"]
            requiredOf "create_blackout" `shouldBe` ["starts_at", "ends_at"]
            requiredOf "create_team" `shouldBe` ["name"]
            requiredOf "search_alerts" `shouldBe` []
        it "declares confirmed optional on mutating tools" do
            let confirmedProp = do
                    tool <- findTool "create_blackout"
                    parameters <- functionField "parameters" tool
                    properties <- field "properties" parameters
                    field "confirmed" properties
            (isJust confirmedProp) `shouldBe` True
  where
    requiredOf name = case findTool name >>= functionField "parameters" >>= field "required" of
        Just (Array items) -> [text | String text <- Vector.toList items]
        _ -> []
    findTool name = case [tool | tool <- agentToolDefinitions, functionField "name" tool == Just (String name)] of
        (tool : _) -> Just tool
        [] -> Nothing
    functionField key tool = field "function" tool >>= field key
    field key (Object obj) = KeyMap.lookup key obj
    field _ _ = Nothing
