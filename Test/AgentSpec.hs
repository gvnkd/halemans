module Test.AgentSpec where

import Application.Service.Agent.Tools (agentToolDefinitions)
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
import IHP.Prelude
import Test.Hspec

-- Pure shape checks on the agent tool registry (internal API milestone);
-- execution paths are covered in Test/Integration/AgentSpec.
spec :: Spec
spec = describe "Application.Service.Agent.Tools" do
    describe "agentToolDefinitions" do
        it "defines the M1 tool set with names, descriptions and schemas" do
            let names = [name | Just (String name) <- map (functionField "name") agentToolDefinitions]
            names
                `shouldBe` [ "list_environments"
                           , "list_dashboards"
                           , "get_dashboard_schema"
                           , "validate_dashboard"
                           , "create_dashboard"
                           , "search_alerts"
                           , "get_llm_config"
                           ]
        it "marks required parameters" do
            requiredOf "validate_dashboard" `shouldBe` ["name", "config"]
            requiredOf "create_dashboard" `shouldBe` ["name", "config"]
            requiredOf "search_alerts" `shouldBe` []
        it "declares create_dashboard.confirmed optional" do
            let confirmedProp = do
                    tool <- findTool "create_dashboard"
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
