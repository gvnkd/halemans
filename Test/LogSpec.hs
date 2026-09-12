module Test.LogSpec (spec) where

import Application.Service.Log (LogLevel (..), parseLogLevel)
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Application.Service.Log" do
    describe "parseLogLevel" do
        it "parses the four levels" do
            parseLogLevel "debug" `shouldBe` Just LogDebug
            parseLogLevel "info" `shouldBe` Just LogInfo
            parseLogLevel "warn" `shouldBe` Just LogWarn
            parseLogLevel "error" `shouldBe` Just LogError
        it "is case-insensitive and trims whitespace" do
            parseLogLevel " DEBUG " `shouldBe` Just LogDebug
            parseLogLevel "Warn" `shouldBe` Just LogWarn
        it "rejects unknown values" do
            parseLogLevel "verbose" `shouldBe` Nothing
            parseLogLevel "" `shouldBe` Nothing
    describe "LogLevel ordering" do
        it "orders debug < info < warn < error for filtering" do
            LogDebug < LogInfo `shouldBe` True
            LogInfo < LogWarn `shouldBe` True
            LogWarn < LogError `shouldBe` True
