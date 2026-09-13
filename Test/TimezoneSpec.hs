module Test.TimezoneSpec where

import Application.Helper.Timezone
import Data.Aeson (object, (.=))
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Application.Helper.Timezone" do
    describe "isValidTimezone" do
        it "accepts fixed-offset labels" do
            forM_ ["UTC", "UTC+4", "UTC-3", "UTC+14", "UTC-12"] \timezone ->
                isValidTimezone timezone `shouldBe` True
        it "rejects unknown zones" do
            isValidTimezone "Asia/Tbilisi" `shouldBe` False
            isValidTimezone "UTC+15" `shouldBe` False
            isValidTimezone "" `shouldBe` False

    describe "timezoneFromSettings" do
        it "reads the timezone key" do
            timezoneFromSettings (object ["timezone" .= ("UTC+4" :: Text)]) `shouldBe` Just "UTC+4"
        it "is Nothing without a key (browser default)" do
            timezoneFromSettings (object []) `shouldBe` Nothing
        it "is Nothing on an invalid key" do
            timezoneFromSettings (object ["timezone" .= ("bogus" :: Text)]) `shouldBe` Nothing
