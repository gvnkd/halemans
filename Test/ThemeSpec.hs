module Test.ThemeSpec where

import Test.Hspec
import IHP.Prelude
import Data.Aeson (object, (.=))
import Application.Helper.Theme

spec :: Spec
spec = describe "Application.Helper.Theme" do
    describe "isValidTheme" do
        it "accepts all six packs" do
            forM_ ["latte", "frappe", "macchiato", "dracula", "light", "dark"] \theme ->
                isValidTheme theme `shouldBe` True
        it "rejects unknown keys" do
            isValidTheme "solarized" `shouldBe` False
            isValidTheme "" `shouldBe` False

    describe "themeFromSettings" do
        it "reads the theme key" do
            themeFromSettings (object ["theme" .= ("frappe" :: Text)]) `shouldBe` "frappe"
        it "defaults to dark without a key" do
            themeFromSettings (object []) `shouldBe` "dark"
        it "defaults to dark on an invalid key" do
            themeFromSettings (object ["theme" .= ("bogus" :: Text)]) `shouldBe` "dark"
