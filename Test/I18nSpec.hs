module Test.I18nSpec where

import Application.Service.I18n
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = do
    describe "I18n.languageFromSettings" do
        it "defaults to English when the key is absent" do
            languageFromSettings (Aeson.object []) `shouldBe` LangEn
        it "reads a valid language code" do
            languageFromSettings (Aeson.object ["language" .= ("ru" :: Text)]) `shouldBe` LangRu
        it "falls back to English for an unknown code" do
            languageFromSettings (Aeson.object ["language" .= ("xx" :: Text)]) `shouldBe` LangEn
    describe "I18n.translate" do
        it "passes English through" do
            translate LangEn "Alerts" `shouldBe` "Alerts"
        it "translates a known key to Russian" do
            translate LangRu "Alerts" `shouldBe` "Алерты"
        it "falls back to the key for a missing entry" do
            translate LangRu "no such key" `shouldBe` "no such key"
    describe "I18n.translateParams" do
        it "substitutes placeholders after translation" do
            translateParams LangRu "Unknown table: {table}" [("table", "alerts")] `shouldBe` "Неизвестная таблица: alerts"
        it "substitutes placeholders in the English fallback too" do
            translateParams LangEn "Unknown table: {table}" [("table", "alerts")] `shouldBe` "Unknown table: alerts"
    describe "I18n.languageFromCode round-trip" do
        it "round-trips every listed language" do
            forM_ languages \(code, _) -> languageCode <$> languageFromCode code `shouldBe` Just code
