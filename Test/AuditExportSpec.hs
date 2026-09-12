module Test.AuditExportSpec where

import Application.Service.AuditExport
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Application.Service.AuditExport" do
    let at = UTCTime (fromGregorian 2026 9 5) 3600
        row =
            ExportRow
                { eventId = "e-1"
                , eventCreatedAt = at
                , alertId = "a-1"
                , alertTitle = "cpu hot"
                , alertEnv = Just "prod"
                , kind = "created"
                , userId = Nothing
                , payload = object ["note" .= ("hello" :: Text)]
                }

    describe "renderCsv" do
        it "emits the fixed header then one line per row" do
            let rendered = renderCsv [row]
                outputLines = Text.lines rendered
            length outputLines `shouldBe` 2
            head outputLines `shouldBe` Just csvHeader
            outputLines !! 1 `shouldBe` "e-1,2026-09-05T01:00:00Z,a-1,cpu hot,prod,created,,\"{\"\"note\"\":\"\"hello\"\"}\""
        it "quotes fields containing commas, quotes and newlines" do
            let tricky = row{alertTitle = "a,b \"quoted\"\nline"}
                rendered = renderCsv [tricky]
            rendered `shouldSatisfy` Text.isInfixOf "\"a,b \"\"quoted\"\"\nline\""
            rendered `shouldSatisfy` Text.isInfixOf "line\""
        it "renders zero rows as just the header" do
            renderCsv [] `shouldBe` csvHeader <> "\n"

    describe "renderJsonl" do
        it "emits one valid json object per row" do
            let rendered = renderJsonl [row, row{kind = "ack"}]
                outputLines = Text.lines rendered
            length outputLines `shouldBe` 2
            forM_ outputLines \line ->
                (Aeson.decode (cs line) :: Maybe Aeson.Value) `shouldSatisfy` isJust
        it "round-trips the fixed field set" do
            let decoded = Aeson.decode (cs (Text.replace "\n" "" (renderJsonl [row]))) :: Maybe Aeson.Value
            case decoded of
                Just value -> do
                    field "event_id" value `shouldBe` Just "e-1"
                    field "kind" value `shouldBe` Just "created"
                    field "environment" value `shouldBe` Just "prod"
                Nothing -> expectationFailure "jsonl row did not decode"
  where
    field :: Text -> Aeson.Value -> Maybe Text
    field key = parseMaybe (Aeson.withObject "row" (\o -> o Aeson..: Key.fromText key))
