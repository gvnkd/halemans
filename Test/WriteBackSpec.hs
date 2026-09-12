module Test.WriteBackSpec where

import Application.Service.WriteBack
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Generated.Types
import IHP.ModelSupport (newRecord)
import IHP.Prelude
import Test.Helpers (atTime)
import Test.Hspec

spec :: Spec
spec = describe "Application.Service.WriteBack" do
    describe "backoffSeconds" do
        it "follows the 1m/5m/15m schedule then caps" do
            map backoffSeconds [1 .. 6] `shouldBe` [60, 300, 900, 900, 900, 900]

    describe "zabbixActionBits" do
        it "maps actions to event.acknowledge bitmasks" do
            zabbixActionBits "ack" `shouldBe` 6
            zabbixActionBits "unack" `shouldBe` 20
            zabbixActionBits "close" `shouldBe` 5

    describe "silenceMatchersFor" do
        it "builds one equality matcher per label" do
            let alert =
                    newRecord @Alert
                        |> set #labels (object ["alertname" .= ("cpu" :: Text), "host" .= ("dev-host-01" :: Text)])
            case silenceMatchersFor alert of
                Aeson.Array matchers -> do
                    length matchers `shouldBe` 2
                    forM_ (Vector.toList matchers) \matcher ->
                        Text.isInfixOf "\"isRegex\":false" (cs (Aeson.encode matcher)) `shouldBe` True
                other -> expectationFailure (cs ("expected matcher array, got " <> show other :: Text))
        it "builds an empty matcher list without labels" do
            silenceMatchersFor (newRecord @Alert) `shouldBe` Aeson.toJSON ([] :: [Aeson.Value])

    describe "silenceEndsAt" do
        let now = atTime "2026-09-04 12:00:00 UTC"
        it "ack uses the ack expiry when set" do
            let expiry = atTime "2026-09-04 14:00:00 UTC"
                alert = newRecord @Alert |> set #ackExpiresAt (Just expiry)
            silenceEndsAt now "ack" alert `shouldBe` expiry
        it "ack defaults to 24h without an expiry" do
            silenceEndsAt now "ack" (newRecord @Alert) `shouldBe` addUTCTime 86400 now
        it "close always uses 24h" do
            let expiry = atTime "2026-09-04 13:00:00 UTC"
                alert = newRecord @Alert |> set #ackExpiresAt (Just expiry)
            silenceEndsAt now "close" alert `shouldBe` addUTCTime 86400 now
