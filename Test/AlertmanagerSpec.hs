module Test.AlertmanagerSpec where

import Application.Connector.Alertmanager (normalize)
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..))
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import IHP.Prelude
import Test.Hspec

alertObj :: Text -> Text -> Aeson.Value
alertObj fp status =
    object
        [ "status" .= status
        , "fingerprint" .= fp
        , "labels" .= object ["alertname" .= ("HighCpu" :: Text), "severity" .= ("critical" :: Text)]
        , "annotations" .= object ["summary" .= ("cpu hot" :: Text)]
        ]

payloadWith :: [Aeson.Value] -> Aeson.Value
payloadWith alerts = object ["status" .= ("firing" :: Text), "alerts" .= alerts]

spec :: Spec
spec = describe "Application.Connector.Alertmanager (milestone 2 §8 hardening)" do
    it "multi-alert payloads produce one event per alert" do
        let result = normalize (payloadWith [alertObj "a" "firing", alertObj "b" "firing", alertObj "c" "firing"])
        fmap (map fingerprint) result `shouldBe` Right ["alertmanager:a", "alertmanager:b", "alertmanager:c"]

    it "resolved status with missing endsAt stays firing" do
        let result = normalize (payloadWith [alertObj "a" "resolved"])
        fmap (map status) result `shouldBe` Right [Firing]

    it "resolved status with zero endsAt stays firing" do
        let withZeroEnd =
                object
                    [ "status" .= ("resolved" :: Text)
                    , "fingerprint" .= ("a" :: Text)
                    , "endsAt" .= ("0001-01-01T00:00:00Z" :: Text)
                    , "labels" .= object ["alertname" .= ("HighCpu" :: Text)]
                    , "annotations" .= object []
                    ]
        fmap (map status) (normalize (payloadWith [withZeroEnd])) `shouldBe` Right [Firing]

    it "resolved status with a real endsAt resolves" do
        let withEnd =
                object
                    [ "status" .= ("resolved" :: Text)
                    , "fingerprint" .= ("a" :: Text)
                    , "endsAt" .= ("2026-09-04T10:00:00Z" :: Text)
                    , "labels" .= object ["alertname" .= ("HighCpu" :: Text)]
                    , "annotations" .= object []
                    ]
        fmap (map status) (normalize (payloadWith [withEnd])) `shouldBe` Right [Resolved]

    it "source_url falls back to payload externalURL" do
        let payload =
                object
                    [ "status" .= ("firing" :: Text)
                    , "externalURL" .= ("http://am:9093" :: Text)
                    , "alerts" .= [alertObj "a" "firing"]
                    ]
        fmap (map sourceUrl) (normalize payload) `shouldBe` Right [Just ("http://am:9093" :: Text)]

    it "generatorURL wins over externalURL" do
        let withGenerator =
                object
                    [ "status" .= ("firing" :: Text)
                    , "fingerprint" .= ("a" :: Text)
                    , "generatorURL" .= ("http://graf:3001/alerting/1" :: Text)
                    , "labels" .= object ["alertname" .= ("HighCpu" :: Text)]
                    , "annotations" .= object []
                    ]
        let payload =
                object
                    [ "status" .= ("firing" :: Text)
                    , "externalURL" .= ("http://am:9093" :: Text)
                    , "alerts" .= [withGenerator]
                    ]
        fmap (map sourceUrl) (normalize payload) `shouldBe` Right [Just ("http://graf:3001/alerting/1" :: Text)]
