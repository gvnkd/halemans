module Test.ApiSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport (newRecord)
import Generated.Types
import Application.Service.Api.Cursor (Cursor (..), encodeCursor, decodeCursor)
import Application.Service.Api.Token (hashToken)
import Application.Service.Api.RateLimit (Bucket (..), emptyBucket, allowRequest)
import Application.Service.Api.Metrics (MetricSample (..), renderMetrics, renderFamily, escapeLabel)
import Application.Service.Api.Encode (encodeAlertSummary, encodeEnvCard)
import Web.View.Dashboard.Index (EnvCard (..))
import qualified Data.UUID as UUID
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Maybe (fromJust)

spec :: Spec
spec = describe "Milestone 6 API" do
    describe "cursor" do
        it "round-trips through encode/decode" do
            let cursor = Cursor (UTCTime (fromGregorian 2026 1 2) 3661.123456789012) testUuid
            decodeCursor (encodeCursor cursor) `shouldBe` Just cursor
        it "rejects garbage" do
            decodeCursor "not-a-cursor" `shouldBe` Nothing
            decodeCursor "" `shouldBe` Nothing
        it "produces url-safe opaque output" do
            let encoded = encodeCursor (Cursor (UTCTime (fromGregorian 2026 1 2) 0) testUuid)
            encoded `shouldSatisfy` (\t -> all (`elem` (['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> "-_")) (cs t :: String))

    describe "hashToken" do
        it "matches the reference sha256 hex" do
            hashToken "halemans-smoke" `shouldBe` sha256Of "halemans-smoke"
        it "is stable" do
            hashToken "abc" `shouldBe` hashToken "abc"

    describe "rate limiter" do
        it "allows up to the capacity immediately" do
            let bucket = Bucket 6 t0
                (allowed, _, _) = allowRequest 6 t0 bucket
            allowed `shouldBe` True
        it "denies when the bucket is empty and reports Retry-After" do
            let bucket = Bucket 0 t0
                (allowed, retryAfter, _) = allowRequest 6 t0 bucket
            allowed `shouldBe` False
            retryAfter `shouldBe` 10
        it "refills over time" do
            let bucket = Bucket 0 t0
                later = addUTCTime 20 t0
                (allowed, _, bucket') = allowRequest 6 later bucket
            allowed `shouldBe` True
            bucket'.bucketTokens `shouldBe` 1
        it "caps at the per-minute capacity" do
            let bucket = Bucket 6 t0
                later = addUTCTime 3600 t0
                (_, _, bucket') = allowRequest 120 later bucket
            bucket'.bucketTokens `shouldBe` 119

    describe "metrics rendering" do
        it "renders samples with labels" do
            renderMetrics [MetricSample "halemans_ws_connections" [] "3"]
                `shouldBe` "halemans_ws_connections 3\n"
            renderMetrics [MetricSample "halemans_alerts" [("environment", "prod"), ("status", "firing")] "2"]
                `shouldBe` "halemans_alerts{environment=\"prod\",status=\"firing\"} 2\n"
        it "emits a TYPE header per family" do
            renderFamily "halemans_build_info" [MetricSample "halemans_build_info" [("version", "1.1.0")] "1"]
                `shouldBe` "# TYPE halemans_build_info gauge\nhalemans_build_info{version=\"1.1.0\"} 1\n"
        it "escapes label values" do
            escapeLabel "a\"b\\c\nd" `shouldBe` "a\\\"b\\\\c\\nd"

    describe "encoders" do
        it "alert summary uses snake_case keys and ISO-8601 timestamps" do
            let json = encodeAlertSummary testAlert
            forM_ ["fingerprint", "last_seen_at", "check_name", "source_url", "group_id"] \key ->
                keys json `shouldContain` [key]
            lookupKey "last_seen_at" json `shouldBe` Just (String "2026-01-02T03:04:05Z")
        it "alert summary renders null for absent refs" do
            let json = encodeAlertSummary testAlert
            lookupKey "group_id" json `shouldBe` Just Null
            lookupKey "host" json `shouldBe` Just Null
        it "env card rollup matches the dashboard shape" do
            let json = encodeEnvCard EnvCard
                    { cardEnvName = Nothing
                    , cardEnvironment = Nothing
                    , cardFiring = 2
                    , cardAcked = 1
                    , cardResolved = 3
                    , cardSuppressed = 1
                    , cardWorstSeverity = Just "critical"
                    , cardHourly = []
                    }
            lookupKey "worst_severity" json `shouldBe` Just (String "critical")
            lookupKey "environment" json `shouldBe` Just Null
            lookupKey "counts" json `shouldBe` Just (object ["firing" .= (2 :: Int), "ack" .= (1 :: Int), "resolved" .= (3 :: Int)])
  where
    t0 = UTCTime (fromGregorian 2026 1 1) 0
    testUuid = fromJust (UUID.fromText "12345678-1234-1234-1234-1234567890ab")
    testAlert = newRecord @Alert
        |> set #fingerprint "test:fingerprint"
        |> set #title "disk full"
        |> set #severity "critical"
        |> set #status "firing"
        |> set #lastSeenAt (UTCTime (fromGregorian 2026 1 2) 11045)
    keys :: Value -> [Text]
    keys (Object o) = map Key.toText (KeyMap.keys o)
    keys _ = []
    lookupKey :: Text -> Value -> Maybe Value
    lookupKey key (Object o) = KeyMap.lookup (Key.fromText key) o
    lookupKey _ _ = Nothing
    -- sha256 reference values (echo -n <input> | sha256sum)
    sha256Of "halemans-smoke" = "d01e3680115ea49bb0789424b6a806fcaea7680be304998c8b583adfdc0a9e00"
    sha256Of _ = ""
