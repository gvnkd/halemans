module Test.TimeRangeSpec where

import Application.Service.TimeRange (resolveTimeExpr)
import Data.Time
import IHP.Prelude
import Test.Hspec

now :: UTCTime
now = UTCTime (fromGregorian 2026 9 13) 43200 -- 2026-09-13 12:00:00 UTC

spec :: Spec
spec = describe "Application.Service.TimeRange" do
    describe "relative expressions" do
        it "resolves bare now()" do
            resolveTimeExpr now "now()" `shouldBe` Just now

        it "resolves day offsets" do
            resolveTimeExpr now "now() - 7d" `shouldBe` Just (UTCTime (fromGregorian 2026 9 6) 43200)

        it "resolves hour offsets without spaces" do
            resolveTimeExpr now "now()-12h" `shouldBe` Just (UTCTime (fromGregorian 2026 9 13) 0)

        it "resolves minute, second and week offsets" do
            resolveTimeExpr now "now() - 30m" `shouldBe` Just (UTCTime (fromGregorian 2026 9 13) 41400)
            resolveTimeExpr now "now() - 90s" `shouldBe` Just (UTCTime (fromGregorian 2026 9 13) 43110)
            resolveTimeExpr now "now() - 2w" `shouldBe` Just (UTCTime (fromGregorian 2026 8 30) 43200)

        it "resolves positive offsets" do
            resolveTimeExpr now "now() + 1h" `shouldBe` Just (UTCTime (fromGregorian 2026 9 13) 46800)

        it "is case-insensitive and whitespace-tolerant" do
            resolveTimeExpr now "  NOW()  -  3D " `shouldBe` Just (UTCTime (fromGregorian 2026 9 10) 43200)

        it "rejects unknown units and garbage" do
            resolveTimeExpr now "now() - 7x" `shouldBe` Nothing
            resolveTimeExpr now "now() - d" `shouldBe` Nothing
            resolveTimeExpr now "yesterday" `shouldBe` Nothing

    describe "absolute timestamps" do
        it "parses ISO with Z" do
            resolveTimeExpr now "2026-09-01T10:30:00Z" `shouldBe` Just (UTCTime (fromGregorian 2026 9 1) 37800)

        it "parses ISO with fractional seconds" do
            resolveTimeExpr now "2026-09-01T10:30:00.123Z" `shouldBe` Just (UTCTime (fromGregorian 2026 9 1) 37800.123)

        it "parses ISO with a zone offset" do
            resolveTimeExpr now "2026-09-01T10:30:00+04:00" `shouldBe` Just (UTCTime (fromGregorian 2026 9 1) 23400)

        it "parses zone-less datetimes as UTC" do
            resolveTimeExpr now "2026-09-01 10:30" `shouldBe` Just (UTCTime (fromGregorian 2026 9 1) 37800)

        it "parses bare dates as midnight UTC" do
            resolveTimeExpr now "2026-09-01" `shouldBe` Just (UTCTime (fromGregorian 2026 9 1) 0)
