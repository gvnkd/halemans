module Test.BlackoutsSpec where

import Application.Pipeline.Blackouts
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Generated.Types
import IHP.ModelSupport (newRecord, textToId)
import IHP.Prelude
import Test.Hspec

utcTime :: String -> UTCTime
utcTime s = fromMaybe (error ("bad timestamp: " <> cs s)) (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" s)

spec :: Spec
spec = describe "Application.Pipeline.Blackouts" do
    let now = utcTime "2026-09-04 10:00:00"
    let blackout start end =
            newRecord @Blackout
                |> set #startsAt start
                |> set #endsAt end

    describe "blackoutWindowActive" do
        it "is active inside the window" do
            blackoutWindowActive now (blackout (utcTime "2026-09-04 09:00:00") (utcTime "2026-09-04 11:00:00")) `shouldBe` True
        it "is inactive before the window" do
            blackoutWindowActive now (blackout (utcTime "2026-09-04 10:00:01") (utcTime "2026-09-04 11:00:00")) `shouldBe` False
        it "is inactive after the window" do
            blackoutWindowActive now (blackout (utcTime "2026-09-04 08:00:00") (utcTime "2026-09-04 09:59:59")) `shouldBe` False
        it "boundary: starts_at inclusive, ends_at exclusive" do
            blackoutWindowActive (utcTime "2026-09-04 09:00:00") (blackout (utcTime "2026-09-04 09:00:00") (utcTime "2026-09-04 11:00:00")) `shouldBe` True
            blackoutWindowActive (utcTime "2026-09-04 11:00:00") (blackout (utcTime "2026-09-04 09:00:00") (utcTime "2026-09-04 11:00:00")) `shouldBe` False

    describe "blackoutApplies" do
        let window = blackout (utcTime "2026-09-04 09:00:00") (utcTime "2026-09-04 11:00:00")
        it "matches on environment scope" do
            let envId = (textToId @"environments" ("2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0001" :: Text) :: Id Environment)
            let scoped = window |> set #environmentId (Just envId)
            blackoutApplies now (Just envId) Nothing Nothing scoped `shouldBe` True
            blackoutApplies now Nothing Nothing Nothing scoped `shouldBe` False
        it "matches on host scope" do
            let hostId = (textToId @"hosts" ("2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0002" :: Text) :: Id Host)
            let scoped = window |> set #hostId (Just hostId)
            blackoutApplies now Nothing (Just hostId) Nothing scoped `shouldBe` True
        it "does not match a different scope value" do
            let hostId = (textToId @"hosts" ("2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0002" :: Text) :: Id Host)
            let otherHostId = (textToId @"hosts" ("2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0003" :: Text) :: Id Host)
            let scoped = window |> set #hostId (Just hostId)
            blackoutApplies now Nothing (Just otherHostId) Nothing scoped `shouldBe` False
        it "expired window never matches" do
            let envId = (textToId @"environments" ("2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0001" :: Text) :: Id Environment)
            let scoped =
                    blackout (utcTime "2026-09-04 06:00:00") (utcTime "2026-09-04 07:00:00")
                        |> set #environmentId (Just envId)
            blackoutApplies now (Just envId) Nothing Nothing scoped `shouldBe` False
