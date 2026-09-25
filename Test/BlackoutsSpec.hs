module Test.BlackoutsSpec where

import Application.Pipeline.Blackouts
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Generated.Types
import IHP.ModelSupport (newRecord, textToId)
import IHP.Prelude
import Test.Hspec

utcTime :: String -> UTCTime
utcTime s = fromMaybe (error ("bad timestamp: " <> cs s)) (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" s)

mkEnvId :: Text -> Id Environment
mkEnvId = textToId @"environments"

mkHostId :: Text -> Id Host
mkHostId = textToId @"hosts"

mkServiceId :: Text -> Id Service
mkServiceId = textToId @"services"

spec :: Spec
spec = describe "Application.Pipeline.Blackouts" do
    let now = utcTime "2026-09-04 10:00:00"
    let blackout start end =
            newRecord @Blackout
                |> set #startsAt start
                |> set #endsAt end
    let window = blackout (utcTime "2026-09-04 09:00:00") (utcTime "2026-09-04 11:00:00")
    let emptySubject =
            BlackoutSubject
                { subjectEnvironmentId = Nothing
                , subjectEnvironmentName = Nothing
                , subjectHostId = Nothing
                , subjectHostName = Nothing
                , subjectServiceId = Nothing
                , subjectServiceName = Nothing
                , subjectTitle = Nothing
                }

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
        it "matches on environment scope" do
            let theEnvId = mkEnvId "2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0001"
            let scoped = window |> set #environmentId (Just theEnvId)
            blackoutApplies now emptySubject{subjectEnvironmentId = Just theEnvId} scoped `shouldBe` True
            blackoutApplies now emptySubject scoped `shouldBe` False
        it "matches on host scope" do
            let theHostId = mkHostId "2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0002"
            let scoped = window |> set #hostId (Just theHostId)
            blackoutApplies now emptySubject{subjectHostId = Just theHostId} scoped `shouldBe` True
        it "matches on service scope" do
            let theServiceId = mkServiceId "2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0004"
            let scoped = window |> set #serviceId (Just theServiceId)
            blackoutApplies now emptySubject{subjectServiceId = Just theServiceId} scoped `shouldBe` True
        it "does not match a different scope value" do
            let theHostId = mkHostId "2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0002"
            let otherHostId = mkHostId "2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0003"
            let scoped = window |> set #hostId (Just theHostId)
            blackoutApplies now emptySubject{subjectHostId = Just otherHostId} scoped `shouldBe` False
        it "multiple legs AND together: env+host matches only when both match" do
            let theEnvId = mkEnvId "2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0001"
            let theHostId = mkHostId "2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0002"
            let otherHostId = mkHostId "2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0003"
            let scoped = window |> set #environmentId (Just theEnvId) |> set #hostId (Just theHostId)
            blackoutApplies now emptySubject{subjectEnvironmentId = Just theEnvId, subjectHostId = Just theHostId} scoped `shouldBe` True
            blackoutApplies now emptySubject{subjectEnvironmentId = Just theEnvId, subjectHostId = Just otherHostId} scoped `shouldBe` False
            blackoutApplies now emptySubject{subjectHostId = Just theHostId} scoped `shouldBe` False
        it "matches a host glob against the raw host name" do
            let scoped = window |> set #hostGlob (Just "9db-affijet*")
            blackoutApplies now emptySubject{subjectHostName = Just "9db-affijet01m"} scoped `shouldBe` True
            blackoutApplies now emptySubject{subjectHostName = Just "9db-web01"} scoped `shouldBe` False
        it "matches a ? glob against a single character" do
            let scoped = window |> set #serviceGlob (Just "db-?")
            blackoutApplies now emptySubject{subjectServiceName = Just "db-1"} scoped `shouldBe` True
            blackoutApplies now emptySubject{subjectServiceName = Just "db-12"} scoped `shouldBe` False
        it "matches a title glob against the alert title" do
            let scoped = window |> set #titleGlob (Just "test memory leak*")
            blackoutApplies now emptySubject{subjectTitle = Just "test memory leak on db-1"} scoped `shouldBe` True
            blackoutApplies now emptySubject{subjectTitle = Just "disk full on db-1"} scoped `shouldBe` False
        it "title glob without wildcards matches exactly" do
            let scoped = window |> set #titleGlob (Just "cpu usage high")
            blackoutApplies now emptySubject{subjectTitle = Just "cpu usage high"} scoped `shouldBe` True
            blackoutApplies now emptySubject{subjectTitle = Just "cpu usage high on web"} scoped `shouldBe` False
        it "title glob leg ANDs with other legs" do
            let scoped = window |> set #hostGlob (Just "9db-*") |> set #titleGlob (Just "test memory leak*")
            blackoutApplies now emptySubject{subjectHostName = Just "9db-web01", subjectTitle = Just "test memory leak on web"} scoped `shouldBe` True
            blackoutApplies now emptySubject{subjectHostName = Just "9db-web01", subjectTitle = Just "disk full"} scoped `shouldBe` False
            blackoutApplies now emptySubject{subjectHostName = Just "10db-web01", subjectTitle = Just "test memory leak on web"} scoped `shouldBe` False
        it "glob leg ANDs with an env leg" do
            let theEnvId = mkEnvId "2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0001"
            let otherEnvId = mkEnvId "2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0005"
            let scoped = window |> set #environmentId (Just theEnvId) |> set #hostGlob (Just "9db-affijet*")
            blackoutApplies now emptySubject{subjectEnvironmentId = Just theEnvId, subjectHostName = Just "9db-affijet01m"} scoped `shouldBe` True
            blackoutApplies now emptySubject{subjectEnvironmentId = Just otherEnvId, subjectHostName = Just "9db-affijet01m"} scoped `shouldBe` False
        it "glob leg fails when the alert has no such name" do
            let scoped = window |> set #hostGlob (Just "*")
            blackoutApplies now emptySubject scoped `shouldBe` False
            blackoutApplies now emptySubject{subjectHostName = Nothing} scoped `shouldBe` False
        it "scopeless blackout matches nothing" do
            blackoutApplies now emptySubject{subjectEnvironmentId = Just (mkEnvId "2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0001")} window `shouldBe` False
        it "expired window never matches" do
            let theEnvId = mkEnvId "2b1b0e4d-3a0f-4a6e-9a3c-4c7f3b8a0001"
            let scoped =
                    blackout (utcTime "2026-09-04 06:00:00") (utcTime "2026-09-04 07:00:00")
                        |> set #environmentId (Just theEnvId)
            blackoutApplies now emptySubject{subjectEnvironmentId = Just theEnvId} scoped `shouldBe` False
