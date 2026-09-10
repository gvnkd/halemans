module Test.PollZabbixSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport (newRecord)
import Generated.Types
import Application.Job.PollZabbix
    ( initialCursor, initialHistoryDays
    , reconcileResolvedEnabled, reconcileGraceSeconds, reconcileIntervalSeconds
    , absentResolveMinAgeSeconds, eventPageLimit, reconcileDue
    , resolveDecision, latestProblemByTrigger
    )
import Application.Connector.Zabbix (ZabbixProblemState (..))
import Data.Aeson (object, (.=))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "Application.Job.PollZabbix" do
    describe "initialHistoryDays" do
        it "defaults to 1 when the key is absent" do
            initialHistoryDays (newRecord @Source) `shouldBe` 1
        it "reads initialHistoryDays from source config" do
            let source = newRecord @Source |> set #config (object ["initialHistoryDays" .= (7 :: Int)])
            initialHistoryDays source `shouldBe` 7

    describe "initialCursor" do
        it "uses lastSyncCursor when present" do
            let cursorTime = addUTCTime (-3600) now
                source = newRecord @Source |> set #lastSyncCursor (Just cursorTime)
            initialCursor now source `shouldBe` floor (utcTimeToPOSIXSeconds cursorTime)
        it "bounds the first poll to one day back by default" do
            initialCursor now (newRecord @Source) `shouldBe` nowPosix - 86400
        it "honors initialHistoryDays on the first poll" do
            let source = newRecord @Source |> set #config (object ["initialHistoryDays" .= (30 :: Int)])
            initialCursor now source `shouldBe` nowPosix - 30 * 86400
        it "ignores initialHistoryDays once a cursor exists" do
            let cursorTime = addUTCTime (-3600) now
                source = newRecord @Source
                    |> set #lastSyncCursor (Just cursorTime)
                    |> set #config (object ["initialHistoryDays" .= (30 :: Int)])
            initialCursor now source `shouldBe` floor (utcTimeToPOSIXSeconds cursorTime)

    describe "reconcile config accessors" do
        it "applies documented defaults when keys are absent" do
            let source = newRecord @Source
            reconcileResolvedEnabled source `shouldBe` True
            reconcileGraceSeconds source `shouldBe` 60
            reconcileIntervalSeconds source `shouldBe` 0
            absentResolveMinAgeSeconds source `shouldBe` 86400
            eventPageLimit source `shouldBe` 1000
        it "reads overrides from source config" do
            let source = newRecord @Source |> set #config (object
                    [ "reconcileResolved" .= False
                    , "reconcileGraceSeconds" .= (120 :: Int)
                    , "reconcileIntervalSeconds" .= (300 :: Int)
                    , "absentResolveMinAgeSeconds" .= (3600 :: Int)
                    , "eventPageLimit" .= (50 :: Int)
                    ])
            reconcileResolvedEnabled source `shouldBe` False
            reconcileGraceSeconds source `shouldBe` 120
            reconcileIntervalSeconds source `shouldBe` 300
            absentResolveMinAgeSeconds source `shouldBe` 3600
            eventPageLimit source `shouldBe` 50
        it "clamps eventPageLimit to at least 1" do
            let source = newRecord @Source |> set #config (object ["eventPageLimit" .= (0 :: Int)])
            eventPageLimit source `shouldBe` 1

    describe "reconcileDue" do
        it "is due when the source never reconciled" do
            reconcileDue now (newRecord @Source) `shouldBe` True
        it "is due every cycle with the default interval" do
            let source = newRecord @Source |> set #lastReconcileAt (Just now)
            reconcileDue now source `shouldBe` True
        it "is not due inside the configured interval" do
            let source = newRecord @Source
                    |> set #lastReconcileAt (Just (addUTCTime (-100) now))
                    |> set #config (object ["reconcileIntervalSeconds" .= (300 :: Int)])
            reconcileDue now source `shouldBe` False
        it "is due once the interval passed" do
            let source = newRecord @Source
                    |> set #lastReconcileAt (Just (addUTCTime (-301) now))
                    |> set #config (object ["reconcileIntervalSeconds" .= (300 :: Int)])
            reconcileDue now source `shouldBe` True

    describe "resolveDecision" do
        it "leaves alerts with fresh local activity alone even when the source reports resolved" do
            let alert = firingAlert |> set #lastSeenAt (addUTCTime (-30) now)
            resolveDecision now (newRecord @Source) alert (Just resolvedProblem) `shouldBe` Nothing
        it "leaves still-open problems alone" do
            resolveDecision now (newRecord @Source) firingAlert (Just openProblem) `shouldBe` Nothing
        it "resolves at the zabbix-side resolution time" do
            resolveDecision now (newRecord @Source) firingAlert (Just resolvedProblem)
                `shouldBe` Just (addUTCTime (-600) now)
        it "caps a future resolution time at now" do
            let future = resolvedProblem { problemRClock = nowPosix + 600 }
            resolveDecision now (newRecord @Source) firingAlert (Just future) `shouldBe` Just now
        it "leaves a young alert with no problem rows alone (permission-gap guard)" do
            let young = firingAlert |> set #startedAt (Just (addUTCTime (-3600) now))
            resolveDecision now (newRecord @Source) young Nothing `shouldBe` Nothing
        it "resolves an old alert with no problem rows (purged or deleted trigger)" do
            resolveDecision now (newRecord @Source) firingAlert Nothing `shouldBe` Just now
        it "honors reconcileGraceSeconds overrides" do
            let source = newRecord @Source |> set #config (object ["reconcileGraceSeconds" .= (10 :: Int)])
                alert = firingAlert |> set #lastSeenAt (addUTCTime (-30) now)
            resolveDecision now source alert (Just resolvedProblem) `shouldBe` Just (addUTCTime (-600) now)

    describe "latestProblemByTrigger" do
        it "keeps the newest problem row per trigger" do
            let older = openProblem { problemEventId = "10", problemClock = nowPosix - 700 }
                newer = resolvedProblem { problemEventId = "11", problemClock = nowPosix - 600 }
            Map.lookup "42" (latestProblemByTrigger [older, newer]) `shouldBe` Just newer
            Map.lookup "42" (latestProblemByTrigger [newer, older]) `shouldBe` Just newer
  where
    now = UTCTime (fromGregorian 2026 6 1) 43200
    nowPosix = floor (utcTimeToPOSIXSeconds now) :: Integer
    firingAlert = newRecord @Alert
        |> set #fingerprint "zabbix:trigger:42"
        |> set #status "firing"
        |> set #lastSeenAt (addUTCTime (-3600) now)
        |> set #startedAt (Just (addUTCTime (-90000) now))
    openProblem = ZabbixProblemState
        { problemEventId = "11"
        , problemTriggerId = "42"
        , problemClock = nowPosix - 600
        , problemREventId = "0"
        , problemRClock = 0
        }
    resolvedProblem = openProblem { problemREventId = "12", problemRClock = nowPosix - 600 }
