module Test.PollZabbixSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport (newRecord)
import Generated.Types
import Application.Job.PollZabbix
    ( initialCursor, initialHistoryDays
    , reconcileResolvedEnabled, reconcileGraceSeconds, reconcileIntervalSeconds
    , absentResolveMinAgeSeconds, eventPageLimit, reconcileDue
    , resolveDecision
    )
import Application.Connector.Zabbix (ZabbixTriggerState (..))
import Data.Aeson (object, (.=))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

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
        it "leaves alerts with fresh local activity alone even when the source reports OK" do
            let alert = firingAlert |> set #lastSeenAt (addUTCTime (-30) now)
            resolveDecision now (newRecord @Source) alert (Just okTrigger) `shouldBe` Nothing
        it "leaves triggers still in problem state alone" do
            resolveDecision now (newRecord @Source) firingAlert (Just problemTrigger) `shouldBe` Nothing
        it "resolves at the zabbix-side state-change time" do
            resolveDecision now (newRecord @Source) firingAlert (Just okTrigger)
                `shouldBe` Just (addUTCTime (-600) now)
        it "caps a future state-change time at now" do
            let future = okTrigger { triggerStateLastChange = nowPosix + 600 }
            resolveDecision now (newRecord @Source) firingAlert (Just future) `shouldBe` Just now
        it "resolves at now when the trigger carries no lastchange" do
            let noClock = okTrigger { triggerStateLastChange = 0 }
            resolveDecision now (newRecord @Source) firingAlert (Just noClock) `shouldBe` Just now
        it "leaves a young alert whose trigger is missing alone (permission-gap guard)" do
            let young = firingAlert |> set #startedAt (Just (addUTCTime (-3600) now))
            resolveDecision now (newRecord @Source) young Nothing `shouldBe` Nothing
        it "resolves an old alert whose trigger is missing (deleted trigger)" do
            resolveDecision now (newRecord @Source) firingAlert Nothing `shouldBe` Just now
        it "honors reconcileGraceSeconds overrides" do
            let source = newRecord @Source |> set #config (object ["reconcileGraceSeconds" .= (10 :: Int)])
                alert = firingAlert |> set #lastSeenAt (addUTCTime (-30) now)
            resolveDecision now source alert (Just okTrigger) `shouldBe` Just (addUTCTime (-600) now)
  where
    now = UTCTime (fromGregorian 2026 6 1) 43200
    nowPosix = floor (utcTimeToPOSIXSeconds now) :: Integer
    firingAlert = newRecord @Alert
        |> set #fingerprint "zabbix:trigger:42"
        |> set #status "firing"
        |> set #lastSeenAt (addUTCTime (-3600) now)
        |> set #startedAt (Just (addUTCTime (-90000) now))
    problemTrigger = ZabbixTriggerState
        { triggerStateId = "42"
        , triggerStateValue = "1"
        , triggerStateLastChange = nowPosix - 600
        }
    okTrigger = problemTrigger { triggerStateValue = "0" }
