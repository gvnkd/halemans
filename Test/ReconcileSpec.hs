module Test.ReconcileSpec where

import Application.Service.Reconcile
import IHP.Prelude
import Test.Helpers (atTime)
import Test.Hspec

spec :: Spec
spec = describe "Application.Service.Reconcile" do
    describe "shouldMirror" do
        let older = atTime "2026-09-04 10:00:00 UTC"
            newer = atTime "2026-09-04 12:00:00 UTC"
        it "mirrors when there is no local action yet" do
            shouldMirror Nothing older `shouldBe` True
        it "mirrors source state newer than the last local action" do
            shouldMirror (Just older) newer `shouldBe` True
        it "never clobbers a newer local action with older source state" do
            shouldMirror (Just newer) older `shouldBe` False
        it "ties do not mirror" do
            shouldMirror (Just older) older `shouldBe` False
