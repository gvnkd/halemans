module Test.StateMachineSpec where

import Test.Hspec
import IHP.Prelude
import Application.Pipeline.StateMachine

spec :: Spec
spec = describe "Application.Pipeline.StateMachine" do
    describe "step (legal edges per 01_highlevel.md §5.2)" do
        it "firing --refire--> firing (repeated)" do
            step Firing Refire `shouldBe` Transition Firing Firing Refire "repeated" True
        it "firing --resolved--> resolved" do
            step Firing SourceResolved `shouldBe` Transition Firing Resolved SourceResolved "resolved" True
        it "firing --ack--> ack" do
            step Firing AckTrigger `shouldBe` Transition Firing Acked AckTrigger "ack" True
        it "ack --refire--> ack (repeated)" do
            step Acked Refire `shouldBe` Transition Acked Acked Refire "repeated" True
        it "ack --resolved--> resolved" do
            step Acked SourceResolved `shouldBe` Transition Acked Resolved SourceResolved "resolved" True
        it "ack --unack--> firing" do
            step Acked Unack `shouldBe` Transition Acked Firing Unack "unack" True
        it "ack --close--> closed" do
            step Acked CloseTrigger `shouldBe` Transition Acked Closed CloseTrigger "closed" True
        it "resolved --refire--> firing" do
            step Resolved Refire `shouldBe` Transition Resolved Firing Refire "repeated" True
        it "resolved --auto-close--> closed" do
            step Resolved AutoClose `shouldBe` Transition Resolved Closed AutoClose "closed" True
        it "firing --stall-timeout--> stalled" do
            step Firing StallTimeout `shouldBe` Transition Firing Stalled StallTimeout "stalled" True
        it "ack --stall-timeout--> stalled" do
            step Acked StallTimeout `shouldBe` Transition Acked Stalled StallTimeout "stalled" True
        it "stalled --refire--> firing (revive)" do
            step Stalled Refire `shouldBe` Transition Stalled Firing Refire "repeated" True
        it "stalled --resolved--> resolved" do
            step Stalled SourceResolved `shouldBe` Transition Stalled Resolved SourceResolved "resolved" True
        it "stalled --ack--> ack" do
            step Stalled AckTrigger `shouldBe` Transition Stalled Acked AckTrigger "ack" True
        it "stalled --close--> closed" do
            step Stalled CloseTrigger `shouldBe` Transition Stalled Closed CloseTrigger "closed" True
        it "stalled --auto-close--> closed" do
            step Stalled AutoClose `shouldBe` Transition Stalled Closed AutoClose "closed" True

    describe "step (illegal transitions are no-ops)" do
        it "never applies and keeps the state" do
            forEach [(state, trigger) | state <- [minBound..maxBound], trigger <- [minBound..maxBound]] \(state, trigger) -> do
                let transition = step state trigger
                unless (transition.applied) do
                    transition.to `shouldBe` state
                    transition.eventKind `shouldBe` "external"

    describe "properties (exhaustive sequences)" do
        let allStates = [minBound..maxBound] :: [AlertState]
        let allTriggers = [minBound..maxBound] :: [Trigger]
        let exactLength 0 = [[]]
            exactLength k = [t:ts | t <- allTriggers, ts <- exactLength (k - 1)]
        let sequencesUpTo n = concatMap exactLength [0..n]

        it "every state reachable from any trigger sequence is a valid AlertState" do
            forEach allStates \start ->
                forEach (sequencesUpTo 4) \triggers -> do
                    let transitions = runSequence start triggers
                    forEach transitions \transition ->
                        transition.to `shouldSatisfy` (`elem` allStates)

        it "applied transitions only ever use the legal edge set" do
            let legalEdges =
                    [ (Firing, Refire), (Firing, SourceResolved), (Firing, AckTrigger), (Firing, StallTimeout)
                    , (Acked, Refire), (Acked, SourceResolved), (Acked, Unack), (Acked, CloseTrigger), (Acked, StallTimeout)
                    , (Resolved, Refire), (Resolved, AutoClose)
                    , (Stalled, Refire), (Stalled, SourceResolved), (Stalled, AckTrigger), (Stalled, CloseTrigger), (Stalled, AutoClose)
                    ]
            forEach allStates \start ->
                forEach (sequencesUpTo 4) \triggers ->
                    forEach (runSequence start triggers) \transition ->
                        when transition.applied do
                            (transition.from, transition.trigger) `shouldSatisfy` (`elem` legalEdges)

        it "closed is terminal" do
            forEach (sequencesUpTo 4) \triggers -> do
                let transitions = runSequence Closed triggers
                forEach transitions \transition -> do
                    transition.applied `shouldBe` False
                    transition.to `shouldBe` Closed

        it "audit ordering: emitted events correspond 1:1 and in order to triggers" do
            forEach allStates \start ->
                forEach (sequencesUpTo 4) \triggers -> do
                    let transitions = runSequence start triggers
                    map (.trigger) transitions `shouldBe` triggers

    describe "text mapping" do
        it "round-trips" do
            forEach [minBound..maxBound] \state ->
                alertStateFromText (alertStateToText state) `shouldBe` Just state
        it "rejects unknown states" do
            alertStateFromText "bogus" `shouldBe` Nothing
