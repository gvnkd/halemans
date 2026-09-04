module Test.EscalationSpec where

import Test.Hspec
import IHP.Prelude
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Application.Pipeline.Escalation

utcTime :: String -> UTCTime
utcTime s = fromMaybe (error ("bad timestamp: " <> cs s)) (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" s)

step0 :: EscalationStep
step0 = EscalationStep 300 (Just "team-a") Nothing Nothing

step1 :: EscalationStep
step1 = EscalationStep 600 Nothing (Just "user-b") (Just "ack")

steps :: [EscalationStep]
steps = [step0, step1]

spec :: Spec
spec = describe "Application.Pipeline.Escalation" do
    describe "stepsFromJSON" do
        it "parses steps with team/user/unless_status" do
            let json = Aeson.toJSON
                    [ object ["after_seconds" .= (300 :: Int), "target_team_id" .= ("team-a" :: Text)]
                    , object ["after_seconds" .= (600 :: Int), "target_user_id" .= ("user-b" :: Text), "unless_status" .= ("ack" :: Text)]
                    ]
            stepsFromJSON json `shouldBe` steps
        it "drops malformed entries" do
            let json = Aeson.toJSON [object ["nope" .= (1 :: Int)], object ["after_seconds" .= (60 :: Int)]]
            stepsFromJSON json `shouldBe` [EscalationStep 60 Nothing Nothing Nothing]
        it "non-array gives no steps" do
            stepsFromJSON (object []) `shouldBe` []

    describe "stepDeadline" do
        it "adds after_seconds to the base" do
            stepDeadline (utcTime "2026-09-04 10:00:00") step0 `shouldBe` utcTime "2026-09-04 10:05:00"

    describe "decideDueTracker" do
        let now = utcTime "2026-09-04 10:00:00"
        it "notifies the current step and advances to the next deadline" do
            decideDueTracker now steps 0 "firing" False
                `shouldBe` EscalateNotify step0 (AdvanceTo 1 (utcTime "2026-09-04 10:10:00"))
        it "last step notifies and marks done" do
            decideDueTracker now steps 1 "firing" False
                `shouldBe` EscalateNotify step1 MarkDone
        it "acked alert cancels" do
            decideDueTracker now steps 0 "ack" False `shouldBe` EscalateCancel
        it "resolved alert cancels" do
            decideDueTracker now steps 0 "resolved" False `shouldBe` EscalateCancel
        it "suppressed alert cancels" do
            decideDueTracker now steps 0 "firing" True `shouldBe` EscalateCancel
        it "unless_status match cancels" do
            decideDueTracker now steps 1 "ack" False `shouldBe` EscalateCancel
        it "step index past the end cancels" do
            decideDueTracker now steps 5 "firing" False `shouldBe` EscalateCancel
