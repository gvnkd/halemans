module Test.DashboardConfigSpec where

import Test.Hspec
import IHP.Prelude
import qualified Data.Aeson as Aeson
import Application.Helper.DashboardConfig

spec :: Spec
spec = describe "Application.Helper.DashboardConfig" do
    let cards =
            [ DashboardCard "dev" ["firing"] ["critical", "high"]
            , DashboardCard "prod" [] []
            ]
    it "round-trips through JSON" do
        decodeDashboardConfig (encodeDashboardConfig cards) `shouldBe` Right cards
    it "defaults missing filters to empty lists" do
        decodeDashboardConfig (Aeson.toJSON [Aeson.object ["env" Aeson..= ("dev" :: Text)]])
            `shouldBe` Right [DashboardCard "dev" [] []]
    it "rejects non-card JSON" do
        decodeDashboardConfig (Aeson.toJSON (42 :: Int)) `shouldSatisfy` isLeft
    it "rejects cards without env" do
        decodeDashboardConfig (Aeson.toJSON [Aeson.object ["filters" Aeson..= Aeson.object []]])
            `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
