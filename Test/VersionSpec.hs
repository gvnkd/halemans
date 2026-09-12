module Test.VersionSpec where

import Application.Version (appVersion)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Application.Version" do
    it "matches the version field in Halemans.cabal" do
        cabalFile <- TextIO.readFile "Halemans.cabal"
        let cabalVersions = [Text.dropWhile (== ' ') (Text.drop 8 l) | l <- Text.lines cabalFile, Text.take 8 l == "version:"]
        cabalVersions `shouldBe` [appVersion]
