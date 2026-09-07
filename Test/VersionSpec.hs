module Test.VersionSpec where

import Test.Hspec
import IHP.Prelude
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Application.Version (appVersion)

spec :: Spec
spec = describe "Application.Version" do
    it "matches the version field in Halemans.cabal" do
        cabalFile <- TextIO.readFile "Halemans.cabal"
        let cabalVersions = [Text.dropWhile (== ' ') (Text.drop 8 l) | l <- Text.lines cabalFile, Text.take 8 l == "version:"]
        cabalVersions `shouldBe` [appVersion]
