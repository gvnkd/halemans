module Test.FilterPrefsSpec where

import Test.Hspec
import IHP.Prelude
import Data.Aeson (object, (.=), toJSON)
import Application.Helper.FilterPrefs (filterPrefsFor)
import Application.Service.AlertList
import Web.View.Environments.Show (EnvFilters (..), emptyEnvFilters, envFiltersToValue, envFiltersFromValue, envPrefsAreDefault)

spec :: Spec
spec = describe "filter prefs" do
    describe "filterPrefsFor" do
        it "reads a nested filters key" do
            let settings = object ["theme" .= ("dark" :: Text), "filters" .= object ["alerts" .= object ["q" .= ("disk" :: Text)]]]
            filterPrefsFor settings "alerts" `shouldBe` Just (object ["q" .= ("disk" :: Text)])
        it "returns Nothing without a filters object" do
            filterPrefsFor (object ["theme" .= ("dark" :: Text)]) "alerts" `shouldBe` Nothing
        it "returns Nothing for an unknown page key" do
            let settings = object ["filters" .= object ["env" .= object []]]
            filterPrefsFor settings "alerts" `shouldBe` Nothing

    describe "alertFilters json roundtrip" do
        it "roundtrips a fully populated record" do
            let filters = AlertListFilters
                    { alfSeverities = ["critical", "high"]
                    , alfStatuses = ["firing"]
                    , alfEnvs = ["prod"]
                    , alfHost = Just "web-01"
                    , alfService = Just "nginx"
                    , alfTitle = Just "disk"
                    , alfGroup = Just "grp"
                    , alfSort = "severity"
                    , alfDir = "asc"
                    }
            alertFiltersFromValue (alertFiltersToValue filters) `shouldBe` Just filters
        it "roundtrips the defaults" do
            alertFiltersFromValue (alertFiltersToValue defaultAlertListFilters) `shouldBe` Just defaultAlertListFilters
        it "falls back to the default sort on an unknown column" do
            let stored = object ["sort" .= ("bogus" :: Text), "dir" .= ("asc" :: Text)]
            alertFiltersFromValue stored `shouldBe` Just defaultAlertListFilters { alfDir = "asc" }
        it "rejects non-objects" do
            alertFiltersFromValue (toJSON ("nope" :: Text)) `shouldBe` Nothing

    describe "envFilters json roundtrip" do
        it "roundtrips filters and view mode" do
            let filters = EnvFilters
                    { filterSeverities = ["warning"]
                    , filterStatuses = []
                    , filterHost = Just "db-01"
                    , filterService = Nothing
                    , filterText = Just "cpu"
                    , filterGroup = Nothing
                    }
            envFiltersFromValue (envFiltersToValue filters "grouped") `shouldBe` Just (filters, "grouped")
        it "treats empty filters with flat view as default" do
            envPrefsAreDefault (emptyEnvFilters, "flat") `shouldBe` True
            envPrefsAreDefault (emptyEnvFilters, "grouped") `shouldBe` False
        it "sanitizes an unknown view mode to flat" do
            let stored = object ["view" .= ("bogus" :: Text)]
            envFiltersFromValue stored `shouldBe` Just (emptyEnvFilters, "flat")
