module Main where

import IHP.Prelude
import Test.Hspec

-- Import your test specs here:

import qualified Test.AlertmanagerSpec
import qualified Test.ApiSpec
import qualified Test.AssetsSpec
import qualified Test.AuditExportSpec
import qualified Test.BlackoutsSpec
import qualified Test.CmdbSpec
import qualified Test.DashboardConfigSpec
import qualified Test.EscalationSpec
import qualified Test.FacetsSpec
import qualified Test.FilterPrefsSpec
import qualified Test.FlappingSpec
import qualified Test.GroupingSpec
import qualified Test.HostGroupsSpec
import qualified Test.HttpSpec
import qualified Test.JiraSpec
import qualified Test.LiveSpec
import qualified Test.LlmSpec
import qualified Test.LogSpec
import qualified Test.MarkdownSpec
import qualified Test.PollZabbixSpec
import qualified Test.PrivilegeSpec
import qualified Test.ProvisionSpec
import qualified Test.PushSpec
import qualified Test.ReconcileSpec
import qualified Test.ReportsSpec
import qualified Test.SourceHealthSpec
import qualified Test.StateMachineSpec
import qualified Test.ThemeSpec
import qualified Test.TimeRangeSpec
import qualified Test.TimelineSpec
import qualified Test.VersionSpec
import qualified Test.WriteBackSpec

main :: IO ()
main = hspec do
    Test.StateMachineSpec.spec
    Test.BlackoutsSpec.spec
    Test.PushSpec.spec
    Test.GroupingSpec.spec
    Test.EscalationSpec.spec
    Test.AlertmanagerSpec.spec
    Test.CmdbSpec.spec
    Test.JiraSpec.spec
    Test.WriteBackSpec.spec
    Test.ReconcileSpec.spec
    Test.DashboardConfigSpec.spec
    Test.ThemeSpec.spec
    Test.LlmSpec.spec
    Test.SourceHealthSpec.spec
    Test.AuditExportSpec.spec
    Test.ApiSpec.spec
    Test.PollZabbixSpec.spec
    Test.HostGroupsSpec.spec
    Test.ProvisionSpec.spec
    Test.LogSpec.spec
    Test.HttpSpec.spec
    Test.VersionSpec.spec
    Test.AssetsSpec.spec
    Test.FacetsSpec.spec
    Test.FilterPrefsSpec.spec
    Test.MarkdownSpec.spec
    Test.TimelineSpec.spec
    Test.FlappingSpec.spec
    Test.ReportsSpec.spec
    Test.TimeRangeSpec.spec
    Test.PrivilegeSpec.spec
    Test.LiveSpec.spec
