module Main where

import Test.Hspec
import IHP.Prelude

-- Import your test specs here:
import qualified Test.StateMachineSpec
import qualified Test.BlackoutsSpec
import qualified Test.PushSpec
import qualified Test.GroupingSpec
import qualified Test.EscalationSpec
import qualified Test.AlertmanagerSpec
import qualified Test.CmdbSpec
import qualified Test.JiraSpec
import qualified Test.WriteBackSpec
import qualified Test.ReconcileSpec
import qualified Test.DashboardConfigSpec
import qualified Test.ThemeSpec
import qualified Test.LlmSpec
import qualified Test.SourceHealthSpec
import qualified Test.AuditExportSpec
import qualified Test.ApiSpec
import qualified Test.PollZabbixSpec
import qualified Test.HostGroupsSpec
import qualified Test.ProvisionSpec
import qualified Test.LogSpec

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
