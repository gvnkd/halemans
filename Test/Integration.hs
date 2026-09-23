module Main where

import IHP.Prelude
import qualified Test.Integration.AgentSpec
import qualified Test.Integration.ApiSpec
import qualified Test.Integration.DashboardsSpec
import qualified Test.Integration.EnrichmentSpec
import qualified Test.Integration.LlmSpec
import qualified Test.Integration.MetricChartSpec
import qualified Test.Integration.PipelineSpec
import qualified Test.Integration.ProvisioningSpec
import Test.Integration.Setup (integrationMain)

-- Thin runner: suites live in Test/Integration/*Spec.hs (milestone 12 §7),
-- shared schema bootstrap and helpers in Test/Integration/Setup.hs.
main :: IO ()
main =
    integrationMain do
        Test.Integration.PipelineSpec.spec
        Test.Integration.LlmSpec.spec
        Test.Integration.ApiSpec.spec
        Test.Integration.ProvisioningSpec.spec
        Test.Integration.EnrichmentSpec.spec
        Test.Integration.MetricChartSpec.spec
        Test.Integration.DashboardsSpec.spec
        Test.Integration.AgentSpec.spec
