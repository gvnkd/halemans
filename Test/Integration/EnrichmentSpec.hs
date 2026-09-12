module Test.Integration.EnrichmentSpec (spec) where


import Control.Exception (SomeException, finally, try)
import Control.Monad (replicateM_, void)
import Data.Aeson (object)
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseMaybe)
import Data.Int (Int64)
import qualified Data.Text as Text
import Data.UUID.V4 (nextRandom)
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.FrameworkConfig (FrameworkConfig, buildFrameworkConfig)
import IHP.Job.Types (Job (..))
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
import qualified Network.Wreq as Wreq
import System.Environment (getEnv, lookupEnv, setEnv, unsetEnv)
import System.Process (callProcess, readProcess)
import Test.Hspec

import qualified Application.Connector.Grafana as Grafana
import Application.Helper.DashboardConfig (DashboardCard (..), FacetRef (..), MatchClause (..), MatchOp (..), decodeDashboardConfig)
import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest)
import Application.Job.AutoClose (autoCloseResolved, closeStalledAlerts, stallStaleAlerts, unackExpiredAcks, unsuppressExpired)
import Application.Job.EnrichAlert ()
import Application.Job.Escalation (runDueTrackers)
import Application.Job.FacetBackfill ()
import Application.Job.LlmAnalysis ()
import Application.Job.PollZabbix ()
import Application.Job.Retention ()
import Application.Job.SourceHealth (checkSilence)
import Application.Pipeline.Actions (ackAlert, closeAlert, unackAlert)
import Application.Pipeline.Grouping (AlertField (..), facetValue)
import Application.Service.AlertList (AlertListFilters (..), defaultAlertListFilters, effectiveEnvNames, listAlerts)
import Application.Service.Api.Alerts (AlertDetail (..), AlertFilters (..), alertDetail, defaultFilters, listAlertsPage)
import Application.Service.Api.Auth (AuthDecision (..), authorizeToken)
import Application.Service.Api.Cursor (decodeCursor)
import Application.Service.Api.Metrics (collectMetrics)
import Application.Service.Api.Token (hashToken, newApiToken, resolveToken)
import Application.Service.Assets.Attrs (objectAttributes)
import Application.Service.DashboardCards (CardGroup (..), CardSummary (..), ExpandedCard (..), expandDashboardCards, runCardQuery, runCardQueryGroups, runCardSummary)
import Application.Service.Jira.DbConfig (syncOpenLinks)
import Application.Service.Llm (LlmProviderConfig (..), ToolCall (..))
import Application.Service.Llm.DbConfig (currentLlmConfig)
import Application.Service.Llm.ToolCache (cachedToolCall)
import Application.Service.Llm.Tools (executeToolCall)
import Application.Service.Notify (currentOnCall, resolveRuleTargets)
import Application.Service.PollerControl (ensurePollerForSourceType)
import Application.Service.Provision (ProvisionError (..), applyProvisionConfig)
import Application.Service.Reconcile (lastAckWasExternal, mirrorExternalAck, mirrorExternalUnack)
import Application.Service.SourceHealth (healthFingerprint, reconcileFingerprint, recordFailure, recordReconcileFailure, recordReconcileSuccess, recordSuccess)
import Application.Service.WriteBack (executeAttempt)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (status401, status403)
import Web.View.Dashboard.Index (EnvCard (..), computeEnvCards)
import Test.Integration.Setup

-- Milestone 8: assets enrichment + agent roles against the mock Assets
-- server on 18085 (launched by the check; seeded Capacity CMDB dataset with
-- dev-host-01, /debug/reset + /debug/fail/500 backdoors).
m8Spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
m8Spec = describe "enrichment phase 0 (milestone 8)" do
    it "enrich caches and links assets for a mock-known host" do
        config <- ensureAssetsConfig
        source <- testSource
        fp <- freshFingerprint
        Just alertId <-
            ingest
                source
                (testEvent fp Firing)
                    { host = Just "dev-host-01"
                    , checkName = Just "halemans test trigger"
                    }
        job <- enrichJobFor alertId
        perform job
        objects <-
            query @AssetsObject
                |> filterWhere (#configId, get #id config)
                |> fetch
        case objects of
            [object] -> do
                object.objectId `shouldBe` 10001
                object.label_ `shouldBe` "dev-host-01"
                object.objectTypeName `shouldBe` "Host"
                assetAttr "Owner" object `shouldBe` Just "team-sre"
                assetAttr "Status" object `shouldBe` Just "Active"
                assetAttr "Datacenter" object `shouldBe` Just "dc-eu-1"
            _ -> expectationFailure "expected exactly one cached asset"
        links <-
            query @AssetAlertLink
                |> filterWhere (#alertId, alertId)
                |> fetch
        case links of
            [link] -> link.matchedBy `shouldBe` "dev-host-01"
            _ -> expectationFailure "expected exactly one asset link"
        -- second run: upserts are idempotent
        perform job
        objectsAfter <-
            query @AssetsObject
                |> filterWhere (#configId, get #id config)
                |> fetch
        length objectsAfter `shouldBe` 1
        linksAfter <-
            query @AssetAlertLink
                |> filterWhere (#alertId, alertId)
                |> fetch
        length linksAfter `shouldBe` 1

    it "unknown hosts are negative-cached (no re-query within the TTL)" do
        _ <- ensureAssetsConfig
        source <- testSource
        fp <- freshFingerprint
        Just alertId <-
            ingest
                source
                (testEvent fp Firing)
                    { host = Just "itest-m8-unknown-host"
                    }
        job <- enrichJobFor alertId
        perform job
        linked <-
            query @AssetAlertLink
                |> filterWhere (#alertId, alertId)
                |> filterWhereSql (#assetsObjectId, "IS NOT NULL")
                |> fetch
        length linked `shouldBe` 0
        misses <-
            query @AssetAlertLink
                |> filterWhere (#alertId, alertId)
                |> fetch
        case misses of
            [miss] -> do
                miss.assetsObjectId `shouldBe` Nothing
                miss.matchedBy `shouldSatisfy` ("itest-m8-unknown-host" `Text.isInfixOf`)
            _ -> expectationFailure "expected exactly one negative-cache marker"
        -- Within the TTL the lookup short-circuits on the marker: even with
        -- the mock armed to fail, the second run records no failure.
        assetsMockFail 10
        perform job
        assetsMockReset
        failures <-
            query @AlertEvent
                |> filterWhere (#alertId, alertId)
                |> filterWhere (#kind, "enrichment_failed" :: Text)
                |> fetch
        mapMaybe (payloadText "subsystem" . (get #payload)) failures `shouldBe` []

    it "assets outage soft-fails with an enrichment_failed event" do
        _ <- ensureAssetsConfig
        assetsMockFail 10
        flip finally assetsMockReset do
            source <- testSource
            fp <- freshFingerprint
            Just alertId <-
                ingest
                    source
                    (testEvent fp Firing)
                        { host = Just "itest-m8-softfail-host"
                        }
            job <- enrichJobFor alertId
            perform job
            failures <-
                query @AlertEvent
                    |> filterWhere (#alertId, alertId)
                    |> filterWhere (#kind, "enrichment_failed" :: Text)
                    |> fetch
            mapMaybe (payloadText "subsystem" . (get #payload)) failures `shouldBe` ["assets"]

    it "a role on the analysis drives the prompt template and is recorded" do
        _ <- ensureTemplate
        -- m7 provisioning tests leave an enabled llm_configs row behind;
        -- disable DB providers so the env mock config applies (DB-first
        -- resolution, milestone_7.md §7).
        void $ sqlExecTyped [typedSql| UPDATE llm_configs SET enabled = false |]
        suffix <- tshow <$> nextRandom
        let templateName = "itest_role_marker_" <> suffix
            roleName = "itest-role-" <> suffix
        template <-
            newRecord @LlmPromptTemplate
                |> set #name templateName
                |> set #version 1
                |> set #body "ROLE MARKER {{alert.title}}\nAssets:\n{{assets_excerpt}}"
                |> set #active True
                |> createRecord
        role <-
            newRecord @LlmAgentRole
                |> set #name roleName
                |> set #promptTemplateName templateName
                |> set #tools (Aeson.toJSON ["assets_lookup" :: Text])
                |> set #enabled True
                |> set #isDefault False
                |> createRecord
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        analysis <-
            newRecord @LlmAnalysis
                |> set #alertId alertId
                |> set #agentRoleId (Just (get #id role))
                |> createRecord
        void do
            newRecord @LlmAnalysisJob
                |> set #analysisId (get #id analysis)
                |> createRecord
        performLatestJob (get #id analysis)
        done <- fetch (get #id analysis)
        done.status `shouldBe` "done"
        done.agentRoleId `shouldBe` Just (get #id role)
        done.promptTemplateId `shouldBe` Just (get #id template)

    it "assets_lookup returns an in-band summary from the mock" do
        _ <- ensureAssetsConfig
        result <-
            executeToolCall
                Nothing
                ToolCall
                    { callId = "call-1"
                    , callName = "assets_lookup"
                    , callArguments = "{\"term\": \"dev-host-01\"}"
                    }
        result `shouldSatisfy` ("dev-host-01" `Text.isInfixOf`)
        result `shouldSatisfy` ("CHCMDB-10001" `Text.isInfixOf`)
        failing <-
            executeToolCall
                Nothing
                ToolCall
                    { callId = "call-2"
                    , callName = "assets_lookup"
                    , callArguments = "{\"term\": \"itest-no-such-asset\"}"
                    }
        failing `shouldBe` "no assets found"

    it "the default role applies to automatic analyses" do
        _ <- ensureTemplate
        void $ sqlExecTyped [typedSql| UPDATE llm_configs SET enabled = false |]
        void $ sqlExecTyped [typedSql| UPDATE llm_agent_roles SET is_default = false |]
        suffix <- tshow <$> nextRandom
        role <-
            newRecord @LlmAgentRole
                |> set #name ("itest-default-role-" <> suffix)
                |> set #promptTemplateName "alert_enrichment"
                |> set #tools (Aeson.toJSON ([] :: [Text]))
                |> set #enabled True
                |> set #isDefault True
                |> createRecord
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        analysis <- latestAnalysis alertId
        performLatestJob (get #id analysis)
        done <- fetch (get #id analysis)
        done.status `shouldBe` "done"
        done.agentRoleId `shouldBe` Just (get #id role)


-- | Enrichment phase 0 (m8).
spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = m8Spec
