module Test.Integration.LlmSpec (spec) where


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

-- LLM enrichment (design_docs/milestone_4.md §10): against the mock
-- OpenAI-compatible server on 18084 (launched by the check; deterministic
-- completions with /debug/fail backdoors for 429/500/malformed).
llmSpec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
llmSpec = describe "llm enrichment (milestone 4)" do
    it "a new alert enqueues an analysis; the job completes against the mock" do
        _ <- ensureTemplate
        source <- testSource
        fp <- freshFingerprint
        Just alertId <-
            ingest
                source
                (testEvent fp Firing)
                    { title = "disk pressure on itest-host"
                    , description = "disk usage above 90%"
                    }
        analysis <- latestAnalysis alertId
        analysis.status `shouldBe` "queued"
        -- refire does not enqueue another analysis (milestone_4.md §4)
        void (ingest source (testEvent fp Firing))
        analyses <-
            query @LlmAnalysis
                |> filterWhere (#alertId, alertId)
                |> fetch
        length analyses `shouldBe` 1
        performLatestJob (get #id analysis)
        done <- fetch (get #id analysis)
        done.status `shouldBe` "done"
        done.resultMd `shouldSatisfy` maybe False (not . Text.null)
        done.provider `shouldBe` "default"
        done.model `shouldBe` "mock-llm-1"
        done.promptVersion `shouldBe` Just 1
        done.dedupedFrom `shouldBe` Nothing
        case done.result of
            Just result -> payloadText "probable_cause" result `shouldSatisfy` isJust
            Nothing -> expectationFailure "structured result missing"
        countered <- counterRequestsAfter "default"
        countered `shouldSatisfy` (>= 1)

    it "identical context within the window dedupes into a copy (one provider call)" do
        _ <- ensureTemplate
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        first <- latestAnalysis alertId
        performLatestJob (get #id first)
        requestsBefore <- counterRequestsAfter "default"
        second <- enqueueAnalysis alertId
        performLatestJob (get #id second)
        copy <- fetch (get #id second)
        copy.status `shouldBe` "done"
        copy.dedupedFrom `shouldBe` Just (get #id first)
        original <- fetch (get #id first)
        copy.result `shouldBe` original.result
        copy.resultMd `shouldBe` original.resultMd
        requestsAfter <- counterRequestsAfter "default"
        requestsAfter `shouldBe` requestsBefore

    it "a stale-recovered job re-runs an analysis left running by a crashed worker" do
        _ <- ensureTemplate
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        analysis <- latestAnalysis alertId
        -- simulate the crash window: worker set running, then died before done
        crashed <-
            analysis
                |> set #status "running"
                |> updateRecord
        performLatestJob (get #id crashed)
        recovered <- fetch (get #id analysis)
        recovered.status `shouldBe` "done"
        recovered.resultMd `shouldSatisfy` maybe False (not . Text.null)

    it "daily budget cap soft-skips with an llm_skipped event" do
        _ <- ensureTemplate
        oldBudget <- lookupEnv "LLM_DAILY_TOKEN_BUDGET"
        setEnv "LLM_DAILY_TOKEN_BUDGET" "0"
        flip finally (maybe (unsetEnv "LLM_DAILY_TOKEN_BUDGET") (setEnv "LLM_DAILY_TOKEN_BUDGET") oldBudget) do
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing)
            analysis <- latestAnalysis alertId
            performLatestJob (get #id analysis)
            skipped <- fetch (get #id analysis)
            skipped.status `shouldBe` "failed"
            skipped.errorMessage `shouldBe` Just "budget_exceeded"
            events <- eventKinds alertId
            events `shouldSatisfy` ("llm_skipped" `elem`)

    it "a retriable 429 requeues the job, then completes" do
        _ <- ensureTemplate
        mockFail "429" 1
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        analysis <- latestAnalysis alertId
        performLatestJob (get #id analysis)
        requeued <- fetch (get #id analysis)
        requeued.status `shouldBe` "queued"
        jobs <-
            query @LlmAnalysisJob
                |> filterWhere (#analysisId, get #id analysis)
                |> fetch
        length jobs `shouldBe` 2
        performLatestJob (get #id analysis)
        done <- fetch (get #id analysis)
        done.status `shouldBe` "done"

    it "persistent 500s exhaust the retry budget into failed" do
        _ <- ensureTemplate
        mockFail "500" 4
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        analysis <- latestAnalysis alertId
        replicateM_ 4 (performLatestJob (get #id analysis))
        failed <- fetch (get #id analysis)
        failed.status `shouldBe` "failed"
        failed.errorMessage `shouldSatisfy` maybe False ("500" `Text.isInfixOf`)
        events <- eventKinds alertId
        events `shouldSatisfy` ("llm_failed" `elem`)

    it "malformed json degrades to markdown-only" do
        _ <- ensureTemplate
        mockFail "malformed" 1
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        analysis <- latestAnalysis alertId
        performLatestJob (get #id analysis)
        done <- fetch (get #id analysis)
        done.status `shouldBe` "done"
        done.resultMd `shouldSatisfy` maybe False (not . Text.null)
        done.result `shouldBe` Nothing

    it "missing provider config soft-fails the analysis" do
        _ <- ensureTemplate
        oldEndpoint <- lookupEnv "LLM_ENDPOINT"
        unsetEnv "LLM_ENDPOINT"
        flip finally (maybe (unsetEnv "LLM_ENDPOINT") (setEnv "LLM_ENDPOINT") oldEndpoint) do
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing)
            analysis <- latestAnalysis alertId
            performLatestJob (get #id analysis)
            failed <- fetch (get #id analysis)
            failed.status `shouldBe` "failed"
            failed.errorMessage `shouldBe` Just "llm_not_configured"
            events <- eventKinds alertId
            events `shouldSatisfy` ("llm_skipped" `elem`)

    it "feedback is one vote per user per analysis; re-vote updates" do
        user <- testUser
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        analysis <- latestAnalysis alertId
        _ <-
            newRecord @LlmFeedback
                |> set #analysisId (get #id analysis)
                |> set #userId (get #id user)
                |> set #score 1
                |> createRecord
        duplicate <-
            try
                ( void
                    ( newRecord @LlmFeedback
                        |> set #analysisId (get #id analysis)
                        |> set #userId (get #id user)
                        |> set #score (-1)
                        |> createRecord
                    )
                ) ::
                IO (Either SomeException ())
        case duplicate of
            Left _ -> pure ()
            Right _ -> expectationFailure "duplicate feedback should violate the unique index"
        existing <-
            query @LlmFeedback
                |> filterWhere (#analysisId, get #id analysis)
                |> filterWhere (#userId, get #id user)
                |> fetchOneOrNothing
                >>= maybe (error "feedback missing") pure
        void (existing |> set #score (-1) |> updateRecord)
        votes <-
            query @LlmFeedback
                |> filterWhere (#analysisId, get #id analysis)
                |> fetch
        map (get #score) votes `shouldBe` [-1]


toolCacheSpec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
toolCacheSpec = describe "LLM tool cache (milestone 10 §6)" do
    it "serves repeat calls from the cache within the TTL" do
        resetToolCache
        counter <- newIORef (0 :: Int)
        let action = modifyIORef' counter (+ 1) >> pure "cached-result"
        first <- cachedToolCall "itest_tool" "{}" action
        second <- cachedToolCall "itest_tool" "{}" action
        first `shouldBe` "cached-result"
        second `shouldBe` "cached-result"
        readIORef counter `shouldReturn` 1
    it "keys on tool and arguments separately" do
        resetToolCache
        counter <- newIORef (0 :: Int)
        let action = modifyIORef' counter (+ 1) >> pure "x"
        _ <- cachedToolCall "itest_tool" "{\"a\":1}" action
        _ <- cachedToolCall "itest_tool" "{\"a\":2}" action
        _ <- cachedToolCall "itest_tool_2" "{\"a\":1}" action
        readIORef counter `shouldReturn` 3
    it "never caches failure texts" do
        resetToolCache
        counter <- newIORef (0 :: Int)
        let action = modifyIORef' counter (+ 1) >> pure "jira search failed: boom"
        _ <- cachedToolCall "itest_tool" "{}" action
        _ <- cachedToolCall "itest_tool" "{}" action
        readIORef counter `shouldReturn` 2
    it "a disabled config bypasses the cache" do
        resetToolCache
        _ <- createRecord (newRecord @LlmToolCacheConfig |> set #enabled False |> set #ttlSeconds 300)
        flip finally resetToolCacheConfig do
            counter <- newIORef (0 :: Int)
            let action = modifyIORef' counter (+ 1) >> pure "y"
            _ <- cachedToolCall "itest_tool" "{}" action
            _ <- cachedToolCall "itest_tool" "{}" action
            readIORef counter `shouldReturn` 2


-- | LLM enrichment (m4) and the tool cache (m10 §6).
spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = llmSpec >> toolCacheSpec
