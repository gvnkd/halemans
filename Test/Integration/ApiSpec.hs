module Test.Integration.ApiSpec (spec) where


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

m6Spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
m6Spec = describe "public API (milestone 6)" do
    describe "api tokens" do
        it "resolves a freshly created token by its plaintext" do
            user <- m6User ["view"]
            (token, plaintext) <- newApiToken (get #id user) "ci" ["alerts:read"] Nothing
            token.prefix `shouldBe` Text.take 8 plaintext
            token.tokenHash `shouldBe` hashToken plaintext
            resolved <- resolveToken plaintext
            fmap (get #id) resolved `shouldBe` Just (get #id token)
            resolveToken (plaintext <> "x") `shouldReturn` Nothing

        it "rejects revoked and expired tokens" do
            user <- m6User ["view"]
            now <- getCurrentTime
            (revoked, revokedPlaintext) <- newApiToken (get #id user) "revoked" ["alerts:read"] Nothing
            void (revoked |> set #revokedAt (Just now) |> updateRecord)
            resolveToken revokedPlaintext `shouldReturn` Nothing
            (_, expiredPlaintext) <- newApiToken (get #id user) "expired" ["alerts:read"] (Just (UTCTime (fromGregorian 2020 1 1) 0))
            resolveToken expiredPlaintext `shouldReturn` Nothing

        it "touches last_used_at at most once per minute" do
            user <- m6User ["view"]
            (token, plaintext) <- newApiToken (get #id user) "ci" ["alerts:read"] Nothing
            Just _ <- resolveToken plaintext
            touched <- fetch (get #id token)
            isJust touched.lastUsedAt `shouldBe` True
            Just _ <- resolveToken plaintext
            again <- fetch (get #id token)
            again.lastUsedAt `shouldBe` touched.lastUsedAt

        it "authorizeToken denies bad credentials, wrong scope and demoted owners" do
            user <- m6User ["view"]
            (_, plaintext) <- newApiToken (get #id user) "metrics-only" ["metrics"] Nothing
            missing <- authorizeToken Nothing "alerts:read"
            denyStatus missing `shouldBe` Just status401
            unknown <- authorizeToken (Just "Bearer nope") "alerts:read"
            denyStatus unknown `shouldBe` Just status401
            wrongScope <- authorizeToken (Just ("Bearer " <> plaintext)) "alerts:read"
            denyStatus wrongScope `shouldBe` Just status403
            allowed <- authorizeToken (Just ("Bearer " <> plaintext)) "metrics"
            case allowed of
                Allow _ allowedUser -> get #id allowedUser `shouldBe` get #id user
                Deny{} -> expectationFailure "expected Allow"
            demoted <- m6User ["ack"]
            (_, demotedPlaintext) <- newApiToken (get #id demoted) "ci" ["alerts:read"] Nothing
            demotedDecision <- authorizeToken (Just ("Bearer " <> demotedPlaintext)) "alerts:read"
            denyStatus demotedDecision `shouldBe` Just status403

    describe "listAlertsPage" do
        it "filters by environment, status, severity, fingerprint, host and service" do
            source <- testSource
            envName <- ("m6env-" <>) . tshow <$> nextRandom
            fp1 <- freshFingerprint
            fp2 <- freshFingerprint
            Just a1 <- ingest source (testEventIn envName fp1 Firing)
            Just a2 <- ingest source ((testEventIn envName fp2 Firing){severity = "critical"})
            void (ingest source (testEventIn envName fp2 Resolved))
            let idsOf filters = map (get #id) . fst <$> listAlertsPage filters
            idsOf defaultFilters{afEnvironment = envName} `shouldReturn` [a2, a1]
            idsOf defaultFilters{afEnvironment = envName, afStatus = "firing"} `shouldReturn` [a1]
            idsOf defaultFilters{afEnvironment = envName, afStatus = "resolved"} `shouldReturn` [a2]
            idsOf defaultFilters{afEnvironment = envName, afSeverity = "critical"} `shouldReturn` [a2]
            idsOf defaultFilters{afEnvironment = envName, afFingerprint = fp1} `shouldReturn` [a1]
            idsOf defaultFilters{afEnvironment = envName, afHost = "itest-host"} `shouldReturn` [a2, a1]
            idsOf defaultFilters{afEnvironment = envName, afHost = "no-such-host"} `shouldReturn` []
            idsOf defaultFilters{afEnvironment = envName, afService = "itest-svc"} `shouldReturn` [a2, a1]
            idsOf defaultFilters{afEnvironment = "no-such-env"} `shouldReturn` []

        it "paginates with a stable cursor and ends with next_cursor Nothing" do
            source <- testSource
            envName <- ("m6env-" <>) . tshow <$> nextRandom
            let fire = do fp <- freshFingerprint; ingest source (testEventIn envName fp Firing)
            Just a1 <- fire
            Just a2 <- fire
            Just a3 <- fire
            (page1, next1) <- listAlertsPage defaultFilters{afEnvironment = envName, afLimit = 2}
            map (get #id) page1 `shouldBe` [a3, a2]
            -- An alert inserted between pages is newer than the cursor and
            -- must not appear on the next page (keyset stability).
            Just _ <- fire
            (page2, next2) <- listAlertsPage defaultFilters{afEnvironment = envName, afLimit = 2, afCursor = decodeCursor =<< next1}
            map (get #id) page2 `shouldBe` [a1]
            next2 `shouldBe` Nothing

        it "honors since/until on last_seen_at" do
            source <- testSource
            envName <- ("m6env-" <>) . tshow <$> nextRandom
            fp <- freshFingerprint
            Just a1 <- ingest source (testEventIn envName fp Firing)
            now <- getCurrentTime
            let old = addUTCTime (-3600) now
            void (sqlExecTyped [typedSql| UPDATE alerts SET last_seen_at = ${old} WHERE id = ${a1} |])
            let idsOf filters = map (get #id) . fst <$> listAlertsPage filters
            idsOf defaultFilters{afEnvironment = envName, afSince = addUTCTime (-60) now} `shouldReturn` []
            idsOf defaultFilters{afEnvironment = envName, afUntil = addUTCTime (-60) now} `shouldReturn` [a1]
            idsOf defaultFilters{afEnvironment = envName, afSince = addUTCTime (-7200) now, afUntil = now} `shouldReturn` [a1]

    describe "alertDetail" do
        it "returns the ordered timeline, group membership and latest done analysis" do
            source <- testSource
            envName <- ("m6env-" <>) . tshow <$> nextRandom
            fp <- freshFingerprint
            Just a1 <- ingest source (testEventIn envName fp Firing)
            void (ingest source (testEventIn envName fp Firing))
            group <-
                newRecord @AlertGroup
                    |> set #groupKey fp
                    |> set #title "m6 group"
                    |> set #status "firing"
                    |> set #worstSeverity "warning"
                    |> set #memberCount 1
                    |> createRecord
            alert <- fetch a1
            void (alert |> set #groupId (Just (get #id group)) |> updateRecord)
            void $
                newRecord @LlmAnalysis
                    |> set #alertId a1
                    |> set #status "done"
                    |> set #resultMd "old analysis"
                    |> set #createdAt (UTCTime (fromGregorian 2998 1 1) 0)
                    |> createRecord
            void $
                newRecord @LlmAnalysis
                    |> set #alertId a1
                    |> set #status "done"
                    |> set #resultMd "new analysis"
                    |> set #createdAt (UTCTime (fromGregorian 2999 1 1) 0)
                    |> createRecord
            Just detail <- alertDetail a1
            let kinds = map (get #kind . fst) detail.adTimeline
            head kinds `shouldBe` Just "created"
            kinds `shouldSatisfy` elem "repeated"
            let timestamps = map (get #createdAt . fst) detail.adTimeline
            timestamps `shouldBe` sort timestamps
            fmap (get #id) detail.adGroup `shouldBe` Just (get #id group)
            fmap (get #resultMd) detail.adAnalysis `shouldBe` Just (Just "new analysis")
            isJust detail.adEnvironment `shouldBe` True
            isJust detail.adHost `shouldBe` True

        it "returns Nothing for an unknown id" do
            missing <- Id <$> nextRandom
            detail <- alertDetail missing
            isNothing detail `shouldBe` True

    describe "collectMetrics" do
        it "exposes alert, source, job, llm, ws and build series matching the DB" do
            source <- testSource
            envName <- ("m6env-" <>) . tshow <$> nextRandom
            fp <- freshFingerprint
            Just _ <- ingest source (testEventIn envName fp Firing)
            body <- collectMetrics
            body `shouldSatisfy` Text.isInfixOf ("halemans_alerts{environment=\"" <> envName <> "\",status=\"firing\",severity=\"warning\"} 1\n")
            body `shouldSatisfy` Text.isInfixOf "# TYPE halemans_source_consecutive_failures gauge\n"
            body `shouldSatisfy` Text.isInfixOf ("halemans_source_healthy{source=\"" <> source.name <> "\"} 1\n")
            body `shouldSatisfy` Text.isInfixOf "# TYPE halemans_job_runs_total gauge\n"
            pendingBefore <- sqlQueryTyped [typedSql| SELECT count(*) FROM poll_zabbix_jobs WHERE status = 'job_status_not_started' |]
            void (sqlExecTyped [typedSql| INSERT INTO poll_zabbix_jobs DEFAULT VALUES |])
            bodyWithJob <- collectMetrics
            bodyWithJob `shouldSatisfy` Text.isInfixOf ("halemans_job_runs_total{job=\"poll_zabbix\",status=\"not_started\"} " <> tshow (fromMaybe 0 (head pendingBefore) + 1) <> "\n")
            body `shouldSatisfy` Text.isInfixOf "# TYPE halemans_llm_tokens_today gauge\n"
            body `shouldSatisfy` Text.isInfixOf "halemans_ws_connections "
            body `shouldSatisfy` Text.isInfixOf "halemans_build_info{version=\"1.1.0\"} 1\n"
  where
    denyStatus (Deny status _ _) = Just status
    denyStatus Allow{} = Nothing


-- | Public read-only API (m6).
spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = m6Spec
