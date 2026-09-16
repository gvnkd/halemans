module Test.Integration.PipelineSpec (spec) where

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
import Application.Service.Reconcile (lastAckWasExternal, mirrorExternalAck, mirrorExternalSuppress, mirrorExternalUnack, mirrorExternalUnsuppress)
import Application.Service.SourceHealth (healthFingerprint, reconcileFingerprint, recordFailure, recordReconcileFailure, recordReconcileSuccess, recordSuccess)
import Application.Service.WriteBack (executeAttempt)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (status401, status403)
import Test.Integration.Setup
import Web.View.Dashboard.Index (EnvCard (..), computeEnvCards)

m1Spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
m1Spec = describe "alert pipeline (milestone 1)" do
    it "creates an alert with inventory refs and audit events" do
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        alert <- fetch alertId
        alert.status `shouldBe` "firing"
        alert.occurrences `shouldBe` 1
        alert.suppressed `shouldBe` False
        isJust alert.environmentId `shouldBe` True
        isJust alert.hostId `shouldBe` True
        isJust alert.serviceId `shouldBe` True
        host <- fetch (fromMaybe (error "no host") alert.hostId)
        host.autoCreated `shouldBe` True
        events <- eventKinds alertId
        events `shouldBe` ["created"]

    it "dedupes a refire: occurrences bumped, repeated event appended" do
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        void (ingest source (testEvent fp Firing))
        alert <- fetch alertId
        alert.occurrences `shouldBe` 2
        alert.status `shouldBe` "firing"
        events <- eventKinds alertId
        events `shouldBe` ["created", "repeated"]

    it "resolved event resolves; refire re-fires the same alert" do
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        void (ingest source (testEvent fp Resolved))
        resolved <- fetch alertId
        resolved.status `shouldBe` "resolved"
        isJust resolved.resolvedAt `shouldBe` True
        void (ingest source (testEvent fp Firing))
        refired <- fetch alertId
        refired.status `shouldBe` "firing"
        refired.resolvedAt `shouldBe` Nothing

    it "a second resolved event is a no-op with an audit note, never an error" do
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        void (ingest source (testEvent fp Resolved))
        void (ingest source (testEvent fp Resolved))
        events <- eventKinds alertId
        last events `shouldBe` "external"
        alert <- fetch alertId
        alert.status `shouldBe` "resolved"

    it "refire after close creates a new alert" do
        source <- testSource
        fp <- freshFingerprint
        user <- testUser
        Just alertId <- ingest source (testEvent fp Firing)
        alert <- fetch alertId
        acked <- ackAlert user alert Nothing Nothing
        void (closeAlert (Just user) acked (Just "done"))
        Just newAlertId <- ingest source (testEvent fp Firing)
        newAlertId `shouldNotBe` alertId
        oldAlert <- fetch alertId
        oldAlert.status `shouldBe` "closed"
        newAlert <- fetch newAlertId
        newAlert.status `shouldBe` "firing"

    it "blackout suppresses a covered alert and skips notification" do
        source <- testSource
        fp <- freshFingerprint
        -- create the environment via subject resolution first
        void (ingest source (testEventIn "itest-env-bo1" "itest-env-bo1-bootstrap" Firing))
        environment <- fetchEnvironment "itest-env-bo1"
        now <- getCurrentTime
        _ <-
            newRecord @Blackout
                |> set #environmentId (Just (get #id environment))
                |> set #startsAt (addUTCTime (-60) now)
                |> set #endsAt (addUTCTime 3600 now)
                |> set #reason "integration test"
                |> createRecord
        Just alertId <- ingest source (testEventIn "itest-env-bo1" fp Firing)
        alert <- fetch alertId
        alert.suppressed `shouldBe` True
        alert.suppressedBy `shouldBe` Just "blackout"
        events <- eventKinds alertId
        events `shouldSatisfy` ("suppressed" `elem`)
        events `shouldSatisfy` (not . ("notified" `elem`))
        pushJobs <-
            query @PushNotificationJob
                |> filterWhere (#alertId, alertId)
                |> fetch
        length pushJobs `shouldBe` 0

    it "blackout expiry restores the alert via unsuppressExpired" do
        source <- testSource
        fp <- freshFingerprint
        void (ingest source (testEventIn "itest-env-bo2" "itest-env-bo2-bootstrap" Firing))
        environment <- fetchEnvironment "itest-env-bo2"
        now <- getCurrentTime
        _ <-
            newRecord @Blackout
                |> set #environmentId (Just (get #id environment))
                |> set #startsAt (addUTCTime (-60) now)
                |> set #endsAt (addUTCTime (-1) now) -- already expired
                |> createRecord
        -- ingest happens while the blackout is already expired -> not suppressed
        Just alertId <- ingest source (testEventIn "itest-env-bo2" fp Firing)
        -- force the suppressed flag as if the blackout was live at ingest time
        let alertUuid = unpackId alertId
        _ <- sqlExecTyped [typedSql| UPDATE alerts SET suppressed = true WHERE id = ${alertUuid} |]
        unsuppressExpired
        alert <- fetch alertId
        alert.suppressed `shouldBe` False
        events <- eventKinds alertId
        events `shouldSatisfy` ("unsuppressed" `elem`)

    it "ack timeout unacks via unackExpiredAcks" do
        source <- testSource
        fp <- freshFingerprint
        user <- testUser
        Just alertId <- ingest source (testEvent fp Firing)
        alert <- fetch alertId
        _ <- ackAlert user alert Nothing (Just (-1)) -- already-expired timeout
        unackExpiredAcks
        updated <- fetch alertId
        updated.status `shouldBe` "firing"
        events <- eventKinds alertId
        events `shouldSatisfy` ("unack" `elem`)

    it "alert with no source updates stalls, revives on refire, auto-closes after stalled TTL" do
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        let alertUuid = unpackId alertId
        void (sqlExecTyped [typedSql| UPDATE alerts SET last_seen_at = NOW() - INTERVAL '7 hours' WHERE id = ${alertUuid} |])
        stallStaleAlerts
        stalled <- fetch alertId
        stalled.status `shouldBe` "stalled"
        events <- eventKinds alertId
        events `shouldSatisfy` ("stalled" `elem`)
        Just revivedId <- ingest source (testEvent fp Firing)
        revivedId `shouldBe` alertId
        revived <- fetch alertId
        revived.status `shouldBe` "firing"
        void (sqlExecTyped [typedSql| UPDATE alerts SET last_seen_at = NOW() - INTERVAL '7 hours' WHERE id = ${alertUuid} |])
        stallStaleAlerts
        void (sqlExecTyped [typedSql| UPDATE alerts SET updated_at = NOW() - INTERVAL '4 days', last_seen_at = NOW() - INTERVAL '4 days' WHERE id = ${alertUuid} |])
        closeStalledAlerts
        closed <- fetch alertId
        closed.status `shouldBe` "closed"
        closed.closeReason `shouldBe` Just "auto-closed: stalled TTL expired"

    it "alerts of a failing source are not stalled" do
        suffix <- tshow <$> nextRandom
        failing <- integrationSource "webhook" ("stall-guard-" <> suffix) "" (object [])
        void (failing |> set #consecutiveFailures 2 |> updateRecord)
        fp <- freshFingerprint
        Just alertId <- ingest failing (testEvent fp Firing)
        let alertUuid = unpackId alertId
        void (sqlExecTyped [typedSql| UPDATE alerts SET last_seen_at = NOW() - INTERVAL '7 hours' WHERE id = ${alertUuid} |])
        stallStaleAlerts
        alert <- fetch alertId
        alert.status `shouldBe` "firing"

    it "resolved alerts auto-close after the resolved TTL" do
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        void (ingest source (testEvent fp Resolved))
        let alertUuid = unpackId alertId
        void (sqlExecTyped [typedSql| UPDATE alerts SET resolved_at = NOW() - INTERVAL '25 hours' WHERE id = ${alertUuid} |])
        autoCloseResolved
        closed <- fetch alertId
        closed.status `shouldBe` "closed"
        closed.closeReason `shouldBe` Just "auto-closed: resolved TTL expired"

    describe "milestone 2: correlation & teams" do
        it "two alerts on the same host group under the env+host rule; rollup is worst severity + member count" do
            source <- testSource
            rule <- groupingRule "it-grp-1" "{env}/{host}"
            fp1 <- freshFingerprint
            fp2 <- freshFingerprint
            Just alertId1 <- ingest source (testEventIn "itest-env-g1" fp1 Firing)
            Just alertId2 <- ingest source ((testEventIn "itest-env-g1" fp2 Firing){severity = "critical"})
            alert1 <- fetch alertId1
            alert2 <- fetch alertId2
            isJust alert1.groupId `shouldBe` True
            alert1.groupId `shouldBe` alert2.groupId
            alert1.groupedByVersion `shouldBe` Just rule.version
            let Just groupId = alert1.groupId
            group <- fetch groupId
            group.groupKey `shouldBe` "itest-env-g1/itest-host"
            group.memberCount `shouldBe` 2
            group.worstSeverity `shouldBe` "critical"
            group.status `shouldBe` "firing"

        it "refire of a group member bumps occurrences, not member count" do
            source <- testSource
            _ <- groupingRule "it-grp-2" "{env}/{host}"
            fp <- freshFingerprint
            Just alertId <- ingest source (testEventIn "itest-env-g2" fp Firing)
            void (ingest source (testEventIn "itest-env-g2" fp Firing))
            alert <- fetch alertId
            alert.occurrences `shouldBe` 2
            let Just groupId = alert.groupId
            group <- fetch groupId
            group.memberCount `shouldBe` 1

        it "group notification is throttled at group level: two members, one notification" do
            source <- testSource
            user <- testUser
            _ <- groupingRule "it-grp-3" "{env}/{host}"
            _ <- notificationRule "it-notify-g3" (Just (get #id user)) Nothing
            fp1 <- freshFingerprint
            fp2 <- freshFingerprint
            Just alertId1 <- ingest source ((testEventIn "itest-env-g3" fp1 Firing){severity = "critical"})
            Just alertId2 <- ingest source ((testEventIn "itest-env-g3" fp2 Firing){severity = "critical"})
            notified1 <- notifiedEvents alertId1
            notified2 <- notifiedEvents alertId2
            length notified1 `shouldBe` 1
            length notified2 `shouldBe` 0

        it "rule edit bumps version; grouped alerts keep their group and version" do
            source <- testSource
            rule <- groupingRule "it-grp-4" "{env}/{host}"
            fp <- freshFingerprint
            Just alertId <- ingest source (testEventIn "itest-env-g4" fp Firing)
            updatedRule <-
                rule
                    |> set #groupKeyTemplate "{env}/{host}/{check}"
                    |> set #version (rule.version + 1)
                    |> updateRecord
            updatedRule.version `shouldBe` 2
            alert <- fetch alertId
            alert.groupedByVersion `shouldBe` Just 1
            isJust alert.groupId `shouldBe` True

        it "disabled rules never match, even at an earlier position" do
            source <- testSource
            _ <-
                newRecord @GroupingRule
                    |> set #name "it-grp-disabled"
                    |> set #position 0
                    |> set #enabled False
                    |> set #version 77
                    |> set #match (object [])
                    |> set #groupKeyTemplate "disabled-rule-key"
                    |> createRecord
            fp <- freshFingerprint
            Just alertId <- ingest source (testEventIn "itest-env-norule" fp Firing)
            alert <- fetch alertId
            isJust alert.groupId `shouldBe` True
            alert.groupedByVersion `shouldNotBe` Just 77

        it "subject-less alerts are never grouped (all-dash key guard)" do
            source <- testSource
            fp <- freshFingerprint
            Just alertId <-
                ingest
                    source
                    (testEvent fp Firing)
                        { env = Nothing
                        , host = Nothing
                        , service = Nothing
                        }
            alert <- fetch alertId
            alert.groupId `shouldBe` Nothing

        it "escalation: tracker created on notify, step fires on schedule, ack cancels, unack restarts" do
            source <- testSource
            user <- testUser
            policy <-
                newRecord @EscalationPolicy
                    |> set #name "it-policy-1"
                    |> set
                        #steps
                        ( Aeson.toJSON
                            [ object
                                [ "after_seconds" .= (0 :: Int)
                                , "target_user_id" .= tshow (get #id user)
                                ]
                            ]
                        )
                    |> createRecord
            _ <- notificationRule "it-notify-esc1" (Just (get #id user)) (Just (get #id policy))
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEvent fp Firing){severity = "critical"})
            tracker <-
                query @EscalationTracker
                    |> filterWhere (#alertId, alertId)
                    |> filterWhere (#status, "active" :: Text)
                    |> fetchOneOrNothing
                    >>= maybe (error "tracker missing") pure
            runDueTrackers
            events <- eventKinds alertId
            events `shouldSatisfy` ("escalated" `elem`)
            firedTracker <- fetch (get #id tracker)
            firedTracker.status `shouldBe` "done" -- single step: no further advance
        it "ack cancels an active tracker; unack re-activates from step 0" do
            source <- testSource
            user <- testUser
            policy <-
                newRecord @EscalationPolicy
                    |> set #name "it-policy-2"
                    |> set
                        #steps
                        ( Aeson.toJSON
                            [ object ["after_seconds" .= (3600 :: Int), "target_user_id" .= tshow (get #id user)]
                            , object ["after_seconds" .= (3600 :: Int), "target_user_id" .= tshow (get #id user)]
                            ]
                        )
                    |> createRecord
            _ <- notificationRule "it-notify-esc2" (Just (get #id user)) (Just (get #id policy))
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEvent fp Firing){severity = "critical"})
            alert <- fetch alertId
            acked <- ackAlert user alert Nothing Nothing
            tracker <-
                query @EscalationTracker
                    |> filterWhere (#alertId, alertId)
                    |> fetchOneOrNothing
                    >>= maybe (error "tracker missing") pure
            tracker.status `shouldBe` "cancelled"
            _ <- unackAlert (Just user) acked "back to firing"
            restarted <- fetch (get #id tracker)
            restarted.status `shouldBe` "active"
            restarted.currentStep `shouldBe` 0

        it "suppressed alerts never create escalation trackers" do
            source <- testSource
            user <- testUser
            policy <-
                newRecord @EscalationPolicy
                    |> set #name "it-policy-3"
                    |> set #steps (Aeson.toJSON [object ["after_seconds" .= (0 :: Int), "target_user_id" .= tshow (get #id user)]])
                    |> createRecord
            _ <- notificationRule "it-notify-esc3" (Just (get #id user)) (Just (get #id policy))
            void (ingest source (testEventIn "itest-env-bo3" "itest-env-bo3-bootstrap" Firing))
            environment <- fetchEnvironment "itest-env-bo3"
            now <- getCurrentTime
            _ <-
                newRecord @Blackout
                    |> set #environmentId (Just (get #id environment))
                    |> set #startsAt (addUTCTime (-60) now)
                    |> set #endsAt (addUTCTime 3600 now)
                    |> set #reason "integration test"
                    |> createRecord
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn "itest-env-bo3" fp Firing){severity = "critical"})
            trackers <-
                query @EscalationTracker
                    |> filterWhere (#alertId, alertId)
                    |> fetch
            length trackers `shouldBe` 0

        it "currentOnCall stub: first schedule member; missing schedule falls back to all team members" do
            user <- testUser
            otherUser <-
                newRecord @User
                    |> set #email "itest2@dev"
                    |> set #passwordHash "unused"
                    |> createRecord
            team <-
                newRecord @Team
                    |> set #name "it-team-1"
                    |> createRecord
            _ <-
                newRecord @TeamMember
                    |> set #teamId (get #id team)
                    |> set #userId (get #id user)
                    |> set #teamRole "lead"
                    |> createRecord
            _ <-
                newRecord @TeamMember
                    |> set #teamId (get #id team)
                    |> set #userId (get #id otherUser)
                    |> set #teamRole "member"
                    |> createRecord
            -- no schedule row yet: targets fall back to all members
            noSchedule <-
                query @Team
                    |> filterWhere (#name, "it-team-nosched" :: Text)
                    |> fetchOneOrNothing
            teamNoSched <- case noSchedule of
                Just t -> pure t
                Nothing -> newRecord @Team |> set #name "it-team-nosched" |> createRecord
            _ <-
                newRecord @TeamMember
                    |> set #teamId (get #id teamNoSched)
                    |> set #userId (get #id user)
                    |> set #teamRole "member"
                    |> createRecord
            ruleNoSched <-
                newRecord @NotificationRule
                    |> set #name "it-notify-team-fallback"
                    |> set #teamId (Just (get #id teamNoSched))
                    |> createRecord
            fallbackTargets <- resolveRuleTargets ruleNoSched
            fallbackTargets `shouldBe` [get #id user]
            -- schedule present: first member wins
            _ <-
                newRecord @OnCallSchedule
                    |> set #teamId (get #id team)
                    |> set #members (Aeson.toJSON [tshow (get #id otherUser) :: Text])
                    |> createRecord
            onCall <- currentOnCall (get #id team)
            onCall `shouldBe` Just (get #id otherUser)
            rule <-
                newRecord @NotificationRule
                    |> set #name "it-notify-team-oncall"
                    |> set #teamId (Just (get #id team))
                    |> createRecord
            targets <- resolveRuleTargets rule
            targets `shouldBe` [get #id otherUser]

        it "grafana webhook and poller paths dedupe onto one alert via shared fingerprint" do
            grafanaSource <-
                query @Source
                    |> filterWhere (#type_, "grafana" :: Text)
                    |> fetchOneOrNothing
                    >>= maybe (error "grafana source fixture missing") pure
            let webhookPayload =
                    object
                        [ "status" .= ("firing" :: Text)
                        , "alerts"
                            .= [ object
                                    [ "status" .= ("firing" :: Text)
                                    , "fingerprint" .= ("poller-dedupe" :: Text)
                                    , "labels" .= object ["severity" .= ("high" :: Text), "env" .= ("itest-env-dedupe" :: Text), "host" .= ("itest-host" :: Text)]
                                    , "annotations" .= object []
                                    ]
                               ]
                        ]
            Right [webhookEvent] <- pure (Grafana.normalize webhookPayload)
            Just alertId <- ingest grafanaSource webhookEvent
            now <- getCurrentTime
            let polled =
                    Grafana.GrafanaAmAlert
                        { amFingerprint = "poller-dedupe"
                        , amLabels = object ["severity" .= ("high" :: Text)]
                        , amAnnotations = object []
                        , amStartsAt = Nothing
                        , amEndsAt = Just now -- resolved while "webhook was down"
                        , amUpdatedAt = Just now
                        , amGeneratorUrl = Nothing
                        }
            void (ingest grafanaSource (Grafana.amAlertToNormalized now polled))
            alerts <-
                query @Alert
                    |> filterWhere (#fingerprint, "grafana:poller-dedupe" :: Text)
                    |> fetch
            length alerts `shouldBe` 1
            resolved <- fetch alertId
            resolved.status `shouldBe` "resolved"

    describe "milestone 3: context layer" do
        it "new alert enqueues EnrichAlertJob; refire does not" do
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing)
            jobs <-
                query @EnrichAlertJob
                    |> filterWhere (#alertId, alertId)
                    |> fetch
            length jobs `shouldBe` 1
            void (ingest source (testEvent fp Firing))
            jobsAfterRefire <-
                query @EnrichAlertJob
                    |> filterWhere (#alertId, alertId)
                    |> fetch
            length jobsAfterRefire `shouldBe` 1

        it "enrich job populates cmdb cache and jira links" do
            ensureMockJiraConfig
            ensureMockCmdbConfig
            source <-
                integrationSource
                    "zabbix"
                    "itest-m3-enrich"
                    ""
                    ( object
                        [ "writeBack" .= True
                        , "jiraProjects" .= (["DEV"] :: [Text])
                        , "cmdbSpaces" .= (["DEV"] :: [Text])
                        ]
                    )
            fp <- freshFingerprint
            Just alertId <-
                ingest
                    source
                    (testEvent fp Firing)
                        { host = Just "dev-host-01"
                        , checkName = Just "halemans test trigger"
                        }
            job <-
                query @EnrichAlertJob
                    |> filterWhere (#alertId, alertId)
                    |> fetchOneOrNothing
                    >>= maybe (error "enrich job missing") pure
            perform job
            alert <- fetch alertId
            let Just hostId = alert.hostId
            entry <-
                query @CmdbEntry
                    |> filterWhere (#hostId, Just hostId)
                    |> fetchOneOrNothing
                    >>= maybe (error "cmdb entry missing") pure
            entry.pageId `shouldBe` Just "1001"
            (not (Text.null entry.excerpt)) `shouldBe` True
            host <- fetch hostId
            host.cmdbPageId `shouldBe` Just "1001"
            links <-
                query @JiraLink
                    |> filterWhere (#alertId, alertId)
                    |> fetch
            case links of
                [link] -> do
                    link.ticketKey `shouldBe` "DEV-101"
                    link.origin `shouldBe` "auto"
                _ -> expectationFailure "expected exactly one auto jira link"
            -- second run: fresh cache row is served, upserts are idempotent
            perform job
            entries <-
                query @CmdbEntry
                    |> filterWhere (#hostId, Just hostId)
                    |> fetch
            length entries `shouldBe` 1
            linksAfter <-
                query @JiraLink
                    |> filterWhere (#alertId, alertId)
                    |> fetch
            length linksAfter `shouldBe` 1

        it "per-source scope override replaces the connection's jira projects" do
            ensureMockJiraConfig
            ensureMockCmdbConfig
            -- the mock jira holds only DEV tickets, so a NOPE scope (both
            -- the array key and the legacy scalar) auto-links nothing — a
            -- legitimate empty result, not an enrichment failure
            forM_
                [ object ["jiraProjects" .= (["NOPE"] :: [Text])]
                , object ["jiraProject" .= ("NOPE" :: Text)]
                ]
                \config -> do
                    suffix <- tshow <$> nextRandom
                    source <- integrationSource "zabbix" ("itest-m3-scope-" <> suffix) "" config
                    fp <- freshFingerprint
                    Just alertId <-
                        ingest
                            source
                            (testEvent fp Firing)
                                { host = Just ("itest-m3-scope-host-" <> suffix)
                                , checkName = Just "halemans test trigger"
                                }
                    job <- enrichJobFor alertId
                    perform job
                    links <-
                        query @JiraLink
                            |> filterWhere (#alertId, alertId)
                            |> fetch
                    links `shouldBe` []
                    failures <-
                        query @AlertEvent
                            |> filterWhere (#alertId, alertId)
                            |> filterWhere (#kind, "enrichment_failed" :: Text)
                            |> fetch
                    mapMaybe (payloadText "subsystem" . (get #payload)) failures `shouldBe` []

        it "enrich soft-fails per subsystem (confluence down, jira still runs)" do
            ensureMockJiraConfig
            withOnlyCmdbConfig "http://127.0.0.1:9" do
                source <-
                    integrationSource
                        "zabbix"
                        "itest-m3-softfail"
                        ""
                        ( object
                            ["writeBack" .= True]
                        )
                fp <- freshFingerprint
                -- unique host (not dev-host-01): that host's cmdb_entries
                -- row is cached by the previous test and would be served
                -- fresh instead of failing against the dead Confluence URL.
                Just alertId <-
                    ingest
                        source
                        (testEvent fp Firing)
                            { host = Just "itest-m3-softfail-host"
                            , checkName = Just "halemans test trigger"
                            }
                job <-
                    query @EnrichAlertJob
                        |> filterWhere (#alertId, alertId)
                        |> fetchOneOrNothing
                        >>= maybe (error "enrich job missing") pure
                perform job
                failures <-
                    query @AlertEvent
                        |> filterWhere (#alertId, alertId)
                        |> filterWhere (#kind, "enrichment_failed" :: Text)
                        |> fetch
                mapMaybe (payloadText "subsystem" . (get #payload)) failures `shouldBe` ["cmdb"]
                links <-
                    query @JiraLink
                        |> filterWhere (#alertId, alertId)
                        |> fetch
                length links `shouldBe` 1

        it "enrich retry carries attempts forward and stops once resolved" do
            ensureMockJiraConfig
            withOnlyCmdbConfig "http://127.0.0.1:9" do
                source <- integrationSource "zabbix" "itest-m3-retry" "" (object [])
                fp <- freshFingerprint
                Just alertId <-
                    ingest
                        source
                        (testEvent fp Firing)
                            { host = Just "itest-m3-retry-host"
                            , checkName = Just "halemans test trigger"
                            }
                job <- freshEnrichJob alertId
                perform job
                retries <-
                    query @EnrichAlertJob
                        |> filterWhere (#alertId, alertId)
                        |> orderByAsc #createdAt
                        |> fetch
                map (get #attemptsCount) retries `shouldBe` [0, 1]
                -- resolved alert: retry budget is irrelevant, no re-enqueue
                alert <- fetch alertId
                _ <- alert |> set #status "resolved" |> updateRecord
                perform (retries !! 1)
                retriesAfter <-
                    query @EnrichAlertJob
                        |> filterWhere (#alertId, alertId)
                        |> fetch
                length retriesAfter `shouldBe` 2

        it "enrich does not re-enqueue on deterministic client errors (401)" do
            oldConfluenceToken <- lookupEnv "CONFLUENCE_TOKEN"
            setEnv "CONFLUENCE_TOKEN" "wrong-token"
            flip finally (maybe (unsetEnv "CONFLUENCE_TOKEN") (setEnv "CONFLUENCE_TOKEN") oldConfluenceToken) do
                withOnlyCmdbConfig "http://127.0.0.1:18082" do
                    source <- integrationSource "zabbix" "itest-m3-4xx" "" (object [])
                    fp <- freshFingerprint
                    Just alertId <-
                        ingest
                            source
                            (testEvent fp Firing)
                                { host = Just "itest-m3-4xx-host"
                                , checkName = Just "halemans test trigger"
                                }
                    job <- freshEnrichJob alertId
                    perform job
                    -- deterministic client error (401): no re-enqueue, whatever
                    -- the other subsystems did (the dev worker may have raced us
                    -- to a cached cmdb row, so event contents are not asserted)
                    retries <-
                        query @EnrichAlertJob
                            |> filterWhere (#alertId, alertId)
                            |> fetch
                    length retries `shouldBe` 1

        it "ack enqueues write-back; webhook sources record unsupported" do
            user <- testUser
            zabbixSource <- integrationSource "zabbix" "itest-m3-wb" "" (object ["writeBack" .= True])
            fp <- freshFingerprint
            Just alertId <- ingest zabbixSource (testEvent fp Firing)
            alert <- fetch alertId
            _ <- ackAlert user alert Nothing Nothing
            attempts <-
                query @WriteBackAttempt
                    |> filterWhere (#alertId, alertId)
                    |> fetch
            case attempts of
                [attempt] -> do
                    attempt.status `shouldBe` "queued"
                    attempt.action `shouldBe` "ack"
                    jobs <-
                        query @WriteBackJob
                            |> filterWhere (#attemptId, get #id attempt)
                            |> fetch
                    length jobs `shouldBe` 1
                _ -> expectationFailure "expected exactly one write-back attempt"
            webhookSource <- integrationSource "webhook" "itest-m3-wb-wh" "" (object ["writeBack" .= True])
            fp2 <- freshFingerprint
            Just alertId2 <- ingest webhookSource (testEvent fp2 Firing)
            alert2 <- fetch alertId2
            _ <- ackAlert user alert2 Nothing Nothing
            attempts2 <-
                query @WriteBackAttempt
                    |> filterWhere (#alertId, alertId2)
                    |> fetch
            case attempts2 of
                [attempt] -> do
                    attempt.status `shouldBe` "done"
                    attempt.lastError `shouldSatisfy` maybe False ("unsupported" `Text.isInfixOf`)
                    jobs <-
                        query @WriteBackJob
                            |> filterWhere (#attemptId, get #id attempt)
                            |> fetch
                    length jobs `shouldBe` 0
                _ -> expectationFailure "expected exactly one write-back attempt"

        it "write-back retries then fails terminally" do
            user <- testUser
            source <-
                integrationSource
                    "zabbix"
                    "itest-m3-wb-retry"
                    "http://127.0.0.1:9"
                    ( object
                        [ "writeBack" .= True
                        , "tokenEnv" .= ("JIRA_TOKEN" :: Text)
                        ]
                    )
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing){externalId = Just "424242"}
            alert <- fetch alertId
            _ <- ackAlert user alert Nothing Nothing
            attempt <-
                query @WriteBackAttempt
                    |> filterWhere (#alertId, alertId)
                    |> fetchOneOrNothing
                    >>= maybe (error "write-back attempt missing") pure
            final <- retryWriteBack 6 attempt
            final.status `shouldBe` "failed"
            final.attempts `shouldBe` 5
            events <- eventKinds alertId
            events `shouldSatisfy` ("writeback_failed" `elem`)

        it "external ack mirror carries attribution and unmirrors" do
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing)
            alert <- fetch alertId
            now <- getCurrentTime
            acked <- mirrorExternalAck alert "zabbix" "admin" now
            acked.status `shouldBe` "ack"
            acked.acknowledgedBy `shouldBe` Nothing
            externalEvents <-
                query @AlertEvent
                    |> filterWhere (#alertId, alertId)
                    |> filterWhere (#kind, "external" :: Text)
                    |> fetch
            case externalEvents of
                [event] -> do
                    payloadText "action" event.payload `shouldBe` Just "ack"
                    payloadText "source" event.payload `shouldBe` Just "zabbix"
                    payloadText "actor" event.payload `shouldBe` Just "admin"
                _ -> expectationFailure "expected exactly one external event"
            wasExternal <- lastAckWasExternal acked
            wasExternal `shouldBe` True
            unacked <- mirrorExternalUnack acked "zabbix" "admin" now
            unacked.status `shouldBe` "firing"

        it "external suppress mirror mutes as source-owned and survives blackout expiry and refires" do
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing)
            alert <- fetch alertId
            now <- getCurrentTime
            muted <- mirrorExternalSuppress alert "zabbix" "admin" now
            muted.suppressed `shouldBe` True
            muted.suppressedBy `shouldBe` Just "source"
            -- blackout expiry job must not clear source-owned muting
            unsuppressExpired
            stillMuted <- fetch alertId
            stillMuted.suppressed `shouldBe` True
            stillMuted.suppressedBy `shouldBe` Just "source"
            -- a refire recomputes the blackout overlay; it must not clear
            -- or re-own source muting either
            void (ingest source (testEvent fp Firing))
            refired <- fetch alertId
            refired.suppressed `shouldBe` True
            refired.suppressedBy `shouldBe` Just "source"
            unmuted <- mirrorExternalUnsuppress refired "zabbix" "admin" now
            unmuted.suppressed `shouldBe` False
            unmuted.suppressedBy `shouldBe` Nothing
            externalEvents <-
                query @AlertEvent
                    |> filterWhere (#alertId, alertId)
                    |> filterWhere (#kind, "external" :: Text)
                    |> fetch
            let actions = map (payloadText "action" . (.payload)) externalEvents
            actions `shouldSatisfy` (elem (Just "suppress"))
            actions `shouldSatisfy` (elem (Just "unsuppress"))

        it "source suppress/unsuppress never clobbers blackout ownership" do
            source <- testSource
            void (ingest source (testEventIn "itest-env-bo-src" "itest-env-bo-src-bootstrap" Firing))
            environment <- fetchEnvironment "itest-env-bo-src"
            now <- getCurrentTime
            _ <-
                newRecord @Blackout
                    |> set #environmentId (Just (get #id environment))
                    |> set #startsAt (addUTCTime (-60) now)
                    |> set #endsAt (addUTCTime 3600 now)
                    |> set #reason "integration test"
                    |> createRecord
            fp <- freshFingerprint
            Just alertId <- ingest source (testEventIn "itest-env-bo-src" fp Firing)
            alert <- fetch alertId
            alert.suppressed `shouldBe` True
            alert.suppressedBy `shouldBe` Just "blackout"
            stillBlackout <- mirrorExternalSuppress alert "zabbix" "admin" now
            stillBlackout.suppressedBy `shouldBe` Just "blackout"
            stillMuted <- mirrorExternalUnsuppress stillBlackout "zabbix" "admin" now
            stillMuted.suppressed `shouldBe` True
            stillMuted.suppressedBy `shouldBe` Just "blackout"

        it "JiraSyncJob reflects status drift from jira" do
            ensureMockJiraConfig
            source <- integrationSource "zabbix" "itest-m3-jirasync" "" (object [])
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing)
            now <- getCurrentTime
            _ <-
                newRecord @JiraLink
                    |> set #alertId alertId
                    |> set #ticketKey "DEV-101"
                    |> set #summary "Investigate halemans test trigger on dev-host-01"
                    |> set #status "Open"
                    |> set #url "http://127.0.0.1:18083/browse/DEV-101"
                    |> set #origin "manual"
                    |> set #syncedAt now
                    |> createRecord
            let postStatus status = void (Wreq.post "http://127.0.0.1:18083/debug/issue/DEV-101/status" (object ["status" .= (status :: Text)]))
            flip finally (postStatus "Open") do
                postStatus "In Progress"
                refreshed <- syncOpenLinks
                refreshed `shouldSatisfy` (>= 1)
                link <-
                    query @JiraLink
                        |> filterWhere (#alertId, alertId)
                        |> fetchOneOrNothing
                        >>= maybe (error "jira link missing") pure
                link.status `shouldBe` "In Progress"

        it "one default dashboard per user" do
            user <- testUser
            _ <-
                newRecord @Dashboard
                    |> set #userId (get #id user)
                    |> set #name "itest-dash-1"
                    |> set #config (Aeson.toJSON ([] :: [Aeson.Value]))
                    |> set #isDefault True
                    |> createRecord
            let userUuid = unpackId (get #id user)
            result <-
                try
                    ( void
                        ( sqlExecTyped
                            [typedSql|
                INSERT INTO dashboards (user_id, name, is_default)
                VALUES (${userUuid}, 'itest-dash-2', true)
            |]
                        )
                    ) ::
                    IO (Either SomeException ())
            case result of
                Left _ -> pure ()
                Right _ -> expectationFailure "second default dashboard should violate the partial unique index"

m5Spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
m5Spec = describe "milestone 5 hardening" do
    it "retention prunes rows older than the configured window, keeps newer, idempotent" do
        _ <- newRecord @RetentionConfig |> set #rawEventsDays 1 |> set #enabled True |> createRecord
        stale <- newRecord @RawEvent |> set #payload (object []) |> createRecord
        let staleId = get #id stale
        _ <- sqlExecTyped [typedSql| UPDATE raw_events SET received_at = NOW() - INTERVAL '2 days' WHERE id = ${staleId} |]
        fresh <- newRecord @RawEvent |> set #payload (object []) |> createRecord
        perform =<< (newRecord @RetentionJob |> createRecord)
        staleGone <- query @RawEvent |> filterWhere (#id, staleId) |> fetchOneOrNothing
        staleGone `shouldBe` Nothing
        freshKept <- query @RawEvent |> filterWhere (#id, get #id fresh) |> fetchOneOrNothing
        isJust freshKept `shouldBe` True
        oldRows <- sqlQueryTyped [typedSql| SELECT count(*) FROM raw_events WHERE received_at < NOW() - INTERVAL '1 day' |]
        perform =<< (newRecord @RetentionJob |> createRecord)
        oldRowsAfter <- sqlQueryTyped [typedSql| SELECT count(*) FROM raw_events WHERE received_at < NOW() - INTERVAL '1 day' |]
        oldRowsAfter `shouldBe` (oldRows :: [Int64])
        head oldRowsAfter `shouldBe` Just 0

    it "disabled retention config skips deletion" do
        _ <- newRecord @RetentionConfig |> set #rawEventsDays 0 |> set #enabled False |> createRecord
        stale <- newRecord @RawEvent |> set #payload (object []) |> createRecord
        let staleId = get #id stale
        _ <- sqlExecTyped [typedSql| UPDATE raw_events SET received_at = NOW() - INTERVAL '2 days' WHERE id = ${staleId} |]
        perform =<< (newRecord @RetentionJob |> createRecord)
        kept <- query @RawEvent |> filterWhere (#id, staleId) |> fetchOneOrNothing
        isJust kept `shouldBe` True
        _ <- sqlExecTyped [typedSql| DELETE FROM raw_events WHERE id = ${staleId} |]
        _ <- newRecord @RetentionConfig |> set #rawEventsDays 3650 |> set #enabled True |> createRecord
        pure ()

    it "source failures raise a warning internal alert, escalate at 5, recovery resolves" do
        source <- integrationSource "webhook" "itest-webhook-health" "" (object [])
        recordFailure source "connection refused"
        Just alert <-
            query @Alert
                |> filterWhere (#fingerprint, healthFingerprint (get #id source))
                |> fetchOneOrNothing
        alert.severity `shouldBe` "warning"
        alert.status `shouldBe` "firing"
        after1 <- fetch (get #id source)
        after1.consecutiveFailures `shouldBe` 1
        isJust after1.nextPollAt `shouldBe` True
        after1.lastError `shouldBe` Just "connection refused"
        replicateM_ 4 do
            current <- fetch (get #id source)
            recordFailure current "connection refused"
        escalated <- fetch (get #id alert)
        escalated.severity `shouldBe` "high"
        events <- eventKinds (get #id alert)
        events `shouldSatisfy` ("severity_upgraded" `elem`)
        recovered <- fetch (get #id source)
        recordSuccess recovered
        reset <- fetch (get #id source)
        reset.consecutiveFailures `shouldBe` 0
        reset.nextPollAt `shouldBe` Nothing
        resolved <- fetch (get #id alert)
        resolved.status `shouldBe` "resolved"

    it "reconcile failure raises an internal alert without backoff; success resolves it" do
        source <- integrationSource "zabbix" "itest-zabbix-reconcile-health" "" (object [])
        recordReconcileFailure source "No permissions to call \"trigger.get\""
        Just alert <-
            query @Alert
                |> filterWhere (#fingerprint, reconcileFingerprint (get #id source))
                |> fetchOneOrNothing
        alert.severity `shouldBe` "warning"
        alert.status `shouldBe` "firing"
        -- polling health state untouched: no backoff, no last_error
        untouched <- fetch (get #id source)
        untouched.consecutiveFailures `shouldBe` 0
        untouched.nextPollAt `shouldBe` Nothing
        untouched.lastError `shouldBe` Nothing
        recordReconcileSuccess source
        resolved <- fetch (get #id alert)
        resolved.status `shouldBe` "resolved"
        -- success with no open alert is a no-op (no new row)
        recordReconcileSuccess source
        let fp = reconcileFingerprint (get #id source)
        count <-
            sqlQueryTyped
                [typedSql|
            SELECT count(*) FROM alerts WHERE fingerprint = ${fp}
        |]
        pure (fromMaybe 0 (head count)) `shouldReturn` 1

    it "webhook silence check flags a push source past 3x its expected interval" do
        source <- integrationSource "alertmanager" "itest-silent" "" (object ["expectedIntervalSeconds" .= (10 :: Int)])
        let sourceId = get #id source
        _ <- sqlExecTyped [typedSql| UPDATE sources SET created_at = NOW() - INTERVAL '1 hour' WHERE id = ${sourceId} |]
        checkSilence
        alert <-
            query @Alert
                |> filterWhere (#fingerprint, healthFingerprint sourceId)
                |> fetchOneOrNothing
        isJust alert `shouldBe` True

    it "webhook silence check ignores poll sources with expectedIntervalSeconds set" do
        source <- integrationSource "zabbix" "itest-silent-poll" "" (object ["expectedIntervalSeconds" .= (10 :: Int)])
        let sourceId = get #id source
        _ <- sqlExecTyped [typedSql| UPDATE sources SET created_at = NOW() - INTERVAL '1 hour' WHERE id = ${sourceId} |]
        checkSilence
        alert <-
            query @Alert
                |> filterWhere (#fingerprint, healthFingerprint sourceId)
                |> fetchOneOrNothing
        isJust alert `shouldBe` False

    it "enrichment completion re-analyzes an alert analyzed before enrichment landed, exactly once" do
        _ <- ensureTemplate
        source <- testSource
        fp <- freshFingerprint
        -- dev-host-01 + the trigger check are the subject the mocks carry
        -- context for (milestone 3): the jira auto-link lands on enrichment,
        -- after the first analysis, and changes the rendered prompt.
        Just alertId <-
            ingest
                source
                (testEvent fp Firing)
                    { host = Just "dev-host-01"
                    , checkName = Just "halemans test trigger"
                    }
        first <- latestAnalysis alertId
        performLatestJob (get #id first)
        doneFirst <- fetch (get #id first)
        doneFirst.status `shouldBe` "done"
        enrichJob <-
            query @EnrichAlertJob
                |> filterWhere (#alertId, alertId)
                |> fetchOneOrNothing
                >>= maybe (error "enrich job missing") pure
        perform enrichJob
        analyses <-
            query @LlmAnalysis
                |> filterWhere (#alertId, alertId)
                |> orderByAsc #createdAt
                |> fetch
        length analyses `shouldBe` 2
        let second = analyses !! 1
        second.errorMessage `shouldBe` Just "enrichment_retrigger"
        performLatestJob (get #id second)
        doneSecond <- fetch (get #id second)
        doneSecond.status `shouldBe` "done"
        doneSecond.dedupedFrom `shouldBe` Nothing
        doneSecond.promptHash `shouldNotBe` doneFirst.promptHash
        perform enrichJob
        analysesAgain <-
            query @LlmAnalysis
                |> filterWhere (#alertId, alertId)
                |> fetch
        length analysesAgain `shouldBe` 2

pollerLifecycleSpec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
pollerLifecycleSpec = describe "poll loop lifecycle" do
    -- Poll loops stop rescheduling when no enabled sources of their type
    -- exist; source create/enable paths re-arm them. Race note: a live dev
    -- worker mutates poll_zabbix_jobs concurrently, so exact counts are
    -- only deterministic in the sandboxed check (no worker there).
    it "stops rescheduling when no enabled zabbix sources exist" do
        void $ sqlExecTyped [typedSql| UPDATE sources SET enabled = false WHERE type = 'zabbix' |]
        void $ sqlExecTyped [typedSql| DELETE FROM poll_zabbix_jobs WHERE status = 'job_status_not_started' |]
        perform =<< (newRecord @PollZabbixJob |> createRecord)
        pending <- pendingZabbixJobs
        void $ sqlExecTyped [typedSql| UPDATE sources SET enabled = true WHERE type = 'zabbix' |]
        pending `shouldBe` 0

    it "ensurePollerForSourceType re-arms a stopped loop idempotently" do
        void $ sqlExecTyped [typedSql| DELETE FROM poll_zabbix_jobs WHERE status = 'job_status_not_started' |]
        ensurePollerForSourceType "zabbix"
        ensurePollerForSourceType "zabbix"
        pending <- pendingZabbixJobs
        pending `shouldBe` 1
        ensurePollerForSourceType "alertmanager" -- push-only type: no poller, no-op
    it "reschedules while an enabled zabbix source exists" do
        void $ sqlExecTyped [typedSql| UPDATE sources SET enabled = false WHERE type = 'zabbix' |]
        name <- ("itest-zabbix-lifecycle-" <>) . tshow <$> nextRandom
        _ <- integrationSource "zabbix" name "" (object [])
        void $ sqlExecTyped [typedSql| DELETE FROM poll_zabbix_jobs WHERE status = 'job_status_not_started' |]
        perform =<< (newRecord @PollZabbixJob |> createRecord)
        pending <- pendingZabbixJobs
        void $ sqlExecTyped [typedSql| UPDATE sources SET enabled = true WHERE type = 'zabbix' |]
        pending `shouldBe` 1

    it "stops a duplicate loop when an older poll job is running" do
        void $ sqlExecTyped [typedSql| DELETE FROM poll_zabbix_jobs WHERE status = 'job_status_not_started' |]
        void $
            sqlExecTyped
                [typedSql|
            INSERT INTO poll_zabbix_jobs (status, created_at)
            VALUES ('job_status_running', now() - interval '1 minute')
        |]
        job <- newRecord @PollZabbixJob |> createRecord
        -- Direct perform doesn't run the runner's status bookkeeping; mark
        -- the row running so it doesn't count as a pending sibling.
        let jobId = get #id job
        void $ sqlExecTyped [typedSql| UPDATE poll_zabbix_jobs SET status = 'job_status_running' WHERE id = ${jobId} |]
        perform job
        pending <- pendingZabbixJobs
        void $ sqlExecTyped [typedSql| DELETE FROM poll_zabbix_jobs WHERE status = 'job_status_running' |]
        pending `shouldBe` 0

-- | Pipeline suites: ingest/state machine (m1), correlation & teams (m2),
-- context layer (m3), hardening (m5) and the poll loop lifecycle.
spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = m1Spec >> m5Spec >> pollerLifecycleSpec
