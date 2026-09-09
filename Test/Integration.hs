module Main where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
import Generated.Types
import System.Environment (lookupEnv, getEnv, setEnv, unsetEnv)
import System.Process (callProcess, readProcess)
import Data.Aeson (object)
import Data.UUID.V4 (nextRandom)
import Control.Monad (void, replicateM_)
import Control.Exception (try, finally, SomeException)
import Data.Int (Int64)
import IHP.Job.Types (Job (..))
import IHP.FrameworkConfig (FrameworkConfig, buildFrameworkConfig)
import qualified Data.Text as Text
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseMaybe)
import qualified Network.Wreq as Wreq

import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest)
import Application.Pipeline.Actions (ackAlert, unackAlert, closeAlert)
import Application.Job.AutoClose (unackExpiredAcks, unsuppressExpired)
import Application.Job.Escalation (runDueTrackers)
import Application.Job.EnrichAlert ()
import Application.Job.LlmAnalysis ()
import Application.Job.Retention ()
import Application.Job.SourceHealth (checkSilence)
import Application.Job.PollZabbix ()
import Application.Service.PollerControl (ensurePollerForSourceType)
import Application.Service.SourceHealth (healthFingerprint, recordFailure, recordSuccess)
import Application.Service.Api.Token (newApiToken, resolveToken, hashToken)
import Application.Service.Api.Alerts (AlertFilters (..), defaultFilters, listAlertsPage, alertDetail, AlertDetail (..))
import Application.Service.Api.Auth (AuthDecision (..), authorizeToken)
import Application.Service.Api.Metrics (collectMetrics)
import Application.Service.Api.Cursor (decodeCursor)
import Network.HTTP.Types (status401, status403)
import Data.Time.Clock (getCurrentTime)
import Application.Service.Notify (currentOnCall, resolveRuleTargets)
import Application.Service.WriteBack (executeAttempt)
import Application.Service.Reconcile (mirrorExternalAck, mirrorExternalUnack, lastAckWasExternal)
import Application.Service.Jira (syncOpenLinks)
import Application.Service.Provision (applyProvisionConfig, ProvisionError (..))
import Application.Service.Llm (LlmProviderConfig (..), ToolCall (..))
import Application.Service.Llm.DbConfig (currentLlmConfig)
import Application.Service.Llm.Tools (executeToolCall)
import Application.Service.Assets.Attrs (objectAttributes)
import Application.Pipeline.Grouping (facetValue)
import Application.Helper.DashboardConfig (DashboardCard (..), MatchClause (..), FacetRef (..), MatchOp (..), decodeDashboardConfig)
import Application.Service.DashboardCards (runCardQueryGroups, CardGroup (..), expandDashboardCards, ExpandedCard (..), runCardSummary, CardSummary (..))
import Application.Job.FacetBackfill ()
import qualified Application.Connector.Grafana as Grafana
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS

-- Pipeline integration tests (design_docs/milestone_1.md §10). The check
-- derivation boots a temp PostgreSQL; we apply IHPSchema + Schema.sql +
-- Fixtures.sql ourselves before running.
main :: IO ()
main = do
    databaseUrl <- lookupEnv "DATABASE_URL" >>= \case
        Just url -> pure (cs url)
        Nothing -> error "DATABASE_URL not set. Run via `nix flake check`."
    ihpLib <- getEnv "IHP_LIB"
    -- Load the schema only when missing (the nix check pre-loads it so
    -- typedSql compile-time introspection works; manual dev runs don't).
    hasSchema <- schemaPresent databaseUrl
    if hasSchema
        then pure ()
        else callProcess "psql" [databaseUrl, "-v", "ON_ERROR_STOP=1", "-q"
            , "-f", ihpLib <> "/IHPSchema.sql"
            , "-f", "Application/Schema.sql"
            , "-f", "Application/Fixtures.sql"
            ]
    frameworkConfig <- buildFrameworkConfig noopLogger (pure ())
    withModelContext (cs databaseUrl) noopLogger \modelContext -> do
        let ?modelContext = modelContext
        let ?context = frameworkConfig
        hspec (spec >> llmSpec >> m5Spec >> pollerLifecycleSpec >> m6Spec >> m7Spec >> m8Spec >> m9Spec)

spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = describe "alert pipeline (milestone 1)" do
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
        _ <- newRecord @Blackout
            |> set #environmentId (Just (get #id environment))
            |> set #startsAt (addUTCTime (-60) now)
            |> set #endsAt (addUTCTime 3600 now)
            |> set #reason "integration test"
            |> createRecord
        Just alertId <- ingest source (testEventIn "itest-env-bo1" fp Firing)
        alert <- fetch alertId
        alert.suppressed `shouldBe` True
        events <- eventKinds alertId
        events `shouldSatisfy` ("suppressed" `elem`)
        events `shouldSatisfy` (not . ("notified" `elem`))
        pushJobs <- query @PushNotificationJob
            |> filterWhere (#alertId, alertId)
            |> fetch
        length pushJobs `shouldBe` 0

    it "blackout expiry restores the alert via unsuppressExpired" do
        source <- testSource
        fp <- freshFingerprint
        void (ingest source (testEventIn "itest-env-bo2" "itest-env-bo2-bootstrap" Firing))
        environment <- fetchEnvironment "itest-env-bo2"
        now <- getCurrentTime
        _ <- newRecord @Blackout
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

    describe "milestone 2: correlation & teams" do
        it "two alerts on the same host group under the env+host rule; rollup is worst severity + member count" do
            source <- testSource
            rule <- groupingRule "it-grp-1" "{env}/{host}"
            fp1 <- freshFingerprint
            fp2 <- freshFingerprint
            Just alertId1 <- ingest source (testEventIn "itest-env-g1" fp1 Firing)
            Just alertId2 <- ingest source ((testEventIn "itest-env-g1" fp2 Firing) { severity = "critical" })
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
            Just alertId1 <- ingest source ((testEventIn "itest-env-g3" fp1 Firing) { severity = "critical" })
            Just alertId2 <- ingest source ((testEventIn "itest-env-g3" fp2 Firing) { severity = "critical" })
            notified1 <- notifiedEvents alertId1
            notified2 <- notifiedEvents alertId2
            length notified1 `shouldBe` 1
            length notified2 `shouldBe` 0

        it "rule edit bumps version; grouped alerts keep their group and version" do
            source <- testSource
            rule <- groupingRule "it-grp-4" "{env}/{host}"
            fp <- freshFingerprint
            Just alertId <- ingest source (testEventIn "itest-env-g4" fp Firing)
            updatedRule <- rule
                |> set #groupKeyTemplate "{env}/{host}/{check}"
                |> set #version (rule.version + 1)
                |> updateRecord
            updatedRule.version `shouldBe` 2
            alert <- fetch alertId
            alert.groupedByVersion `shouldBe` Just 1
            isJust alert.groupId `shouldBe` True

        it "disabled rules never match, even at an earlier position" do
            source <- testSource
            _ <- newRecord @GroupingRule
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
            Just alertId <- ingest source (testEvent fp Firing)
                { env = Nothing, host = Nothing, service = Nothing }
            alert <- fetch alertId
            alert.groupId `shouldBe` Nothing

        it "escalation: tracker created on notify, step fires on schedule, ack cancels, unack restarts" do
            source <- testSource
            user <- testUser
            policy <- newRecord @EscalationPolicy
                |> set #name "it-policy-1"
                |> set #steps (Aeson.toJSON [object
                    [ "after_seconds" .= (0 :: Int)
                    , "target_user_id" .= tshow (get #id user)
                    ]])
                |> createRecord
            _ <- notificationRule "it-notify-esc1" (Just (get #id user)) (Just (get #id policy))
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEvent fp Firing) { severity = "critical" })
            tracker <- query @EscalationTracker
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
            policy <- newRecord @EscalationPolicy
                |> set #name "it-policy-2"
                |> set #steps (Aeson.toJSON
                    [ object ["after_seconds" .= (3600 :: Int), "target_user_id" .= tshow (get #id user)]
                    , object ["after_seconds" .= (3600 :: Int), "target_user_id" .= tshow (get #id user)]
                    ])
                |> createRecord
            _ <- notificationRule "it-notify-esc2" (Just (get #id user)) (Just (get #id policy))
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEvent fp Firing) { severity = "critical" })
            alert <- fetch alertId
            acked <- ackAlert user alert Nothing Nothing
            tracker <- query @EscalationTracker
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
            policy <- newRecord @EscalationPolicy
                |> set #name "it-policy-3"
                |> set #steps (Aeson.toJSON [object ["after_seconds" .= (0 :: Int), "target_user_id" .= tshow (get #id user)]])
                |> createRecord
            _ <- notificationRule "it-notify-esc3" (Just (get #id user)) (Just (get #id policy))
            void (ingest source (testEventIn "itest-env-bo3" "itest-env-bo3-bootstrap" Firing))
            environment <- fetchEnvironment "itest-env-bo3"
            now <- getCurrentTime
            _ <- newRecord @Blackout
                |> set #environmentId (Just (get #id environment))
                |> set #startsAt (addUTCTime (-60) now)
                |> set #endsAt (addUTCTime 3600 now)
                |> set #reason "integration test"
                |> createRecord
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn "itest-env-bo3" fp Firing) { severity = "critical" })
            trackers <- query @EscalationTracker
                |> filterWhere (#alertId, alertId)
                |> fetch
            length trackers `shouldBe` 0

        it "currentOnCall stub: first schedule member; missing schedule falls back to all team members" do
            user <- testUser
            otherUser <- newRecord @User
                |> set #email "itest2@dev"
                |> set #passwordHash "unused"
                |> createRecord
            team <- newRecord @Team
                |> set #name "it-team-1"
                |> createRecord
            _ <- newRecord @TeamMember
                |> set #teamId (get #id team)
                |> set #userId (get #id user)
                |> set #teamRole "lead"
                |> createRecord
            _ <- newRecord @TeamMember
                |> set #teamId (get #id team)
                |> set #userId (get #id otherUser)
                |> set #teamRole "member"
                |> createRecord
            -- no schedule row yet: targets fall back to all members
            noSchedule <- query @Team
                |> filterWhere (#name, "it-team-nosched" :: Text)
                |> fetchOneOrNothing
            teamNoSched <- case noSchedule of
                Just t -> pure t
                Nothing -> newRecord @Team |> set #name "it-team-nosched" |> createRecord
            _ <- newRecord @TeamMember
                |> set #teamId (get #id teamNoSched)
                |> set #userId (get #id user)
                |> set #teamRole "member"
                |> createRecord
            ruleNoSched <- newRecord @NotificationRule
                |> set #name "it-notify-team-fallback"
                |> set #teamId (Just (get #id teamNoSched))
                |> createRecord
            fallbackTargets <- resolveRuleTargets ruleNoSched
            fallbackTargets `shouldBe` [get #id user]
            -- schedule present: first member wins
            _ <- newRecord @OnCallSchedule
                |> set #teamId (get #id team)
                |> set #members (Aeson.toJSON [tshow (get #id otherUser) :: Text])
                |> createRecord
            onCall <- currentOnCall (get #id team)
            onCall `shouldBe` Just (get #id otherUser)
            rule <- newRecord @NotificationRule
                |> set #name "it-notify-team-oncall"
                |> set #teamId (Just (get #id team))
                |> createRecord
            targets <- resolveRuleTargets rule
            targets `shouldBe` [get #id otherUser]

        it "grafana webhook and poller paths dedupe onto one alert via shared fingerprint" do
            grafanaSource <- query @Source
                |> filterWhere (#type_, "grafana" :: Text)
                |> fetchOneOrNothing
                >>= maybe (error "grafana source fixture missing") pure
            let webhookPayload = object
                    [ "status" .= ("firing" :: Text)
                    , "alerts" .= [object
                        [ "status" .= ("firing" :: Text)
                        , "fingerprint" .= ("poller-dedupe" :: Text)
                        , "labels" .= object ["severity" .= ("high" :: Text), "env" .= ("itest-env-dedupe" :: Text), "host" .= ("itest-host" :: Text)]
                        , "annotations" .= object []
                        ]]
                    ]
            Right [webhookEvent] <- pure (Grafana.normalize webhookPayload)
            Just alertId <- ingest grafanaSource webhookEvent
            now <- getCurrentTime
            let polled = Grafana.GrafanaAmAlert
                    { amFingerprint = "poller-dedupe"
                    , amLabels = object ["severity" .= ("high" :: Text)]
                    , amAnnotations = object []
                    , amStartsAt = Nothing
                    , amEndsAt = Just now -- resolved while "webhook was down"
                    , amUpdatedAt = Just now
                    , amGeneratorUrl = Nothing
                    }
            void (ingest grafanaSource (Grafana.amAlertToNormalized now polled))
            alerts <- query @Alert
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
            jobs <- query @EnrichAlertJob
                |> filterWhere (#alertId, alertId)
                |> fetch
            length jobs `shouldBe` 1
            void (ingest source (testEvent fp Firing))
            jobsAfterRefire <- query @EnrichAlertJob
                |> filterWhere (#alertId, alertId)
                |> fetch
            length jobsAfterRefire `shouldBe` 1

        it "enrich job populates cmdb cache and jira links" do
            source <- integrationSource "zabbix" "itest-m3-enrich" "" (object
                [ "writeBack" .= True
                , "cmdbSpace" .= ("DEV" :: Text)
                , "jiraProject" .= ("DEV" :: Text)
                ])
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing)
                { host = Just "dev-host-01", checkName = Just "halemans test trigger" }
            job <- query @EnrichAlertJob
                |> filterWhere (#alertId, alertId)
                |> fetchOneOrNothing
                >>= maybe (error "enrich job missing") pure
            perform job
            alert <- fetch alertId
            let Just hostId = alert.hostId
            entry <- query @CmdbEntry
                |> filterWhere (#hostId, Just hostId)
                |> fetchOneOrNothing
                >>= maybe (error "cmdb entry missing") pure
            entry.pageId `shouldBe` Just "1001"
            (not (Text.null entry.excerpt)) `shouldBe` True
            host <- fetch hostId
            host.cmdbPageId `shouldBe` Just "1001"
            links <- query @JiraLink
                |> filterWhere (#alertId, alertId)
                |> fetch
            case links of
                [link] -> do
                    link.ticketKey `shouldBe` "DEV-101"
                    link.origin `shouldBe` "auto"
                _ -> expectationFailure "expected exactly one auto jira link"
            -- second run: fresh cache row is served, upserts are idempotent
            perform job
            entries <- query @CmdbEntry
                |> filterWhere (#hostId, Just hostId)
                |> fetch
            length entries `shouldBe` 1
            linksAfter <- query @JiraLink
                |> filterWhere (#alertId, alertId)
                |> fetch
            length linksAfter `shouldBe` 1

        it "enrich soft-fails per subsystem (confluence down, jira still runs)" do
            oldConfluenceUrl <- lookupEnv "HALEMANS_CONFLUENCE_URL"
            setEnv "HALEMANS_CONFLUENCE_URL" "http://127.0.0.1:9"
            flip finally (maybe (unsetEnv "HALEMANS_CONFLUENCE_URL") (setEnv "HALEMANS_CONFLUENCE_URL") oldConfluenceUrl) do
                source <- integrationSource "zabbix" "itest-m3-softfail" "" (object
                    [ "writeBack" .= True
                    , "cmdbSpace" .= ("DEV" :: Text)
                    , "jiraProject" .= ("DEV" :: Text)
                    ])
                fp <- freshFingerprint
                -- unique host (not dev-host-01): that host's cmdb_entries
                -- row is cached by the previous test and would be served
                -- fresh instead of failing against the dead Confluence URL.
                Just alertId <- ingest source (testEvent fp Firing)
                    { host = Just "itest-m3-softfail-host", checkName = Just "halemans test trigger" }
                job <- query @EnrichAlertJob
                    |> filterWhere (#alertId, alertId)
                    |> fetchOneOrNothing
                    >>= maybe (error "enrich job missing") pure
                perform job
                failures <- query @AlertEvent
                    |> filterWhere (#alertId, alertId)
                    |> filterWhere (#kind, "enrichment_failed" :: Text)
                    |> fetch
                mapMaybe (payloadText "subsystem" . (get #payload)) failures `shouldBe` ["cmdb"]
                links <- query @JiraLink
                    |> filterWhere (#alertId, alertId)
                    |> fetch
                length links `shouldBe` 1

        it "ack enqueues write-back; webhook sources record unsupported" do
            user <- testUser
            zabbixSource <- integrationSource "zabbix" "itest-m3-wb" "" (object ["writeBack" .= True])
            fp <- freshFingerprint
            Just alertId <- ingest zabbixSource (testEvent fp Firing)
            alert <- fetch alertId
            _ <- ackAlert user alert Nothing Nothing
            attempts <- query @WriteBackAttempt
                |> filterWhere (#alertId, alertId)
                |> fetch
            case attempts of
                [attempt] -> do
                    attempt.status `shouldBe` "queued"
                    attempt.action `shouldBe` "ack"
                    jobs <- query @WriteBackJob
                        |> filterWhere (#attemptId, get #id attempt)
                        |> fetch
                    length jobs `shouldBe` 1
                _ -> expectationFailure "expected exactly one write-back attempt"
            webhookSource <- integrationSource "webhook" "itest-m3-wb-wh" "" (object ["writeBack" .= True])
            fp2 <- freshFingerprint
            Just alertId2 <- ingest webhookSource (testEvent fp2 Firing)
            alert2 <- fetch alertId2
            _ <- ackAlert user alert2 Nothing Nothing
            attempts2 <- query @WriteBackAttempt
                |> filterWhere (#alertId, alertId2)
                |> fetch
            case attempts2 of
                [attempt] -> do
                    attempt.status `shouldBe` "done"
                    attempt.lastError `shouldSatisfy` maybe False ("unsupported" `Text.isInfixOf`)
                    jobs <- query @WriteBackJob
                        |> filterWhere (#attemptId, get #id attempt)
                        |> fetch
                    length jobs `shouldBe` 0
                _ -> expectationFailure "expected exactly one write-back attempt"

        it "write-back retries then fails terminally" do
            user <- testUser
            source <- integrationSource "zabbix" "itest-m3-wb-retry" "http://127.0.0.1:9" (object
                [ "writeBack" .= True
                , "tokenEnv" .= ("JIRA_TOKEN" :: Text)
                ])
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing) { externalId = Just "424242" }
            alert <- fetch alertId
            _ <- ackAlert user alert Nothing Nothing
            attempt <- query @WriteBackAttempt
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
            externalEvents <- query @AlertEvent
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

        it "JiraSyncJob reflects status drift from jira" do
            source <- integrationSource "zabbix" "itest-m3-jirasync" "" (object ["jiraProject" .= ("DEV" :: Text)])
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing)
            now <- getCurrentTime
            _ <- newRecord @JiraLink
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
                link <- query @JiraLink
                    |> filterWhere (#alertId, alertId)
                    |> fetchOneOrNothing
                    >>= maybe (error "jira link missing") pure
                link.status `shouldBe` "In Progress"

        it "one default dashboard per user" do
            user <- testUser
            _ <- newRecord @Dashboard
                |> set #userId (get #id user)
                |> set #name "itest-dash-1"
                |> set #config (Aeson.toJSON ([] :: [Aeson.Value]))
                |> set #isDefault True
                |> createRecord
            let userUuid = unpackId (get #id user)
            result <- try (void (sqlExecTyped [typedSql|
                INSERT INTO dashboards (user_id, name, is_default)
                VALUES (${userUuid}, 'itest-dash-2', true)
            |])) :: IO (Either SomeException ())
            case result of
                Left _ -> pure ()
                Right _ -> expectationFailure "second default dashboard should violate the partial unique index"

-- LLM enrichment (design_docs/milestone_4.md §10): against the mock
-- OpenAI-compatible server on 18084 (launched by the check; deterministic
-- completions with /debug/fail backdoors for 429/500/malformed).
llmSpec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
llmSpec = describe "llm enrichment (milestone 4)" do
    it "a new alert enqueues an analysis; the job completes against the mock" do
        _ <- ensureTemplate
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
            { title = "disk pressure on itest-host", description = "disk usage above 90%" }
        analysis <- latestAnalysis alertId
        analysis.status `shouldBe` "queued"
        -- refire does not enqueue another analysis (milestone_4.md §4)
        void (ingest source (testEvent fp Firing))
        analyses <- query @LlmAnalysis
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
        jobs <- query @LlmAnalysisJob
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
        _ <- newRecord @LlmFeedback
            |> set #analysisId (get #id analysis)
            |> set #userId (get #id user)
            |> set #score 1
            |> createRecord
        duplicate <- try (void (newRecord @LlmFeedback
            |> set #analysisId (get #id analysis)
            |> set #userId (get #id user)
            |> set #score (-1)
            |> createRecord)) :: IO (Either SomeException ())
        case duplicate of
            Left _ -> pure ()
            Right _ -> expectationFailure "duplicate feedback should violate the unique index"
        existing <- query @LlmFeedback
            |> filterWhere (#analysisId, get #id analysis)
            |> filterWhere (#userId, get #id user)
            |> fetchOneOrNothing
            >>= maybe (error "feedback missing") pure
        void (existing |> set #score (-1) |> updateRecord)
        votes <- query @LlmFeedback
            |> filterWhere (#analysisId, get #id analysis)
            |> fetch
        map (get #score) votes `shouldBe` [-1]

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
        Just alert <- query @Alert
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

    it "webhook silence check flags a push source past 3x its expected interval" do
        source <- integrationSource "alertmanager" "itest-silent" "" (object ["expectedIntervalSeconds" .= (10 :: Int)])
        let sourceId = get #id source
        _ <- sqlExecTyped [typedSql| UPDATE sources SET created_at = NOW() - INTERVAL '1 hour' WHERE id = ${sourceId} |]
        checkSilence
        alert <- query @Alert
            |> filterWhere (#fingerprint, healthFingerprint sourceId)
            |> fetchOneOrNothing
        isJust alert `shouldBe` True

    it "enrichment completion re-analyzes an alert analyzed before enrichment landed, exactly once" do
        _ <- ensureTemplate
        source <- testSource
        fp <- freshFingerprint
        -- dev-host-01 + the trigger check are the subject the mocks carry
        -- context for (milestone 3): the jira auto-link lands on enrichment,
        -- after the first analysis, and changes the rendered prompt.
        Just alertId <- ingest source (testEvent fp Firing)
            { host = Just "dev-host-01", checkName = Just "halemans test trigger" }
        first <- latestAnalysis alertId
        performLatestJob (get #id first)
        doneFirst <- fetch (get #id first)
        doneFirst.status `shouldBe` "done"
        enrichJob <- query @EnrichAlertJob
            |> filterWhere (#alertId, alertId)
            |> fetchOneOrNothing
            >>= maybe (error "enrich job missing") pure
        perform enrichJob
        analyses <- query @LlmAnalysis
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
        analysesAgain <- query @LlmAnalysis
            |> filterWhere (#alertId, alertId)
            |> fetch
        length analysesAgain `shouldBe` 2

ensureTemplate :: (?modelContext :: ModelContext) => IO LlmPromptTemplate
ensureTemplate = do
    existing <- query @LlmPromptTemplate
        |> filterWhere (#name, "alert_enrichment" :: Text)
        |> filterWhere (#version, 1)
        |> fetchOneOrNothing
    case existing of
        Just template -> pure template
        Nothing -> newRecord @LlmPromptTemplate
            |> set #name "alert_enrichment"
            |> set #version 1
            |> set #body "Alert: {{alert.title}}\n{{alert.description}}\nEvents:\n{{events}}\nCMDB:\n{{cmdb_excerpt}}\nSimilar:\n{{similar_alerts}}\nJira:\n{{jira_links}}"
            |> set #active True
            |> createRecord

latestAnalysis :: (?modelContext :: ModelContext) => Id Alert -> IO LlmAnalysis
latestAnalysis alertId = query @LlmAnalysis
    |> filterWhere (#alertId, alertId)
    |> orderByDesc #createdAt
    |> limit 1
    |> fetchOneOrNothing
    >>= maybe (error "llm analysis missing") pure

enqueueAnalysis :: (?modelContext :: ModelContext) => Id Alert -> IO LlmAnalysis
enqueueAnalysis alertId = do
    analysis <- newRecord @LlmAnalysis
        |> set #alertId alertId
        |> createRecord
    void do
        newRecord @LlmAnalysisJob
            |> set #analysisId (get #id analysis)
            |> createRecord
    pure analysis

performLatestJob :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Id LlmAnalysis -> IO ()
performLatestJob analysisId = do
    job <- query @LlmAnalysisJob
        |> filterWhere (#analysisId, analysisId)
        |> orderByDesc #createdAt
        |> limit 1
        |> fetchOneOrNothing
        >>= maybe (error "llm job missing") pure
    perform job

counterRequestsAfter :: (?modelContext :: ModelContext) => Text -> IO Int
counterRequestsAfter provider = do
    rows <- sqlQueryTyped [typedSql|
        SELECT requests FROM llm_budget_counters
        WHERE provider = ${provider} AND day = CURRENT_DATE
    |]
    pure (fromMaybe 0 (head rows))

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

pendingZabbixJobs :: (?modelContext :: ModelContext) => IO Int64
pendingZabbixJobs = do
    rows <- sqlQueryTyped [typedSql|
        SELECT count(*) FROM poll_zabbix_jobs WHERE status = 'job_status_not_started'
    |]
    pure (fromMaybe 0 (head rows))

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
                Deny {} -> expectationFailure "expected Allow"
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
            Just a2 <- ingest source ((testEventIn envName fp2 Firing) { severity = "critical" })
            void (ingest source (testEventIn envName fp2 Resolved))
            let idsOf filters = map (get #id) . fst <$> listAlertsPage filters
            idsOf defaultFilters { afEnvironment = envName } `shouldReturn` [a2, a1]
            idsOf defaultFilters { afEnvironment = envName, afStatus = "firing" } `shouldReturn` [a1]
            idsOf defaultFilters { afEnvironment = envName, afStatus = "resolved" } `shouldReturn` [a2]
            idsOf defaultFilters { afEnvironment = envName, afSeverity = "critical" } `shouldReturn` [a2]
            idsOf defaultFilters { afEnvironment = envName, afFingerprint = fp1 } `shouldReturn` [a1]
            idsOf defaultFilters { afEnvironment = envName, afHost = "itest-host" } `shouldReturn` [a2, a1]
            idsOf defaultFilters { afEnvironment = envName, afHost = "no-such-host" } `shouldReturn` []
            idsOf defaultFilters { afEnvironment = envName, afService = "itest-svc" } `shouldReturn` [a2, a1]
            idsOf defaultFilters { afEnvironment = "no-such-env" } `shouldReturn` []

        it "paginates with a stable cursor and ends with next_cursor Nothing" do
            source <- testSource
            envName <- ("m6env-" <>) . tshow <$> nextRandom
            let fire = do fp <- freshFingerprint; ingest source (testEventIn envName fp Firing)
            Just a1 <- fire
            Just a2 <- fire
            Just a3 <- fire
            (page1, next1) <- listAlertsPage defaultFilters { afEnvironment = envName, afLimit = 2 }
            map (get #id) page1 `shouldBe` [a3, a2]
            -- An alert inserted between pages is newer than the cursor and
            -- must not appear on the next page (keyset stability).
            Just _ <- fire
            (page2, next2) <- listAlertsPage defaultFilters { afEnvironment = envName, afLimit = 2, afCursor = decodeCursor =<< next1 }
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
            idsOf defaultFilters { afEnvironment = envName, afSince = addUTCTime (-60) now } `shouldReturn` []
            idsOf defaultFilters { afEnvironment = envName, afUntil = addUTCTime (-60) now } `shouldReturn` [a1]
            idsOf defaultFilters { afEnvironment = envName, afSince = addUTCTime (-7200) now, afUntil = now } `shouldReturn` [a1]

    describe "alertDetail" do
        it "returns the ordered timeline, group membership and latest done analysis" do
            source <- testSource
            envName <- ("m6env-" <>) . tshow <$> nextRandom
            fp <- freshFingerprint
            Just a1 <- ingest source (testEventIn envName fp Firing)
            void (ingest source (testEventIn envName fp Firing))
            group <- newRecord @AlertGroup
                |> set #groupKey fp
                |> set #title "m6 group"
                |> set #status "firing"
                |> set #worstSeverity "warning"
                |> set #memberCount 1
                |> createRecord
            alert <- fetch a1
            void (alert |> set #groupId (Just (get #id group)) |> updateRecord)
            void $ newRecord @LlmAnalysis
                |> set #alertId a1
                |> set #status "done"
                |> set #resultMd "old analysis"
                |> set #createdAt (UTCTime (fromGregorian 2998 1 1) 0)
                |> createRecord
            void $ newRecord @LlmAnalysis
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
    denyStatus Allow {} = Nothing

m6User :: (?modelContext :: ModelContext) => [Text] -> IO User
m6User privileges = do
    suffix <- tshow <$> nextRandom
    role <- newRecord @Role
        |> set #name ("m6-" <> suffix)
        |> set #privileges privileges
        |> createRecord
    user <- newRecord @User
        |> set #email ("m6-" <> suffix <> "@dev")
        |> set #passwordHash "unused"
        |> createRecord
    void $ newRecord @UserRole
        |> set #userId (get #id user)
        |> set #roleId (get #id role)
        |> createRecord
    pure user

mockFail :: Text -> Int -> IO ()
mockFail kind times = void (Wreq.post ("http://127.0.0.1:18084/debug/fail/" <> cs kind) (object ["times" .= times]))

schemaPresent :: String -> IO Bool
schemaPresent databaseUrl = do
    output <- readProcess "psql" [databaseUrl, "-tA", "-c", "SELECT to_regclass('public.alerts') IS NOT NULL"] ""
    pure (output == "t\n")

testSource :: (?modelContext :: ModelContext) => IO Source
testSource = query @Source
    |> filterWhere (#type_, "alertmanager" :: Text)
    |> fetchOneOrNothing
    >>= maybe (error "alertmanager source fixture missing") pure

integrationSource :: (?modelContext :: ModelContext) => Text -> Text -> Text -> Aeson.Value -> IO Source
integrationSource sourceType name baseUrl config = newRecord @Source
    |> set #type_ sourceType
    |> set #name name
    |> set #baseUrl baseUrl
    |> set #config config
    |> createRecord

payloadText :: Text -> Aeson.Value -> Maybe Text
payloadText key = parseMaybe (Aeson.withObject "payload" (\o -> o Aeson..: Key.fromText key))

retryWriteBack :: (?modelContext :: ModelContext) => Int -> WriteBackAttempt -> IO WriteBackAttempt
retryWriteBack 0 attempt = pure attempt
retryWriteBack n attempt
    | attempt.status == "failed" || attempt.status == "done" = pure attempt
    | otherwise = do
        executeAttempt attempt
        updated <- fetch (get #id attempt)
        retryWriteBack (n - 1) updated

fetchEnvironment :: (?modelContext :: ModelContext) => Text -> IO Environment
fetchEnvironment name = query @Environment
    |> filterWhere (#name, name)
    |> fetchOneOrNothing
    >>= maybe (error "environment missing") pure

testUser :: (?modelContext :: ModelContext) => IO User
testUser = do
    existing <- query @User
        |> filterWhere (#email, "itest@dev" :: Text)
        |> fetchOneOrNothing
    case existing of
        Just user -> pure user
        Nothing -> newRecord @User
            |> set #email "itest@dev"
            |> set #passwordHash "unused"
            |> createRecord

freshFingerprint :: IO Text
freshFingerprint = ("itest:" <>) . tshow <$> nextRandom

testEvent :: Text -> SourceStatus -> NormalizedEvent
testEvent = testEventIn "itest-env"

testEventIn :: Text -> Text -> SourceStatus -> NormalizedEvent
testEventIn envName fp status = NormalizedEvent
    { fingerprint = fp
    , externalId = Nothing
    , status
    , severity = "warning"
    , title = "integration test alert"
    , description = ""
    , env = Just envName
    , host = Just "itest-host"
    , service = Just "itest-svc"
    , checkName = Just "itest-check"
    , labels = object []
    , annotations = object []
    , startedAt = Nothing
    , sourceUrl = Nothing
    }

eventKinds :: (?modelContext :: ModelContext) => Id Alert -> IO [Text]
eventKinds alertId = map (get #kind) <$> (query @AlertEvent
    |> filterWhere (#alertId, alertId)
    |> orderByAsc #createdAt
    |> fetch)

notifiedEvents :: (?modelContext :: ModelContext) => Id Alert -> IO [AlertEvent]
notifiedEvents alertId = query @AlertEvent
    |> filterWhere (#alertId, alertId)
    |> filterWhere (#kind, "notified" :: Text)
    |> fetch

groupingRule :: (?modelContext :: ModelContext) => Text -> Text -> IO GroupingRule
groupingRule name template = newRecord @GroupingRule
    |> set #name name
    |> set #position 10
    |> set #enabled True
    |> set #match (object [])
    |> set #groupKeyTemplate template
    |> createRecord

notificationRule :: (?modelContext :: ModelContext) => Text -> Maybe (Id User) -> Maybe (Id EscalationPolicy) -> IO NotificationRule
notificationRule name userRef policyRef = newRecord @NotificationRule
    |> set #name name
    |> set #position 50
    |> set #enabled True
    |> set #match (object [])
    |> set #severityThreshold "high"
    |> set #userId userRef
    |> set #channel "browser_push"
    |> set #throttleSeconds 300
    |> set #escalationPolicyId policyRef
    |> createRecord

-- Milestone 7: declarative provisioning (design_docs/milestone_7.md §9).
-- Keep-lists for strict tests are built from current DB rows so the specs are
-- order-independent and safe against a populated dev DB.
m7Spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
m7Spec = describe "provisioning (milestone 7)" do
    it "applies a full config idempotently (users, sources, teams, llm)" do
        suffix <- tshow <$> nextRandom
        setEnv "M7_TEST_HOOK_TOKEN" ("tok-" <> cs suffix)
        let email = "m7-" <> suffix <> "@dev"
            sourceName = "m7-src-" <> suffix
            teamName = "m7-team-" <> suffix
            provider = "m7-llm-" <> suffix
            templateName = "m7_tmpl_" <> Text.replace "-" "_" suffix
            token = "tok-" <> suffix
            config = object
                [ "users" .= object ["items" .= [object
                    [ "email" .= email, "passwordHash" .= ("sha256|17|a|b" :: Text)
                    , "displayName" .= ("M7 " <> suffix)
                    , "roles" .= (["m7-role-" <> suffix] :: [Text])
                    , "settings" .= object ["theme" .= ("latte" :: Text)] ]]]
                , "sources" .= object ["items" .= [object
                    [ "type" .= ("webhook" :: Text), "name" .= sourceName
                    , "enabled" .= False
                    , "webhookTokens" .= [object ["tokenEnv" .= ("M7_TEST_HOOK_TOKEN" :: Text)]] ]]]
                , "teams" .= object ["items" .= [object
                    [ "name" .= teamName, "description" .= ("m7 team " <> suffix)
                    , "hostGroups" .= (["Linux servers"] :: [Text])
                    , "members" .= [object ["email" .= email, "role" .= ("lead" :: Text)]] ]]]
                , "llm" .= object ["items" .= [object
                    [ "providerName" .= provider, "endpoint" .= ("http://m7.example" :: Text)
                    , "model" .= ("m7-model" :: Text), "enabled" .= False
                    , "promptTemplates" .= [object
                        [ "name" .= templateName, "version" .= (1 :: Int)
                        , "body" .= ("body one" :: Text), "active" .= True ]] ]]]
                ]
        m7Apply config
        m7Apply config
        users <- query @User |> filterWhere (#email, email) |> fetch
        length users `shouldBe` 1
        user <- case users of
            [user] -> pure user
            _ -> expectationFailure "expected exactly one provisioned user" >> error "unreachable"
        user.displayName `shouldBe` "M7 " <> suffix
        roles <- sqlQueryTyped [typedSql|
            SELECT r.name FROM user_roles ur JOIN roles r ON r.id = ur.role_id
            JOIN users u ON u.id = ur.user_id WHERE u.email = ${email}
        |]
        roles `shouldBe` ["m7-role-" <> suffix]
        sources <- query @Source |> filterWhere (#name, sourceName) |> fetch
        source <- case sources of
            [source] -> pure source
            _ -> expectationFailure "expected exactly one provisioned source" >> error "unreachable"
        hookToken <- query @WebhookToken |> filterWhere (#token, token) |> fetchOneOrNothing
        fmap (get #sourceId) hookToken `shouldBe` Just (get #id source)
        teams <- query @Team |> filterWhere (#name, teamName) |> fetch
        team <- case teams of
            [team] -> pure team
            _ -> expectationFailure "expected exactly one provisioned team" >> error "unreachable"
        members <- query @TeamMember |> filterWhere (#teamId, get #id team) |> fetch
        map (get #teamRole) members `shouldBe` ["lead"]
        llmConfigs <- query @LlmConfig |> filterWhere (#providerName, provider) |> fetch
        length llmConfigs `shouldBe` 1
        templates <- query @LlmPromptTemplate |> filterWhere (#name, templateName) |> fetch
        map (\t -> (t.version, t.active)) templates `shouldBe` [(1, True)]

    it "re-applies changed password_hash, enabled and member role in place" do
        suffix <- tshow <$> nextRandom
        let email = "m7-" <> suffix <> "@dev"
            sourceName = "m7-src-" <> suffix
            teamName = "m7-team-" <> suffix
            config hash enabled role = object
                [ "users" .= object ["items" .= [object ["email" .= email, "passwordHash" .= hash]]]
                , "sources" .= object ["items" .= [object ["type" .= ("webhook" :: Text), "name" .= sourceName, "enabled" .= enabled]]]
                , "teams" .= object ["items" .= [object ["name" .= teamName, "members" .= [object ["email" .= email, "role" .= role]]]]]
                ]
        m7Apply (config ("hash-one" :: Text) False ("member" :: Text))
        m7Apply (config ("hash-two" :: Text) True ("lead" :: Text))
        user <- query @User |> filterWhere (#email, email) |> fetchOneOrNothing >>= maybe (error "user missing") pure
        user.passwordHash `shouldBe` "hash-two"
        source <- query @Source |> filterWhere (#name, sourceName) |> fetchOneOrNothing >>= maybe (error "source missing") pure
        source.enabled `shouldBe` True
        team <- query @Team |> filterWhere (#name, teamName) |> fetchOneOrNothing >>= maybe (error "team missing") pure
        members <- query @TeamMember |> filterWhere (#teamId, get #id team) |> fetch
        map (get #teamRole) members `shouldBe` ["lead"]

    it "merges user settings instead of replacing them" do
        suffix <- tshow <$> nextRandom
        let email = "m7-" <> suffix <> "@dev"
            config = object ["users" .= object ["items" .= [object
                ["email" .= email, "passwordHash" .= ("x" :: Text), "settings" .= object ["theme" .= ("frappe" :: Text)]]]]]
        m7Apply config
        let patch = object ["ui_note" .= ("kept" :: Text)]
        void $ sqlExecTyped [typedSql| UPDATE users SET settings = settings || ${patch} WHERE email = ${email} |]
        m7Apply config
        user <- query @User |> filterWhere (#email, email) |> fetchOneOrNothing >>= maybe (error "user missing") pure
        payloadText "theme" user.settings `shouldBe` Just "frappe"
        payloadText "ui_note" user.settings `shouldBe` Just "kept"

    it "re-provision without team keys preserves UI-set host_groups, description and defaults" do
        suffix <- tshow <$> nextRandom
        let teamName = "m7-team-" <> suffix
            uiGroups = Aeson.toJSON (["UI group"] :: [Text])
            uiDescription = "ui-edited " <> suffix :: Text
        m7Apply (object ["teams" .= object ["items" .= [object
            [ "name" .= teamName
            , "description" .= ("original" :: Text)
            , "hostGroups" .= (["Linux servers"] :: [Text])
            , "defaults" .= object ["k" .= ("v" :: Text)]
            ]]]])
        -- Simulate UI edits on top of the provisioned values.
        void $ sqlExecTyped [typedSql|
            UPDATE teams SET host_groups = ${uiGroups}, description = ${uiDescription}
            WHERE name = ${teamName}
        |]
        m7Apply (object ["teams" .= object ["items" .= [object ["name" .= teamName]]]])
        team <- query @Team |> filterWhere (#name, teamName) |> fetchOneOrNothing >>= maybe (error "team missing") pure
        get #description team `shouldBe` uiDescription
        get #hostGroups team `shouldBe` uiGroups
        payloadText "k" (get #defaults team) `shouldBe` Just "v"
        -- An explicit empty list still clears the groups.
        m7Apply (object ["teams" .= object ["items" .= [object
            [ "name" .= teamName, "hostGroups" .= ([] :: [Text]) ]]]])
        cleared <- query @Team |> filterWhere (#name, teamName) |> fetchOneOrNothing >>= maybe (error "team missing") pure
        get #hostGroups cleared `shouldBe` Aeson.toJSON ([] :: [Text])

    it "aborts on an unresolvable team member email" do
        suffix <- tshow <$> nextRandom
        m7Apply (object ["teams" .= object ["items" .= [object
            ["name" .= ("m7-team-" <> suffix), "members" .= [object ["email" .= ("m7-missing-" <> suffix <> "@dev")]]]]]])
            `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf "does not resolve to any user" msg

    it "aborts on an unset tokenEnv reference" do
        suffix <- tshow <$> nextRandom
        unsetEnv "M7_MISSING_TOKEN"
        m7Apply (object ["sources" .= object ["items" .= [object
            [ "type" .= ("zabbix" :: Text), "name" .= ("m7-src-" <> suffix)
            , "config" .= object ["tokenEnv" .= ("M7_MISSING_TOKEN" :: Text)] ]]]])
            `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf "M7_MISSING_TOKEN" msg

    it "imports zabbix host groups from a local file, replacing the cache" do
        suffix <- tshow <$> nextRandom
        let sourceName = "m7-zbx-" <> suffix
            groupsPath :: Text
            groupsPath = "/tmp/halemans-m7-groups-" <> cs suffix <> ".json"
            config = object ["sources" .= object ["items" .= [object
                [ "type" .= ("zabbix" :: Text), "name" .= sourceName
                , "hostGroupsFile" .= groupsPath ]]]]
        LBS.writeFile (cs groupsPath) (Aeson.encode
            [ object ["groupid" .= ("2" :: Text), "name" .= ("Linux servers" :: Text)]
            , object ["groupid" .= ("5" :: Text), "name" .= ("Databases" :: Text)] ])
        m7Apply config
        source <- query @Source |> filterWhere (#name, sourceName) |> fetchOneOrNothing >>= maybe (error "source missing") pure
        rows <- query @ZabbixHostGroup |> filterWhere (#sourceId, get #id source) |> orderByAsc #name |> fetch
        map (\g -> (g.groupId, g.name)) rows `shouldBe` [("5", "Databases"), ("2", "Linux servers")]
        -- A hostgroup.get response dump works verbatim and re-apply replaces.
        LBS.writeFile (cs groupsPath) (Aeson.encode $ object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "result" .= [object ["groupid" .= ("7" :: Text), "name" .= ("Hypervisors" :: Text)]]
            , "id" .= (1 :: Int) ])
        m7Apply config
        rows' <- query @ZabbixHostGroup |> filterWhere (#sourceId, get #id source) |> fetch
        map (\g -> (g.groupId, g.name)) rows' `shouldBe` [("7", "Hypervisors")]

    it "aborts when hostGroupsFile is unreadable" do
        suffix <- tshow <$> nextRandom
        m7Apply (object ["sources" .= object ["items" .= [object
            [ "type" .= ("zabbix" :: Text), "name" .= ("m7-zbx-" <> suffix)
            , "hostGroupsFile" .= ("/tmp/halemans-m7-no-such-" <> suffix) ]]]])
            `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf "cannot read hostGroupsFile" msg

    it "currentLlmConfig prefers the enabled DB row, env is the fallback" do
        oldEndpoint <- lookupEnv "LLM_ENDPOINT"
        oldModel <- lookupEnv "LLM_MODEL"
        flip finally (restoreEnv "LLM_ENDPOINT" oldEndpoint >> restoreEnv "LLM_MODEL" oldModel) do
            setEnv "LLM_ENDPOINT" "http://m7-env.example"
            setEnv "LLM_MODEL" "env-model"
            void $ sqlExecTyped [typedSql| DELETE FROM llm_configs |]
            fromEnv <- currentLlmConfig
            fmap (.endpoint) fromEnv `shouldBe` Just "http://m7-env.example"
            suffix <- tshow <$> nextRandom
            let provider = "m7-llm-" <> suffix
            m7Apply (object ["llm" .= object ["items" .= [object
                [ "providerName" .= provider, "endpoint" .= ("http://m7-db.example" :: Text)
                , "model" .= ("m7-db-model" :: Text), "enabled" .= True ]]]])
            fromDb <- currentLlmConfig
            fmap (.endpoint) fromDb `shouldBe` Just "http://m7-db.example"
            fmap (.providerName) fromDb `shouldBe` Just provider

    it "prompt template provisioning swaps the active version" do
        suffix <- tshow <$> nextRandom
        let provider = "m7-llm-" <> suffix
            templateName = "m7_tmpl_" <> Text.replace "-" "_" suffix
            config version = object ["llm" .= object ["items" .= [object
                [ "providerName" .= provider, "endpoint" .= ("http://m7.example" :: Text)
                , "model" .= ("m" :: Text)
                , "promptTemplates" .= [object
                    [ "name" .= templateName, "version" .= version
                    , "body" .= ("body" :: Text), "active" .= True ]] ]]]]
        m7Apply (config (1 :: Int))
        m7Apply (config (2 :: Int))
        m7Apply (config (2 :: Int))
        templates <- query @LlmPromptTemplate
            |> filterWhere (#name, templateName)
            |> orderByAsc #version
            |> fetch
        map (\t -> (t.version, t.active)) templates `shouldBe` [(1, False), (2, True)]

    it "strict teams deletes absent teams and prunes members of kept teams" do
        suffix <- tshow <$> nextRandom
        let doomedName = "m7-doomed-" <> suffix
            keepName = "m7-keep-" <> suffix
        user1 <- m7User ("m7-a-" <> suffix <> "@dev")
        user2 <- m7User ("m7-b-" <> suffix <> "@dev")
        doomed <- newRecord @Team |> set #name doomedName |> createRecord
        void $ newRecord @TeamMember |> set #teamId (get #id doomed) |> set #userId (get #id user1) |> createRecord
        keep <- newRecord @Team |> set #name keepName |> createRecord
        void $ newRecord @TeamMember |> set #teamId (get #id keep) |> set #userId (get #id user1) |> set #teamRole "lead" |> createRecord
        void $ newRecord @TeamMember |> set #teamId (get #id keep) |> set #userId (get #id user2) |> createRecord
        keepItems <- m7TeamKeepItems [doomedName, keepName]
        let keepItem = object ["name" .= keepName, "members" .= [object ["email" .= get #email user1, "role" .= ("lead" :: Text)]]]
        m7Apply (object ["teams" .= object ["strict" .= True, "items" .= (keepItems <> [keepItem])]])
        query @Team |> filterWhere (#name, doomedName) |> fetch `shouldReturn` []
        members <- query @TeamMember |> filterWhere (#teamId, get #id keep) |> fetch
        map (get #userId) members `shouldBe` [get #id user1]

    it "strict llm deletes absent providers and unreferenced template versions" do
        suffix <- tshow <$> nextRandom
        let templateName = "m7_strict_" <> Text.replace "-" "_" suffix
        keepProviders <- m7LlmKeepItems
        void $ newRecord @LlmConfig
            |> set #providerName ("m7-doomed-llm-" <> suffix)
            |> set #endpoint "http://doomed.example"
            |> set #model "m"
            |> createRecord
        v1 <- newRecord @LlmPromptTemplate |> set #name templateName |> set #version 1 |> set #body "one" |> createRecord
        void $ newRecord @LlmPromptTemplate |> set #name templateName |> set #version 2 |> set #body "two" |> createRecord
        m7Apply (object ["llm" .= object ["strict" .= True, "items" .= (keepProviders <> [object
            [ "providerName" .= ("m7-strict-llm-" <> suffix), "endpoint" .= ("http://kept.example" :: Text)
            , "model" .= ("m" :: Text)
            , "promptTemplates" .= [object ["name" .= templateName, "version" .= (1 :: Int), "body" .= ("one" :: Text), "active" .= True]] ]])]])
        query @LlmConfig |> filterWhere (#providerName, "m7-doomed-llm-" <> suffix) |> fetch `shouldReturn` []
        templates <- query @LlmPromptTemplate |> filterWhere (#name, templateName) |> fetch
        map (get #id) templates `shouldBe` [get #id v1]

    it "strict llm template delete aborts when an analysis references the version" do
        suffix <- tshow <$> nextRandom
        let templateName = "m7_fk_" <> Text.replace "-" "_" suffix
        v1 <- newRecord @LlmPromptTemplate |> set #name templateName |> set #version 1 |> set #body "one" |> createRecord
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        void $ newRecord @LlmAnalysis
            |> set #alertId alertId
            |> set #promptTemplateId (Just (get #id v1))
            |> set #promptHash fp
            |> createRecord
        keepProviders <- m7LlmKeepItems
        m7Apply (object ["llm" .= object ["strict" .= True, "items" .= (keepProviders <> [object
            [ "providerName" .= ("m7-strict-llm-" <> suffix), "endpoint" .= ("http://kept.example" :: Text)
            , "model" .= ("m" :: Text)
            , "promptTemplates" .= [object ["name" .= templateName, "version" .= (2 :: Int), "body" .= ("two" :: Text)]] ]])]])
            `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf "cannot delete prompt template" msg

    it "strict users deletes unreferenced users and aborts on alert-history references" do
        suffix <- tshow <$> nextRandom
        doomedPlain <- m7User ("m7-doomed-" <> suffix <> "@dev")
        doomedReferenced <- m7User ("m7-fk-" <> suffix <> "@dev")
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        void $ newRecord @AlertEvent
            |> set #alertId alertId
            |> set #userId (Just (get #id doomedReferenced))
            |> set #kind "external"
            |> createRecord
        -- Transactional: the FK-blocked delete rolls the whole category back,
        -- so even the unreferenced doomed user survives this apply.
        keepWithoutReferenced <- m7UserKeepItems [get #email doomedReferenced]
        m7Apply (object ["users" .= object ["strict" .= True, "items" .= keepWithoutReferenced]])
            `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf ("cannot delete user \"m7-fk-" <> suffix <> "@dev\"") msg
        surviving <- query @User |> filterWhere (#email, get #email doomedPlain) |> fetch
        map (get #id) surviving `shouldBe` [get #id doomedPlain]
        -- Without the referenced user in scope the plain one is deleted.
        keepWithoutPlain <- m7UserKeepItems [get #email doomedPlain]
        m7Apply (object ["users" .= object ["strict" .= True, "items" .= keepWithoutPlain]])
        query @User |> filterWhere (#email, get #email doomedPlain) |> fetch `shouldReturn` []
        kept <- query @User |> filterWhere (#email, get #email doomedReferenced) |> fetch
        map (get #id) kept `shouldBe` [get #id doomedReferenced]

    it "strict sources delete aborts when alerts reference the source" do
        suffix <- tshow <$> nextRandom
        doomed <- integrationSource "webhook" ("m7-doomed-src-" <> suffix) "" (object [])
        fp <- freshFingerprint
        Just _ <- ingest doomed (testEvent fp Firing)
        -- The keep-list replays existing sources whose config tokenEnv refs
        -- (fixture zabbix/grafana) must resolve at apply time (§5).
        oldZabbix <- lookupEnv "ZABBIX_TOKEN"
        oldGrafana <- lookupEnv "GRAFANA_TOKEN"
        flip finally (restoreEnv "ZABBIX_TOKEN" oldZabbix >> restoreEnv "GRAFANA_TOKEN" oldGrafana) do
            setEnv "ZABBIX_TOKEN" "m7-dummy-zabbix"
            setEnv "GRAFANA_TOKEN" "m7-dummy-grafana"
            keepItems <- m7SourceKeepItems [get #name doomed]
            m7Apply (object ["sources" .= object ["strict" .= True, "items" .= keepItems]])
                `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf ("cannot delete source \"m7-doomed-src-" <> suffix <> "\"") msg

    it "provisions field mappings and dashboards idempotently" do
        suffix <- tshow <$> nextRandom
        let email = "m7-" <> suffix <> "@dev"
            facet = "m7facet-" <> suffix
            dashName = "m7-dash-" <> suffix
            config enabled = object
                [ "users" .= object ["items" .= [object ["email" .= email, "passwordHash" .= ("x" :: Text)]]]
                , "fieldMappings" .= object ["items" .= [object
                    [ "facet" .= facet, "rank" .= (42 :: Int), "kind" .= ("field" :: Text)
                    , "key" .= ("env" :: Text), "enabled" .= enabled ]]]
                , "dashboards" .= object ["items" .= [object
                    [ "name" .= dashName, "userEmail" .= email, "isDefault" .= True
                    , "config" .= [object
                        [ "title" .= ("probe" :: Text)
                        , "match" .= [object ["facet" .= ("field:env" :: Text), "op" .= ("=" :: Text), "value" .= ("prod" :: Text)]]
                        , "groupBy" .= ("field:host" :: Text) ]] ]]]
                ]
        m7Apply (config False)
        m7Apply (config True)
        mappings <- query @FieldMapping |> filterWhere (#facet, facet) |> fetch
        map (\mapping -> (mapping.rank, mapping.enabled)) mappings `shouldBe` [(42, True)]
        dashboards <- query @Dashboard |> filterWhere (#name, dashName) |> fetch
        map (.isDefault) dashboards `shouldBe` [True]

    it "dashboard provisioning rejects an unresolvable userEmail" do
        suffix <- tshow <$> nextRandom
        m7Apply (object ["dashboards" .= object ["items" .= [object
            [ "name" .= ("m7-dash-" <> suffix), "userEmail" .= ("m7-ghost-" <> suffix <> "@dev") ]]]])
            `shouldThrow` \(ProvisionError msg) -> Text.isInfixOf "does not resolve to any user" msg

    it "strict fieldMappings/dashboards delete only unlisted rows" do
        suffix <- tshow <$> nextRandom
        let email = "m7-" <> suffix <> "@dev"
            doomedFacet = "m7doomed-" <> suffix
            doomedDash = "m7-doomed-dash-" <> suffix
        owner <- m7User email
        void $ sqlExecTyped [typedSql|
            INSERT INTO field_mappings (facet, rank, kind, key, enabled)
            VALUES (${doomedFacet}, 7, 'field', 'env', true)
        |]
        _ <- newRecord @Dashboard
            |> set #userId (get #id owner)
            |> set #name doomedDash
            |> createRecord
        mappings <- query @FieldMapping |> fetch
        let keepMappings = [object
                [ "facet" .= mapping.facet, "rank" .= mapping.rank, "kind" .= mapping.kind
                , "key" .= mapping.key, "enabled" .= mapping.enabled ]
                | mapping <- mappings, mapping.facet /= doomedFacet ]
        dashboards <- query @Dashboard |> fetch
        keepDashboards <- fmap catMaybes $ forM dashboards \dashboard -> do
            dashOwner <- fetch dashboard.userId
            pure $ if dashboard.name == doomedDash then Nothing else Just (object
                [ "name" .= dashboard.name, "userEmail" .= dashOwner.email
                , "config" .= dashboard.config, "position" .= dashboard.position
                , "isDefault" .= dashboard.isDefault ])
        m7Apply (object
            [ "fieldMappings" .= object ["strict" .= True, "items" .= keepMappings]
            , "dashboards" .= object ["strict" .= True, "items" .= keepDashboards]
            ])
        query @FieldMapping |> filterWhere (#facet, doomedFacet) |> fetch `shouldReturn` []
        query @Dashboard |> filterWhere (#name, doomedDash) |> fetch `shouldReturn` []
        remainingMappings <- query @FieldMapping |> fetch
        length remainingMappings `shouldBe` length keepMappings
        remainingDashboards <- query @Dashboard |> fetch
        length remainingDashboards `shouldBe` length keepDashboards

m7Apply :: (?modelContext :: ModelContext) => Aeson.Value -> IO ()
m7Apply config = do
    suffix <- tshow <$> nextRandom
    let path = "/tmp/halemans-m7-" <> cs suffix <> ".json"
    LBS.writeFile path (Aeson.encode config)
    applyProvisionConfig path

m7User :: (?modelContext :: ModelContext) => Text -> IO User
m7User email = newRecord @User
    |> set #email email
    |> set #passwordHash "unused"
    |> createRecord

-- Rows currently in the DB rendered back as config items (minus the excluded
-- natural keys), so strict applies keep them untouched.
m7UserKeepItems :: (?modelContext :: ModelContext) => [Text] -> IO [Aeson.Value]
m7UserKeepItems exclude = do
    users <- query @User |> fetch
    pure [object
        [ "email" .= get #email user
        , "passwordHash" .= get #passwordHash user
        , "displayName" .= get #displayName user
        ] | user <- users, get #email user `notElem` exclude]

m7SourceKeepItems :: (?modelContext :: ModelContext) => [Text] -> IO [Aeson.Value]
m7SourceKeepItems exclude = do
    sources <- query @Source |> fetch
    pure [object
        [ "type" .= get #type_ source
        , "name" .= get #name source
        , "baseUrl" .= get #baseUrl source
        , "env" .= get #env source
        , "pollIntervalSeconds" .= get #pollIntervalSeconds source
        , "enabled" .= get #enabled source
        , "config" .= get #config source
        ] | source <- sources, get #name source `notElem` exclude]

m7TeamKeepItems :: (?modelContext :: ModelContext) => [Text] -> IO [Aeson.Value]
m7TeamKeepItems exclude = do
    teams <- query @Team |> fetch
    forM (filter (\team -> get #name team `notElem` exclude) teams) \team -> do
        let teamId = get #id team
        members <- sqlQueryTyped [typedSql|
            SELECT u.email, tm.team_role FROM team_members tm
            JOIN users u ON u.id = tm.user_id WHERE tm.team_id = ${teamId}
        |]
        pure $ object
            [ "name" .= get #name team
            , "description" .= get #description team
            , "hostGroups" .= get #hostGroups team
            , "defaults" .= get #defaults team
            , "members" .= map (\row -> object ["email" .= get #email row, "role" .= get #team_role row]) members
            ]

m7LlmKeepItems :: (?modelContext :: ModelContext) => IO [Aeson.Value]
m7LlmKeepItems = do
    rows <- query @LlmConfig |> fetch
    pure [object
        [ "providerName" .= get #providerName row
        , "endpoint" .= get #endpoint row
        , "model" .= get #model row
        , "apiKeyEnv" .= get #apiKeyEnv row
        , "toolsEnabled" .= get #toolsEnabled row
        , "enabled" .= get #enabled row
        ] | row <- rows]

restoreEnv :: String -> Maybe String -> IO ()
restoreEnv name = maybe (unsetEnv name) (setEnv name)

-- Milestone 8: assets enrichment + agent roles against the mock Assets
-- server on 18085 (launched by the check; seeded Capacity CMDB dataset with
-- dev-host-01, /debug/reset + /debug/fail/500 backdoors).
m8Spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
m8Spec = describe "enrichment phase 0 (milestone 8)" do
    it "enrich caches and links assets for a mock-known host" do
        config <- ensureAssetsConfig
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
            { host = Just "dev-host-01", checkName = Just "halemans test trigger" }
        job <- enrichJobFor alertId
        perform job
        objects <- query @AssetsObject
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
        links <- query @AssetAlertLink
            |> filterWhere (#alertId, alertId)
            |> fetch
        case links of
            [link] -> link.matchedBy `shouldBe` "dev-host-01"
            _ -> expectationFailure "expected exactly one asset link"
        -- second run: upserts are idempotent
        perform job
        objectsAfter <- query @AssetsObject
            |> filterWhere (#configId, get #id config)
            |> fetch
        length objectsAfter `shouldBe` 1
        linksAfter <- query @AssetAlertLink
            |> filterWhere (#alertId, alertId)
            |> fetch
        length linksAfter `shouldBe` 1

    it "unknown hosts are negative-cached (no re-query within the TTL)" do
        _ <- ensureAssetsConfig
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
            { host = Just "itest-m8-unknown-host" }
        job <- enrichJobFor alertId
        perform job
        linked <- query @AssetAlertLink
            |> filterWhere (#alertId, alertId)
            |> filterWhereSql (#assetsObjectId, "IS NOT NULL")
            |> fetch
        length linked `shouldBe` 0
        misses <- query @AssetAlertLink
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
        failures <- query @AlertEvent
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
            Just alertId <- ingest source (testEvent fp Firing)
                { host = Just "itest-m8-softfail-host" }
            job <- enrichJobFor alertId
            perform job
            failures <- query @AlertEvent
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
        template <- newRecord @LlmPromptTemplate
            |> set #name templateName
            |> set #version 1
            |> set #body "ROLE MARKER {{alert.title}}\nAssets:\n{{assets_excerpt}}"
            |> set #active True
            |> createRecord
        role <- newRecord @LlmAgentRole
            |> set #name roleName
            |> set #promptTemplateName templateName
            |> set #tools (Aeson.toJSON ["assets_lookup" :: Text])
            |> set #enabled True
            |> set #isDefault False
            |> createRecord
        source <- testSource
        fp <- freshFingerprint
        Just alertId <- ingest source (testEvent fp Firing)
        analysis <- newRecord @LlmAnalysis
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
        result <- executeToolCall Nothing ToolCall
            { callId = "call-1"
            , callName = "assets_lookup"
            , callArguments = "{\"term\": \"dev-host-01\"}"
            }
        result `shouldSatisfy` ("dev-host-01" `Text.isInfixOf`)
        result `shouldSatisfy` ("CHCMDB-10001" `Text.isInfixOf`)
        failing <- executeToolCall Nothing ToolCall
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
        role <- newRecord @LlmAgentRole
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

itestAttrNames :: Text
itestAttrNames = "Owner,Cluster,Database,IP,Datacenter,Service,DB Cluster,Environments,Team,Location"

ensureAssetsConfig :: (?modelContext :: ModelContext) => IO AssetsConfig
ensureAssetsConfig = do
    existing <- query @AssetsConfig
        |> filterWhere (#name, "itest-assets" :: Text)
        |> fetchOneOrNothing
    case existing of
        -- Milestone 9 widened the verbatim-facet whitelist; refresh stale rows.
        Just config
            | config.attributeNames == itestAttrNames -> pure config
            | otherwise -> config
                |> set #attributeNames itestAttrNames
                |> updateRecord
        Nothing -> newRecord @AssetsConfig
            |> set #name "itest-assets"
            |> set #baseUrl "http://127.0.0.1:18085/rest/assets/latest"
            |> set #tokenEnv "ASSETS_TOKEN"
            |> set #authMode "bearer"
            |> set #defaultSchemaName "Capacity CMDB"
            |> set #hostQueryTemplate "objectSchema = \"Capacity CMDB\" AND Name like \"{host}\""
            |> set #attributeNames itestAttrNames
            |> set #enabled True
            |> createRecord

enrichJobFor :: (?modelContext :: ModelContext) => Id Alert -> IO EnrichAlertJob
enrichJobFor alertId = query @EnrichAlertJob
    |> filterWhere (#alertId, alertId)
    |> fetchOneOrNothing
    >>= maybe (error "enrich job missing") pure

assetAttr :: Text -> AssetsObject -> Maybe Text
assetAttr name object = lookup name (objectAttributes object)

assetsMockReset :: IO ()
assetsMockReset = void (Wreq.post "http://127.0.0.1:18085/debug/reset" (object ["reset" .= True]))

assetsMockFail :: Int -> IO ()
assetsMockFail times = void (Wreq.post "http://127.0.0.1:18085/debug/fail/500" (object ["times" .= times]))

-- Milestone 9: resolved facets, facet dashboards, grouping over facets
-- (design_docs/milestone_9.md §9). Field mappings are global state, so each
-- test cleans up its own rows.
m9Spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
m9Spec = describe "resolved facets (milestone 9)" do
    it "materializes field/label facets at ingest" do
        (envMapping, envCreated) <- ensureMapping "env" 100 "field" "env"
        teamMapping <- createRecord (newRecord @FieldMapping |> set #facet "team" |> set #rank 100 |> set #kind "label" |> set #key "team" |> set #enabled True)
        flip finally (cleanupMappings [(envMapping, envCreated), (teamMapping, True)]) do
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing)
                { labels = object ["team" .= ("itest-facet-team" :: Text)] }
            alert <- fetch alertId
            facetValue alert "env" `shouldBe` Just "itest-env"
            facetValue alert "team" `shouldBe` Just "itest-facet-team"

    it "enrichment materializes attr facets and the env override beats the source env" do
        _ <- ensureAssetsConfig
        overrideMapping <- createRecord (newRecord @FieldMapping |> set #facet "env" |> set #rank 50 |> set #kind "attr" |> set #key "Environments" |> set #enabled True)
        (fallbackMapping, fallbackCreated) <- ensureMapping "env" 100 "field" "env"
        flip finally (cleanupMappings [(overrideMapping, True), (fallbackMapping, fallbackCreated)]) do
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing)
                { host = Just "dev-host-01", checkName = Just "halemans test trigger", env = Just "zabbix-prod" }
            alertIngest <- fetch alertId
            -- ingest-time: attr source absent, field fallback wins
            facetValue alertIngest "env" `shouldBe` Just "zabbix-prod"
            job <- enrichJobFor alertId
            perform job
            alert <- fetch alertId
            facetValue alert "env" `shouldBe` Just "PROD"
            facetValue alert "Service" `shouldBe` Just "PostgreSQL"
            facetValue alert "DB Cluster" `shouldBe` Just "ibstaffcopdb01"
            facetValue alert "Location" `shouldBe` Just "LV"

    it "grouped card query returns one section per DB Cluster value" do
        _ <- ensureAssetsConfig
        source <- testSource
        alertIds <- forM [("dev-host-01", "ibstaffcopdb01"), ("dev-db-01", "ibstaffcopdb02")] \(host, _) -> do
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing)
                { host = Just host, checkName = Just "halemans test trigger", title = "m9 grouped " <> host }
            job <- enrichJobFor alertId
            perform job
            pure alertId
        let card = DashboardCard
                { cardTitle = Just "pg clusters"
                , cardMatch = [MatchClause (FacetAttr "Service") OpEq "PostgreSQL" []]
                , cardGroupBy = Just (FacetAttr "DB Cluster")
                , cardLimit = 50
                , cardLegacy = False
                , cardForEach = Nothing
                , cardHideWhen = Nothing
                , cardSummary = False
                , cardExtras = mempty
                }
        groups <- runCardQueryGroups card (FacetAttr "DB Cluster")
        -- dev-DB tolerant: other runs' enriched alerts may share the sections
        let alertsIn value = concatMap cgAlerts [group | group <- groups, group.cgValue == value]
        map (get #id) (alertsIn "ibstaffcopdb01") `shouldContain` [alertIds !! 0]
        map (get #id) (alertsIn "ibstaffcopdb02") `shouldContain` [alertIds !! 1]

    it "regroup after enrichment groups an alert a facet rule missed at ingest" do
        _ <- ensureAssetsConfig
        -- unique env/check: no other (dev-DB) rule may match this alert
        tag <- tshow <$> nextRandom
        let envName = "m9-regroup-" <> tag
            checkName' = "m9-regroup-check-" <> tag
        -- dev DBs carry seeded catch-all rules that would win first-match;
        -- sideline all other rules for the duration of this test.
        otherRules <- query @GroupingRule |> filterWhere (#enabled, True) |> fetch
        forM_ otherRules \other -> void (other |> set #enabled False |> updateRecord)
        rule <- newRecord @GroupingRule
            |> set #name ("itest-facet-group-" <> tag)
            |> set #position 9000
            |> set #enabled True
            |> set #match (object ["facets" .= object ["DB Cluster" .= ("ib*" :: Text)]])
            |> set #groupKeyTemplate "db-{facet:DB Cluster}"
            |> set #createdBy Nothing
            |> createRecord
        let restore = do
                deleteRecord rule
                forM_ otherRules \other -> void (other |> set #enabled True |> updateRecord)
        flip finally restore do
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source (testEventIn envName fp Firing)
                { host = Just "dev-host-01", checkName = Just checkName' }
            alertIngest <- fetch alertId
            -- facet absent at ingest: the rule does not match yet
            alertIngest.groupId `shouldBe` Nothing
            job <- enrichJobFor alertId
            perform job
            alert <- fetch alertId
            case alert.groupId of
                Nothing -> expectationFailure "alert not regrouped after enrichment"
                Just groupId -> do
                    group <- fetch groupId
                    group.groupKey `shouldBe` "db-ibstaffcopdb01"
                    alert.groupedByVersion `shouldBe` Just rule.version

    it "facet backfill job recomputes facets for non-closed alerts" do
        (mapping, created) <- ensureMapping "env" 100 "field" "env"
        flip finally (cleanupMappings [(mapping, created)]) do
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source (testEvent fp Firing)
            void (sqlExecTyped [typedSql| UPDATE alerts SET facets = '{}'::jsonb WHERE id = ${alertId} |])
            before <- fetch alertId
            facetValue before "env" `shouldBe` Nothing
            backfillJob <- createRecord (newRecord @FacetBackfillJob)
            perform backfillJob
            after <- fetch alertId
            facetValue after "env" `shouldBe` Just "itest-env"

    it "card templates: forEach expands per facet value; hideWhen hides zero-count cards" do
        suffix <- tshow <$> nextRandom
        let host = "m9tpl-host-" <> suffix
            envA = "m9tpl-a-" <> suffix
            envB = "m9tpl-b-" <> suffix
        source <- integrationSource "webhook" ("m9tpl-" <> suffix) "" (object [])
        let eventIn env fp severity = (testEventIn env fp Firing :: NormalizedEvent) { host = Just host, severity = severity }
        Just _ <- ingest source (eventIn envA ("itest:" <> suffix <> "-a") "warning")
        Just _ <- ingest source (eventIn envB ("itest:" <> suffix <> "-b") "critical")
        cards <- case decodeDashboardConfig (Aeson.toJSON [object
                [ "title" .= ("probe {value}" :: Text)
                , "match" .= [object ["facet" .= ("field:host" :: Text), "op" .= ("=" :: Text), "value" .= host]]
                , "forEach" .= ("field:env" :: Text)
                , "hideWhen" .= object
                    [ "match" .= [object ["facet" .= ("field:severity" :: Text), "op" .= ("=" :: Text), "value" .= ("critical" :: Text)]] ]
                ]]) of
            Left err -> expectationFailure (cs err) >> error "unreachable"
            Right decoded -> pure decoded
        expanded <- expandDashboardCards cards
        map ecDomId expanded `shouldBe` ["dashboard-card-0-" <> envA, "dashboard-card-0-" <> envB]
        map ecIndex expanded `shouldBe` [0, 0]
        map ecValue expanded `shouldBe` [Just envA, Just envB]
        map (.cardTitle) (map ecCard expanded) `shouldBe` [Just ("probe " <> envA), Just ("probe " <> envB)]
        -- envA has no critical alert: hidden; envB has one: visible
        map ecHidden expanded `shouldBe` [True, False]
        -- same hideWhen on a plain (non-template) card
        let plainCard sev = object
                [ "match" .= [object ["facet" .= ("field:host" :: Text), "op" .= ("=" :: Text), "value" .= host]]
                , "hideWhen" .= object
                    [ "match" .= [object ["facet" .= ("field:severity" :: Text), "op" .= ("=" :: Text), "value" .= (sev :: Text)]] ]
                ]
        void $ forM [("critical", False), ("info", True)] \(sev, expectedHidden) -> do
            single <- case decodeDashboardConfig (Aeson.toJSON [plainCard sev]) of
                Left err -> expectationFailure (cs err) >> error "unreachable"
                Right decoded -> pure decoded
            [expandedCard] <- expandDashboardCards single
            expandedCard.ecDomId `shouldBe` "dashboard-card-0"
            expandedCard.ecValue `shouldBe` Nothing
            expandedCard.ecHidden `shouldBe` expectedHidden

    it "summary cards aggregate status counts like the overview env cards" do
        suffix <- tshow <$> nextRandom
        let host = "m9sum-host-" <> suffix
            envA = "m9sum-a-" <> suffix
            envB = "m9sum-b-" <> suffix
        source <- integrationSource "webhook" ("m9sum-" <> suffix) "" (object [])
        let eventIn env fp severity status = (testEventIn env fp status :: NormalizedEvent) { host = Just host, severity = severity }
        Just _ <- ingest source (eventIn envA ("itest:" <> suffix <> "-a1") "warning" Firing)
        Just _ <- ingest source (eventIn envA ("itest:" <> suffix <> "-a2") "info" Firing)
        void $ ingest source (eventIn envA ("itest:" <> suffix <> "-a2") "info" Resolved)
        Just _ <- ingest source (eventIn envB ("itest:" <> suffix <> "-b1") "critical" Firing)
        cards <- case decodeDashboardConfig (Aeson.toJSON [object
                [ "match" .= [object ["facet" .= ("field:host" :: Text), "op" .= ("=" :: Text), "value" .= host]]
                , "forEach" .= ("field:env" :: Text)
                , "summary" .= True
                ]]) of
            Left err -> expectationFailure (cs err) >> error "unreachable"
            Right decoded -> pure decoded
        expanded <- expandDashboardCards cards
        map ecDomId expanded `shouldBe` ["dashboard-card-0-" <> envA, "dashboard-card-0-" <> envB]
        forM_ (map ecCard expanded) \expandedCard -> expandedCard.cardSummary `shouldBe` True
        [summaryA, summaryB] <- mapM (runCardSummary . ecCard) expanded
        (summaryA.csFiring, summaryA.csResolved, summaryA.csWorstSeverity) `shouldBe` (1, 1, Just "warning")
        (summaryB.csFiring, summaryB.csResolved, summaryB.csWorstSeverity) `shouldBe` (1, 0, Just "critical")
        summaryA.csHourly `shouldSatisfy` (not . null)

-- | Reuse an existing mapping row (dev DBs carry the seeded passthrough
-- mappings); the Bool marks rows this run created and must delete.
ensureMapping :: (?modelContext :: ModelContext) => Text -> Int -> Text -> Text -> IO (FieldMapping, Bool)
ensureMapping facet rank kind key = do
    existing <- query @FieldMapping
        |> filterWhere (#facet, facet)
        |> filterWhere (#rank, rank)
        |> fetchOneOrNothing
    case existing of
        Just row -> pure (row, False)
        Nothing -> do
            row <- newRecord @FieldMapping
                |> set #facet facet
                |> set #rank rank
                |> set #kind kind
                |> set #key key
                |> set #enabled True
                |> createRecord
            pure (row, True)

cleanupMappings :: (?modelContext :: ModelContext) => [(FieldMapping, Bool)] -> IO ()
cleanupMappings = mapM_ \(row, created) -> when created (deleteRecord row)


