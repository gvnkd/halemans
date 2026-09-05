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
import Application.Service.Notify (currentOnCall, resolveRuleTargets)
import Application.Service.WriteBack (executeAttempt)
import Application.Service.Reconcile (mirrorExternalAck, mirrorExternalUnack, lastAckWasExternal)
import Application.Service.Jira (syncOpenLinks)
import qualified Application.Connector.Grafana as Grafana
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson

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
        hspec (spec >> llmSpec)

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
