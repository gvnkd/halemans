module Main where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Generated.Types
import System.Environment (lookupEnv, getEnv)
import System.Process (callProcess, readProcess)
import Data.Aeson (object)
import Data.UUID.V4 (nextRandom)
import Control.Monad (void)

import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest)
import Application.Pipeline.Actions (ackAlert, unackAlert, closeAlert)
import Application.Job.AutoClose (unackExpiredAcks, unsuppressExpired)
import Application.Job.Escalation (runDueTrackers)
import Application.Service.Notify (currentOnCall, resolveRuleTargets)
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
    withModelContext (cs databaseUrl) noopLogger \modelContext -> do
        let ?modelContext = modelContext
        hspec spec

spec :: (?modelContext :: ModelContext) => Spec
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

schemaPresent :: String -> IO Bool
schemaPresent databaseUrl = do
    output <- readProcess "psql" [databaseUrl, "-tA", "-c", "SELECT to_regclass('public.alerts') IS NOT NULL"] ""
    pure (output == "t\n")

testSource :: (?modelContext :: ModelContext) => IO Source
testSource = query @Source
    |> filterWhere (#type_, "alertmanager" :: Text)
    |> fetchOneOrNothing
    >>= maybe (error "alertmanager source fixture missing") pure

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
