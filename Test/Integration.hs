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
import Application.Pipeline.Actions (ackAlert, closeAlert)
import Application.Job.AutoClose (unackExpiredAcks, unsuppressExpired)

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
