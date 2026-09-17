module Test.Integration.Setup (schemaPresent, testSource, integrationSource, payloadText, retryWriteBack, fetchEnvironment, testUser, freshFingerprint, testEvent, testEventIn, eventKinds, notifiedEvents, groupingRule, notificationRule, m6User, mockFail, m7Apply, m7User, m7UserKeepItems, m7SourceKeepItems, m7TeamKeepItems, m7LlmKeepItems, restoreEnv, itestAttrNames, ensureAssetsConfig, ensureMockJiraConfig, ensureMockCmdbConfig, withOnlyCmdbConfig, enrichJobFor, freshEnrichJob, assetAttr, assetsMockReset, assetsMockFail, ensureMapping, cleanupMappings, ensureTemplate, latestAnalysis, enqueueAnalysis, performLatestJob, counterRequestsAfter, pendingZabbixJobs, resetToolCache, resetToolCacheConfig, integrationMain) where

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

ensureTemplate :: (?modelContext :: ModelContext) => IO LlmPromptTemplate
ensureTemplate = do
    existing <-
        query @LlmPromptTemplate
            |> filterWhere (#name, "alert_enrichment" :: Text)
            |> filterWhere (#version, 1)
            |> fetchOneOrNothing
    case existing of
        Just template -> pure template
        Nothing ->
            newRecord @LlmPromptTemplate
                |> set #name "alert_enrichment"
                |> set #version 1
                |> set #body "Alert: {{alert.title}}\n{{alert.description}}\nEvents:\n{{events}}\nCMDB:\n{{cmdb_excerpt}}\nSimilar:\n{{similar_alerts}}\nJira:\n{{jira_links}}"
                |> set #active True
                |> createRecord

latestAnalysis :: (?modelContext :: ModelContext) => Id Alert -> IO LlmAnalysis
latestAnalysis alertId =
    query @LlmAnalysis
        |> filterWhere (#alertId, alertId)
        |> orderByDesc #createdAt
        |> limit 1
        |> fetchOneOrNothing
        >>= maybe (error "llm analysis missing") pure

enqueueAnalysis :: (?modelContext :: ModelContext) => Id Alert -> IO LlmAnalysis
enqueueAnalysis alertId = do
    analysis <-
        newRecord @LlmAnalysis
            |> set #alertId alertId
            |> createRecord
    void do
        newRecord @LlmAnalysisJob
            |> set #analysisId (get #id analysis)
            |> createRecord
    pure analysis

performLatestJob :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Id LlmAnalysis -> IO ()
performLatestJob analysisId = do
    job <-
        query @LlmAnalysisJob
            |> filterWhere (#analysisId, analysisId)
            |> orderByDesc #createdAt
            |> limit 1
            |> fetchOneOrNothing
            >>= maybe (error "llm job missing") pure
    perform job

counterRequestsAfter :: (?modelContext :: ModelContext) => Text -> IO Int
counterRequestsAfter provider = do
    rows <-
        sqlQueryTyped
            [typedSql|
        SELECT requests FROM llm_budget_counters
        WHERE provider = ${provider} AND day = CURRENT_DATE
    |]
    pure (fromMaybe 0 (head rows))

pendingZabbixJobs :: (?modelContext :: ModelContext) => IO Int64
pendingZabbixJobs = do
    rows <-
        sqlQueryTyped
            [typedSql|
        SELECT count(*) FROM poll_zabbix_jobs WHERE status = 'job_status_not_started'
    |]
    pure (fromMaybe 0 (head rows))

m6User :: (?modelContext :: ModelContext) => [Text] -> IO User
m6User privileges = do
    suffix <- tshow <$> nextRandom
    role <-
        newRecord @Role
            |> set #name ("m6-" <> suffix)
            |> set #privileges privileges
            |> createRecord
    user <-
        newRecord @User
            |> set #email ("m6-" <> suffix <> "@dev")
            |> set #passwordHash "unused"
            |> createRecord
    void $
        newRecord @UserRole
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
testSource =
    query @Source
        |> filterWhere (#type_, "alertmanager" :: Text)
        |> fetchOneOrNothing
        >>= maybe (error "alertmanager source fixture missing") pure

integrationSource :: (?modelContext :: ModelContext) => Text -> Text -> Text -> Aeson.Value -> IO Source
integrationSource sourceType name baseUrl config =
    newRecord @Source
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
fetchEnvironment name =
    query @Environment
        |> filterWhere (#name, name)
        |> fetchOneOrNothing
        >>= maybe (error "environment missing") pure

testUser :: (?modelContext :: ModelContext) => IO User
testUser = do
    existing <-
        query @User
            |> filterWhere (#email, "itest@dev" :: Text)
            |> fetchOneOrNothing
    case existing of
        Just user -> pure user
        Nothing ->
            newRecord @User
                |> set #email "itest@dev"
                |> set #passwordHash "unused"
                |> createRecord

freshFingerprint :: IO Text
freshFingerprint = ("itest:" <>) . tshow <$> nextRandom

testEvent :: Text -> SourceStatus -> NormalizedEvent
testEvent = testEventIn "itest-env"

testEventIn :: Text -> Text -> SourceStatus -> NormalizedEvent
testEventIn envName fp status =
    NormalizedEvent
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
eventKinds alertId =
    map (get #kind)
        <$> ( query @AlertEvent
                |> filterWhere (#alertId, alertId)
                |> orderByAsc #createdAt
                |> fetch
            )

notifiedEvents :: (?modelContext :: ModelContext) => Id Alert -> IO [AlertEvent]
notifiedEvents alertId =
    query @AlertEvent
        |> filterWhere (#alertId, alertId)
        |> filterWhere (#kind, "notified" :: Text)
        |> fetch

groupingRule :: (?modelContext :: ModelContext) => Text -> Text -> IO GroupingRule
groupingRule name template =
    newRecord @GroupingRule
        |> set #name name
        |> set #position 10
        |> set #enabled True
        |> set #match (object [])
        |> set #groupKeyTemplate template
        |> createRecord

notificationRule :: (?modelContext :: ModelContext) => Text -> Maybe (Id User) -> Maybe (Id EscalationPolicy) -> IO NotificationRule
notificationRule name userRef policyRef =
    newRecord @NotificationRule
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

m7Apply :: (?modelContext :: ModelContext) => Aeson.Value -> IO ()
m7Apply config = do
    suffix <- tshow <$> nextRandom
    let path = "/tmp/halemans-m7-" <> cs suffix <> ".json"
    LBS.writeFile path (Aeson.encode config)
    applyProvisionConfig path

m7User :: (?modelContext :: ModelContext) => Text -> IO User
m7User email =
    newRecord @User
        |> set #email email
        |> set #passwordHash "unused"
        |> createRecord

-- Rows currently in the DB rendered back as a config section map (keyed by
-- the natural key, minus the excluded ones), so strict applies keep them
-- untouched.
m7UserKeepItems :: (?modelContext :: ModelContext) => [Text] -> IO Aeson.Value
m7UserKeepItems exclude = do
    users <- query @User |> fetch
    pure $
        object
            [ Key.fromText (get #email user)
                .= object
                    [ "passwordHash" .= get #passwordHash user
                    , "displayName" .= get #displayName user
                    ]
            | user <- users
            , get #email user `notElem` exclude
            ]

m7SourceKeepItems :: (?modelContext :: ModelContext) => [Text] -> IO Aeson.Value
m7SourceKeepItems exclude = do
    sources <- query @Source |> fetch
    pure $
        object
            [ Key.fromText (get #name source)
                .= object
                    [ "type" .= get #type_ source
                    , "baseUrl" .= get #baseUrl source
                    , "env" .= get #env source
                    , "pollIntervalSeconds" .= get #pollIntervalSeconds source
                    , "enabled" .= get #enabled source
                    , "config" .= get #config source
                    ]
            | source <- sources
            , get #name source `notElem` exclude
            ]

m7TeamKeepItems :: (?modelContext :: ModelContext) => [Text] -> IO Aeson.Value
m7TeamKeepItems exclude = do
    teams <- query @Team |> fetch
    entries <- forM (filter (\team -> get #name team `notElem` exclude) teams) \team -> do
        let teamId = get #id team
        members <-
            sqlQueryTyped
                [typedSql|
            SELECT u.email, tm.team_role FROM team_members tm
            JOIN users u ON u.id = tm.user_id WHERE tm.team_id = ${teamId}
        |]
        pure $
            Key.fromText (get #name team)
                .= object
                    [ "description" .= get #description team
                    , "hostGroups" .= get #hostGroups team
                    , "defaults" .= get #defaults team
                    , "members" .= object [Key.fromText (get #email row) .= object ["role" .= get #team_role row] | row <- members]
                    ]
    pure (object entries)

m7LlmKeepItems :: (?modelContext :: ModelContext) => IO Aeson.Value
m7LlmKeepItems = do
    rows <- query @LlmConfig |> fetch
    pure $
        object
            [ Key.fromText (get #providerName row)
                .= object
                    [ "endpoint" .= get #endpoint row
                    , "model" .= get #model row
                    , "apiKeyEnv" .= get #apiKeyEnv row
                    , "toolsEnabled" .= get #toolsEnabled row
                    , "enabled" .= get #enabled row
                    ]
            | row <- rows
            ]

restoreEnv :: String -> Maybe String -> IO ()
restoreEnv name = maybe (unsetEnv name) (setEnv name)

itestAttrNames :: Text
itestAttrNames = "Owner,Cluster,Database,IP,Datacenter,Service,DB Cluster,Environments,Team,Location"

ensureAssetsConfig :: (?modelContext :: ModelContext) => IO AssetsConfig
ensureAssetsConfig = do
    existing <-
        query @AssetsConfig
            |> filterWhere (#name, "itest-assets" :: Text)
            |> fetchOneOrNothing
    case existing of
        -- Milestone 9 widened the verbatim-facet whitelist; refresh stale rows.
        Just config
            | config.attributeNames == itestAttrNames -> pure config
            | otherwise ->
                config
                    |> set #attributeNames itestAttrNames
                    |> updateRecord
        Nothing ->
            newRecord @AssetsConfig
                |> set #name "itest-assets"
                |> set #baseUrl "http://127.0.0.1:18085/rest/assets/latest"
                |> set #tokenEnv "ASSETS_TOKEN"
                |> set #authMode "bearer"
                |> set #defaultSchemaName "Capacity CMDB"
                |> set #hostQueryTemplate "objectSchema = \"Capacity CMDB\" AND Name like \"{host}\""
                |> set #attributeNames itestAttrNames
                |> set #enabled True
                |> createRecord

ensureMockJiraConfig :: (?modelContext :: ModelContext) => IO ()
ensureMockJiraConfig = do
    existing <-
        query @JiraConfig
            |> filterWhere (#name, "itest-jira" :: Text)
            |> fetchOneOrNothing
    case existing of
        Just config ->
            unless
                config.enabled
                (void (config |> set #enabled True |> updateRecord))
        Nothing ->
            void $
                newRecord @JiraConfig
                    |> set #name "itest-jira"
                    |> set #baseUrl "http://127.0.0.1:18083"
                    |> set #tokenEnv "JIRA_TOKEN"
                    |> set #apiVersion "3"
                    |> set #projects (Aeson.toJSON ["DEV" :: Text])
                    |> set #enabled True
                    |> createRecord

ensureMockCmdbConfig :: (?modelContext :: ModelContext) => IO ()
ensureMockCmdbConfig = do
    existing <-
        query @CmdbConfig
            |> filterWhere (#name, "itest-confluence" :: Text)
            |> fetchOneOrNothing
    case existing of
        Just config ->
            unless
                config.enabled
                (void (config |> set #enabled True |> updateRecord))
        Nothing ->
            void $
                newRecord @CmdbConfig
                    |> set #name "itest-confluence"
                    |> set #baseUrl "http://127.0.0.1:18082"
                    |> set #tokenEnv "CONFLUENCE_TOKEN"
                    |> set #spaces (Aeson.toJSON ["DEV" :: Text])
                    |> set #enabled True
                    |> createRecord

-- Runs the action with the ONLY enabled CMDB connection pointing at
-- baseUrl (dead port, wrong token, ...), restoring the previous rows
-- afterwards. Config resolution is global (all enabled rows), so a failure
-- probe must exclude the healthy mock connection.
withOnlyCmdbConfig :: (?modelContext :: ModelContext) => Text -> IO a -> IO a
withOnlyCmdbConfig baseUrl action = do
    suffix <- tshow <$> nextRandom
    previous <- query @CmdbConfig |> fetch
    forM_ (filter (.enabled) previous) \row ->
        void (row |> set #enabled False |> updateRecord)
    probe <-
        newRecord @CmdbConfig
            |> set #name ("itest-cmdb-probe-" <> suffix)
            |> set #baseUrl baseUrl
            |> set #tokenEnv "CONFLUENCE_TOKEN"
            |> set #enabled True
            |> createRecord
    flip finally (restore previous probe) action
  where
    restore previous probe = do
        deleteRecord probe
        forM_ (filter (.enabled) previous) \row -> do
            current <- fetch (get #id row)
            void (current |> set #enabled True |> updateRecord)

enrichJobFor :: (?modelContext :: ModelContext) => Id Alert -> IO EnrichAlertJob
enrichJobFor alertId =
    query @EnrichAlertJob
        |> filterWhere (#alertId, alertId)
        |> fetchOneOrNothing
        >>= maybe (error "enrich job missing") pure

-- Replaces the ingest-created row (which the dev worker races us for) with
-- a manually inserted one whose run_at is in the future, so only the test
-- performs it.
freshEnrichJob :: (?modelContext :: ModelContext) => Id Alert -> IO EnrichAlertJob
freshEnrichJob alertId = do
    _ <- sqlExecTyped [typedSql| DELETE FROM enrich_alert_jobs WHERE alert_id = ${alertId} |]
    now <- getCurrentTime
    newRecord @EnrichAlertJob
        |> set #alertId alertId
        |> set #runAt (addUTCTime 86400 now)
        |> createRecord

assetAttr :: Text -> AssetsObject -> Maybe Text
assetAttr name object = lookup name (objectAttributes object)

assetsMockReset :: IO ()
assetsMockReset = void (Wreq.post "http://127.0.0.1:18085/debug/reset" (object ["reset" .= True]))

assetsMockFail :: Int -> IO ()
assetsMockFail times = void (Wreq.post "http://127.0.0.1:18085/debug/fail/500" (object ["times" .= times]))

-- | Reuse an existing mapping row (dev DBs carry the seeded passthrough
-- mappings); the Bool marks rows this run created and must delete.
ensureMapping :: (?modelContext :: ModelContext) => Text -> Int -> Text -> Text -> IO (FieldMapping, Bool)
ensureMapping facet rank kind key = do
    existing <-
        query @FieldMapping
            |> filterWhere (#facet, facet)
            |> filterWhere (#rank, rank)
            |> fetchOneOrNothing
    case existing of
        Just row -> pure (row, False)
        Nothing -> do
            row <-
                newRecord @FieldMapping
                    |> set #facet facet
                    |> set #rank rank
                    |> set #kind kind
                    |> set #key key
                    |> set #enabled True
                    |> createRecord
            pure (row, True)

cleanupMappings :: (?modelContext :: ModelContext) => [(FieldMapping, Bool)] -> IO ()
cleanupMappings = mapM_ \(row, created) -> when created (deleteRecord row)

resetToolCache :: (?modelContext :: ModelContext) => IO ()
resetToolCache = do
    void (sqlExecTyped [typedSql| DELETE FROM llm_tool_cache WHERE tool LIKE 'itest_tool%' |])
    resetToolCacheConfig

resetToolCacheConfig :: (?modelContext :: ModelContext) => IO ()
resetToolCacheConfig =
    void (sqlExecTyped [typedSql| DELETE FROM llm_tool_cache_configs |])

-- | Boot a temp/dev DATABASE_URL with the schema (loading it when missing)
-- and run the given suite with model/framework contexts bound.
integrationMain :: ((?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec) -> IO ()
integrationMain suite = do
    databaseUrl <-
        lookupEnv "DATABASE_URL" >>= \case
            Just url -> pure (cs url)
            Nothing -> error "DATABASE_URL not set. Run via `nix flake check`."
    ihpLib <- getEnv "IHP_LIB"
    -- Load the schema only when missing (the nix check pre-loads it so
    -- typedSql compile-time introspection works; manual dev runs don't).
    hasSchema <- schemaPresent databaseUrl
    if hasSchema
        then pure ()
        else
            callProcess
                "psql"
                [ databaseUrl
                , "-v"
                , "ON_ERROR_STOP=1"
                , "-q"
                , "-f"
                , ihpLib <> "/IHPSchema.sql"
                , "-f"
                , "Application/Schema.sql"
                , "-f"
                , "Application/Fixtures.sql"
                ]
    frameworkConfig <- buildFrameworkConfig noopLogger (pure ())
    withModelContext (cs databaseUrl) noopLogger \modelContext -> do
        let ?modelContext = modelContext
        let ?context = frameworkConfig
        hspec suite
