module Test.Integration.DashboardsSpec (spec) where

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
import Test.Integration.Setup
import Web.View.Dashboard.Index (EnvCard (..), computeEnvCards)

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
            Just alertId <-
                ingest
                    source
                    (testEvent fp Firing)
                        { labels = object ["team" .= ("itest-facet-team" :: Text)]
                        }
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
            Just alertId <-
                ingest
                    source
                    (testEvent fp Firing)
                        { host = Just "dev-host-01"
                        , checkName = Just "halemans test trigger"
                        , env = Just "zabbix-prod"
                        }
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
            Just alertId <-
                ingest
                    source
                    (testEvent fp Firing)
                        { host = Just host
                        , checkName = Just "halemans test trigger"
                        , title = "m9 grouped " <> host
                        }
            job <- enrichJobFor alertId
            perform job
            pure alertId
        let card =
                DashboardCard
                    { cardTitle = Just "pg clusters"
                    , cardMatch = [MatchClause (FacetAttr "Service") OpEq "PostgreSQL" []]
                    , cardGroupBy = Just (FacetAttr "DB Cluster")
                    , cardLimit = 50
                    , cardLegacy = False
                    , cardForEach = Nothing
                    , cardHideWhen = Nothing
                    , cardSummary = False
                    , cardSortBy = []
                    , cardAlertSortBy = []
                    , cardSize = Nothing
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
        rule <-
            newRecord @GroupingRule
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
            Just alertId <-
                ingest
                    source
                    (testEventIn envName fp Firing)
                        { host = Just "dev-host-01"
                        , checkName = Just checkName'
                        }
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

    it "facet env override drives list filters, card queries and overview cards" do
        _ <- ensureAssetsConfig
        overrideMapping <- createRecord (newRecord @FieldMapping |> set #facet "env" |> set #rank 50 |> set #kind "attr" |> set #key "Environments" |> set #enabled True)
        (fallbackMapping, fallbackCreated) <- ensureMapping "env" 100 "field" "env"
        flip finally (cleanupMappings [(overrideMapping, True), (fallbackMapping, fallbackCreated)]) do
            source <- testSource
            fp <- freshFingerprint
            Just alertId <-
                ingest
                    source
                    (testEvent fp Firing)
                        { host = Just "dev-host-01"
                        , checkName = Just "halemans test trigger"
                        , env = Just "m9-override-raw"
                        }
            job <- enrichJobFor alertId
            perform job
            -- /alerts env filter matches the override, not the raw env
            -- (dev-DB tolerant: other runs may leave PROD-overridden alerts)
            let idsFor envName = map (get #id) <$> listAlerts defaultAlertListFilters{alfEnvs = [envName]} 500
            prodIds <- idsFor "PROD"
            prodIds `shouldContain` [alertId]
            idsFor "m9-override-raw" `shouldReturn` []
            -- env filter dropdown source includes the override-only name
            names <- effectiveEnvNames
            names `shouldContain` ["PROD"]
            -- dashboard card with a legacy field:env clause follows the override
            let card =
                    DashboardCard
                        { cardTitle = Nothing
                        , cardMatch = [MatchClause (FacetField FieldEnv) OpEq "PROD" []]
                        , cardGroupBy = Nothing
                        , cardLimit = 50
                        , cardLegacy = False
                        , cardForEach = Nothing
                        , cardHideWhen = Nothing
                        , cardSummary = False
                        , cardSortBy = []
                        , cardAlertSortBy = []
                        , cardSize = Nothing
                        , cardExtras = mempty
                        }
            cardIds <- map (get #id) <$> runCardQuery card
            cardIds `shouldContain` [alertId]
            -- overview cards are keyed by the effective env
            let cardTotal' card = card.cardFiring + card.cardAcked + card.cardResolved + card.cardStalled
            (cards, _) <- computeEnvCards
            let cardFor name = find (\card -> card.cardEnvName == Just name) cards
            case cardFor "PROD" of
                Nothing -> expectationFailure "no overview card for the overridden env"
                Just card -> cardTotal' card `shouldSatisfy` (> 0)
            case cardFor "m9-override-raw" of
                Nothing -> pure ()
                Just card -> cardTotal' card `shouldBe` 0

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
        let eventIn env fp severity = (testEventIn env fp Firing :: NormalizedEvent){host = Just host, severity = severity}
        Just _ <- ingest source (eventIn envA ("itest:" <> suffix <> "-a") "warning")
        Just _ <- ingest source (eventIn envB ("itest:" <> suffix <> "-b") "critical")
        cards <- case decodeDashboardConfig
            ( Aeson.toJSON
                [ object
                    [ "title" .= ("probe {value}" :: Text)
                    , "match" .= [object ["facet" .= ("field:host" :: Text), "op" .= ("=" :: Text), "value" .= host]]
                    , "forEach" .= ("field:env" :: Text)
                    , "hideWhen"
                        .= object
                            ["match" .= [object ["facet" .= ("field:severity" :: Text), "op" .= ("=" :: Text), "value" .= ("critical" :: Text)]]]
                    ]
                ]
            ) of
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
        let plainCard sev =
                object
                    [ "match" .= [object ["facet" .= ("field:host" :: Text), "op" .= ("=" :: Text), "value" .= host]]
                    , "hideWhen"
                        .= object
                            ["match" .= [object ["facet" .= ("field:severity" :: Text), "op" .= ("=" :: Text), "value" .= (sev :: Text)]]]
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
        let eventIn env fp severity status = (testEventIn env fp status :: NormalizedEvent){host = Just host, severity = severity}
        Just _ <- ingest source (eventIn envA ("itest:" <> suffix <> "-a1") "warning" Firing)
        Just _ <- ingest source (eventIn envA ("itest:" <> suffix <> "-a2") "info" Firing)
        void $ ingest source (eventIn envA ("itest:" <> suffix <> "-a2") "info" Resolved)
        Just _ <- ingest source (eventIn envB ("itest:" <> suffix <> "-b1") "critical" Firing)
        cards <- case decodeDashboardConfig
            ( Aeson.toJSON
                [ object
                    [ "match" .= [object ["facet" .= ("field:host" :: Text), "op" .= ("=" :: Text), "value" .= host]]
                    , "forEach" .= ("field:env" :: Text)
                    , "summary" .= True
                    ]
                ]
            ) of
            Left err -> expectationFailure (cs err) >> error "unreachable"
            Right decoded -> pure decoded
        expanded <- expandDashboardCards cards
        map ecDomId expanded `shouldBe` ["dashboard-card-0-" <> envA, "dashboard-card-0-" <> envB]
        forM_ (map ecCard expanded) \expandedCard -> expandedCard.cardSummary `shouldBe` True
        [summaryA, summaryB] <- mapM (runCardSummary . ecCard) expanded
        (summaryA.csFiring, summaryA.csResolved, summaryA.csWorstSeverity) `shouldBe` (1, 1, Just "warning")
        (summaryB.csFiring, summaryB.csResolved, summaryB.csWorstSeverity) `shouldBe` (1, 0, Just "critical")
        summaryA.csHourly `shouldSatisfy` (not . null)

    it "summary hourly buckets key on last_seen_at, not created_at" do
        suffix <- tshow <$> nextRandom
        let host = "m9hour-host-" <> suffix
            envA = "m9hour-a-" <> suffix
        source <- integrationSource "webhook" ("m9hour-" <> suffix) "" (object [])
        let eventIn env fp = (testEventIn env fp Firing :: NormalizedEvent){host = Just host, severity = "warning"}
        Just alertId <- ingest source (eventIn envA ("itest:" <> suffix <> "-a"))
        void (sqlExecTyped [typedSql| UPDATE alerts SET created_at = NOW() - INTERVAL '3 days' WHERE id = ${alertId} |])
        cards <- case decodeDashboardConfig
            ( Aeson.toJSON
                [ object
                    [ "match" .= [object ["facet" .= ("field:host" :: Text), "op" .= ("=" :: Text), "value" .= host]]
                    , "summary" .= True
                    ]
                ]
            ) of
            Left err -> expectationFailure (cs err) >> error "unreachable"
            Right decoded -> pure decoded
        [summaryCard] <- expandDashboardCards cards
        summary <- runCardSummary summaryCard.ecCard
        summary.csFiring `shouldBe` 1
        summary.csHourly `shouldSatisfy` (not . null)
        (envCards, _) <- computeEnvCards
        case find (\card -> card.cardEnvName == Just envA) envCards of
            Nothing -> expectationFailure "no overview card for the backdated alert's env"
            Just card -> card.cardHourly `shouldSatisfy` (not . null)

    it "sortBy orders the cards a template expands into" do
        suffix <- tshow <$> nextRandom
        let host = "m9srt-host-" <> suffix
            envA = "m9srt-a-" <> suffix
            envB = "m9srt-b-" <> suffix
            envC = "m9srt-c-" <> suffix
        source <- integrationSource "webhook" ("m9srt-" <> suffix) "" (object [])
        let eventIn env fp severity = (testEventIn env fp Firing :: NormalizedEvent){host = Just host, severity = severity}
        Just _ <- ingest source (eventIn envA ("itest:" <> suffix <> "-a") "critical")
        Just _ <- ingest source (eventIn envB ("itest:" <> suffix <> "-b") "info")
        Just _ <- ingest source (eventIn envC ("itest:" <> suffix <> "-c") "warning")
        let expandWith sortKeys = do
                cards <- case decodeDashboardConfig
                    ( Aeson.toJSON
                        [ object
                            [ "match" .= [object ["facet" .= ("field:host" :: Text), "op" .= ("=" :: Text), "value" .= host]]
                            , "forEach" .= ("field:env" :: Text)
                            , "sortBy" .= sortKeys
                            ]
                        ]
                    ) of
                    Left err -> expectationFailure (cs err) >> error "unreachable"
                    Right decoded -> pure decoded
                map ecValue <$> expandDashboardCards cards
        -- severity: worst first (critical > warning > info), not alphabetical
        expandWith (["key:severity"] :: [Text]) `shouldReturn` [Just envA, Just envC, Just envB]
        -- descending facet ref: reverse alphabetical
        expandWith (["-field:env"] :: [Text]) `shouldReturn` [Just envC, Just envB, Just envA]
        -- count as tiebreak-relevant key: single-alert cards tie, facet breaks ties
        expandWith (["key:count", "field:env"] :: [Text]) `shouldReturn` [Just envA, Just envB, Just envC]

    it "alertSortBy orders the card's alert list" do
        suffix <- tshow <$> nextRandom
        let host = "m9asrt-host-" <> suffix
            env = "m9asrt-" <> suffix
        source <- integrationSource "webhook" ("m9asrt-" <> suffix) "" (object [])
        let eventIn fp severity = (testEventIn env fp Firing :: NormalizedEvent){host = Just host, severity = severity}
        Just _ <- ingest source (eventIn ("itest:" <> suffix <> "-i1") "info")
        Just _ <- ingest source (eventIn ("itest:" <> suffix <> "-w1") "warning")
        Just _ <- ingest source (eventIn ("itest:" <> suffix <> "-i2") "info")
        Just _ <- ingest source (eventIn ("itest:" <> suffix <> "-c1") "critical")
        let queryWith alertSortBy = do
                cards <- case decodeDashboardConfig
                    ( Aeson.toJSON
                        [ object
                            [ "match" .= [object ["facet" .= ("field:host" :: Text), "op" .= ("=" :: Text), "value" .= host]]
                            , "alertSortBy" .= alertSortBy
                            ]
                        ]
                    ) of
                    Left err -> expectationFailure (cs err) >> error "unreachable"
                    Right decoded -> pure decoded
                case cards of
                    (card : _) -> map (\alert -> alert.severity) <$> runCardQuery card
                    [] -> expectationFailure "expected one card" >> error "unreachable"
        -- severity worst-first, not fetch (newest-first) order
        queryWith (["severity"] :: [Text]) `shouldReturn` ["critical", "warning", "info", "info"]
        -- flipped: info first
        queryWith (["-severity"] :: [Text]) `shouldReturn` ["info", "info", "warning", "critical"]

-- | Resolved facets (m9).
spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = m9Spec
