module Application.Service.SourceHealth where

import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest, publishAlertUpdate)
import Control.Monad (void)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseMaybe)
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

-- Source-health alerting (design_docs/milestone_5.md §4): connector failures
-- and webhook silence become ordinary alerts with fingerprint
-- halemans:source-health:<source_id> so dedupe/grouping/notifications/WS all
-- work unmodified. Backoff state lives on the sources row and is honored by
-- the pollers (next_poll_at) and shown on the admin sources page.

healthFingerprint :: Id Source -> Text
healthFingerprint sourceId = "halemans:source-health:" <> tshow sourceId

maxBackoffSeconds :: Int
maxBackoffSeconds = 30 * 60

escalateAfterFailures :: Int
escalateAfterFailures = 5

-- | interval * 2^failures, capped at 30min (failures is 1-based: the count
-- after the current failure).
backoffSeconds :: Int -> Int -> Int
backoffSeconds intervalSeconds failures
    | failures <= 0 = intervalSeconds
    | otherwise = min maxBackoffSeconds (intervalSeconds * 2 ^ failures)

-- | Deterministic ±10% jitter keyed on a seed (source id) — avoids a random
-- dependency and keeps tests reproducible.
jitteredBackoff :: Text -> Int -> Int -> Int
jitteredBackoff seed intervalSeconds failures =
    let base = backoffSeconds intervalSeconds failures
        window = max 1 (base `div` 10)
        offset = fromIntegral (hashText seed `mod` fromIntegral (2 * window + 1)) - window
     in max 1 (base + offset)

hashText :: Text -> Integer
hashText text = foldl' (\acc c -> (acc * 31 + fromIntegral (fromEnum c)) `mod` 1000003) 7 (cs text :: String)

-- | Poll skip predicate honoring the backoff cursor.
pollDue :: UTCTime -> Source -> Bool
pollDue now source = maybe True (<= now) source.nextPollAt

-- | Per-source expected inbound interval for push sources (webhook silence
-- detection); unset disables silence detection for that source. Ignored on
-- poll sources (zabbix/grafana): SourceHealthJob only scans push types
-- because poll connectors never write raw_events rows.
expectedIntervalSeconds :: Source -> Maybe Int
expectedIntervalSeconds source =
    parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: Key.fromText "expectedIntervalSeconds")) source.config

-- | Connector failure: bump the counter, set last_error + next_poll_at, and
-- feed an internal alert through the normal pipeline. The 5th consecutive
-- failure upgrades the open alert to high.
recordFailure :: (?modelContext :: ModelContext) => Source -> Text -> IO ()
recordFailure = recordFailureWith True

-- | Webhook silence: like a failure, but a push source has no poll schedule
-- to back off, so next_poll_at stays NULL.
recordSilence :: (?modelContext :: ModelContext) => Source -> Text -> IO ()
recordSilence = recordFailureWith False

recordFailureWith :: (?modelContext :: ModelContext) => Bool -> Source -> Text -> IO ()
recordFailureWith setBackoff source err = do
    now <- getCurrentTime
    let failures = source.consecutiveFailures + 1
        delay = jitteredBackoff (healthFingerprint (get #id source)) source.pollIntervalSeconds failures
    updated <-
        source
            |> set #consecutiveFailures failures
            |> set #lastError (Just err)
            |> set #nextPollAt (if setBackoff then Just (addUTCTime (fromIntegral delay) now) else Nothing)
            |> updateRecord
    maybeAlertId <-
        ingest
            updated
            NormalizedEvent
                { fingerprint = healthFingerprint (get #id source)
                , externalId = Nothing
                , status = Firing
                , severity = if failures >= escalateAfterFailures then "high" else "warning"
                , title = "Source " <> source.name <> " unhealthy"
                , description = err
                , env = Just source.env
                , host = Nothing
                , service = Nothing
                , checkName = Just "source_health"
                , labels = object ["source" .= source.name, "kind" .= ("source_health" :: Text)]
                , annotations = object []
                , startedAt = Just now
                , sourceUrl = Nothing
                }
    when (failures >= escalateAfterFailures) do
        forM_ maybeAlertId \alertId -> do
            alert <- fetch alertId
            when (alert.severity /= "high" && alert.status /= "closed") do
                _ <-
                    alert
                        |> set #severity "high"
                        |> set #updatedAt now
                        |> updateRecord
                void do
                    newRecord @AlertEvent
                        |> set #alertId alertId
                        |> set #userId Nothing
                        |> set #kind "severity_upgraded"
                        |> set #payload (object ["from" .= alert.severity, "to" .= ("high" :: Text), "failures" .= failures])
                        |> createRecord
                publishAlertUpdate alert "updated"

-- | Recovery: reset backoff state and post a resolved event for the internal
-- alert (a no-op when the source was healthy or no such alert exists).
recordSuccess :: (?modelContext :: ModelContext) => Source -> IO ()
recordSuccess source =
    when (source.consecutiveFailures > 0 || isJust source.nextPollAt || isJust source.lastError) do
        now <- getCurrentTime
        updated <-
            source
                |> set #consecutiveFailures 0
                |> set #lastError Nothing
                |> set #nextPollAt Nothing
                |> updateRecord
        void do
            ingest
                updated
                NormalizedEvent
                    { fingerprint = healthFingerprint (get #id source)
                    , externalId = Nothing
                    , status = Resolved
                    , severity = "warning"
                    , title = "Source " <> source.name <> " unhealthy"
                    , description = "source recovered"
                    , env = Just source.env
                    , host = Nothing
                    , service = Nothing
                    , checkName = Just "source_health"
                    , labels = object ["source" .= source.name, "kind" .= ("source_health" :: Text)]
                    , annotations = object []
                    , startedAt = Just now
                    , sourceUrl = Nothing
                    }

-- Reconcile-path health (halemans:source-reconcile:<source_id>): the reverse
-- state sync (trigger.get for zabbix) can fail while polling itself is fine
-- — classically a token role missing the method from its API allow-list.
-- No backoff state is touched; the alert is the only signal, resolved on the
-- first successful reconcile after the fix.

reconcileFingerprint :: Id Source -> Text
reconcileFingerprint sourceId = "halemans:source-reconcile:" <> tshow sourceId

recordReconcileFailure :: (?modelContext :: ModelContext) => Source -> Text -> IO ()
recordReconcileFailure source err = do
    now <- getCurrentTime
    void $
        ingest
            source
            NormalizedEvent
                { fingerprint = reconcileFingerprint (get #id source)
                , externalId = Nothing
                , status = Firing
                , severity = "warning"
                , title = "Source " <> source.name <> " state reconcile failing"
                , description = err
                , env = Just source.env
                , host = Nothing
                , service = Nothing
                , checkName = Just "source_reconcile"
                , labels = object ["source" .= source.name, "kind" .= ("source_reconcile" :: Text)]
                , annotations = object []
                , startedAt = Just now
                , sourceUrl = Nothing
                }

-- | No-op unless a reconcile alert is currently open (avoids a write per
-- successful cycle).
recordReconcileSuccess :: (?modelContext :: ModelContext) => Source -> IO ()
recordReconcileSuccess source = do
    open <-
        query @Alert
            |> filterWhere (#fingerprint, reconcileFingerprint (get #id source))
            |> filterWhereIn (#status, ["firing", "ack"] :: [Text])
            |> fetchOneOrNothing
    forM_ open \_ -> do
        now <- getCurrentTime
        void $
            ingest
                source
                NormalizedEvent
                    { fingerprint = reconcileFingerprint (get #id source)
                    , externalId = Nothing
                    , status = Resolved
                    , severity = "warning"
                    , title = "Source " <> source.name <> " state reconcile failing"
                    , description = "reconcile recovered"
                    , env = Just source.env
                    , host = Nothing
                    , service = Nothing
                    , checkName = Just "source_reconcile"
                    , labels = object ["source" .= source.name, "kind" .= ("source_reconcile" :: Text)]
                    , annotations = object []
                    , startedAt = Just now
                    , sourceUrl = Nothing
                    }
