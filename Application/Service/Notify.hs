module Application.Service.Notify
( dispatchNotification
, notifyMinIntervalSeconds
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Generated.Types
import Data.Aeson (Value, object, (.=))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- Phase-1 simplified notification dispatch (design_docs/milestone_1.md §3
-- step 6): no rules engine yet. Phase 2 replaces this single call site with
-- NotificationRule evaluation.

-- | Min interval between notifications per alert (throttle). Default 5m,
-- override via HALEMANS_NOTIFY_MIN_INTERVAL_SECONDS.
notifyMinIntervalSeconds :: IO Int
notifyMinIntervalSeconds = do
    override <- lookupEnv "HALEMANS_NOTIFY_MIN_INTERVAL_SECONDS"
    pure (fromMaybe 300 (override >>= readMaybe))

-- | Enqueue a push notification for the alert unless throttled. Records an
-- AlertEvent(notified) at enqueue time so the throttle holds across storms.
dispatchNotification :: (?modelContext :: ModelContext) => Alert -> IO Bool
dispatchNotification alert = do
    intervalSeconds <- notifyMinIntervalSeconds
    let alertId = get #id alert
    let intervalSecs = fromIntegral intervalSeconds :: Double
    recent <- sqlQueryTyped [typedSql|
        SELECT count(*) FROM alert_events
        WHERE alert_id = ${alertId}
          AND kind = 'notified'
          AND created_at > now() - make_interval(secs => ${intervalSecs})
    |]
    let recentCount = case recent of
            (n:_) -> n
            [] -> 0
    if recentCount > 0
        then pure False
        else do
            recordNotifiedEvent alertId
            _ <- newRecord @PushNotificationJob
                |> set #alertId alertId
                |> createRecord
            pure True

recordNotifiedEvent :: (?modelContext :: ModelContext) => Id Alert -> IO ()
recordNotifiedEvent alertId = do
    let payload :: Value = object ["note" .= ("push notification enqueued" :: Text)]
    _ <- newRecord @AlertEvent
        |> set #alertId alertId
        |> set #userId Nothing
        |> set #kind "notified"
        |> set #payload payload
        |> createRecord
    pure ()
