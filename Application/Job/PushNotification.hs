module Application.Job.PushNotification where

import IHP.Prelude
import IHP.Job.Types
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch)
import IHP.TypedSql (sqlQueryTyped, sqlExecTyped, typedSql)
import Generated.Types
import Application.Service.Push (VapidKeys (..), PushResult (..), loadVapidKeys, sendPush)
import qualified Data.Aeson as Aeson
import Data.Aeson (object, (.=))
import Control.Monad (void)

-- Delivers a browser push for one alert (design_docs/milestone_1.md §3 step
-- 6 simplified dispatch: every user with view/admin privilege and a push
-- subscription). Dead subscriptions (404/410) are removed.
instance Job PushNotificationJob where
    perform job = do
        alert <- fetch job.alertId
        keysOrNothing <- loadVapidKeys
        case keysOrNothing of
            Nothing -> pure () -- no VAPID keys yet (seed not run); soft-fail
            Just keys -> do
                subscriptions <- subscriptionsForViewers
                forM_ subscriptions \subscription -> do
                    result <- sendPush keys subscription (payload alert)
                    case result of
                        PushDelivered -> pure ()
                        PushSubscriptionGone -> deleteRecord subscription
                        PushFailed err -> error (cs err) -- job retry/backoff
      where
        payload alert = Aeson.encode (object
            [ "title" .= alert.title
            , "severity" .= alert.severity
            , "status" .= alert.status
            , "env" .= alert.env
            , "alertId" .= get #id alert
            , "url" .= ("/alerts/" <> tshow (get #id alert))
            ])

    maxAttempts = 5

-- | Push subscriptions belonging to users with the view (or admin) privilege.
subscriptionsForViewers :: (?modelContext :: ModelContext) => IO [PushSubscription]
subscriptionsForViewers = do
    rows <- sqlQueryTyped [typedSql|
        SELECT ps.id FROM push_subscriptions ps
        JOIN user_roles ur ON ur.user_id = ps.user_id
        JOIN roles r ON r.id = ur.role_id
        WHERE r.privileges @> ARRAY['view'] OR r.privileges @> ARRAY['admin']
        GROUP BY ps.id
    |]
    forM rows fetch
