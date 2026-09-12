module Web.Controller.PushSubscriptions where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Network.HTTP.Types (status400)
import qualified Network.Wai as Wai
import Web.Controller.Prelude

-- JSON API for browser push subscriptions (milestone_1.md §8). Session-authed.
instance Controller PushSubscriptionsController where
    beforeAction = ensureIsUser

    action SubscribePushAction = do
        body <- getRequestBody
        case parseSubscription body of
            Nothing -> renderJsonWithStatusCode status400 (Aeson.object ["error" .= ("invalid subscription" :: Text)])
            Just (endpoint, p256dh, auth) -> do
                existing <-
                    query @PushSubscription
                        |> filterWhere (#endpoint, endpoint)
                        |> fetchOneOrNothing
                case existing of
                    Just subscription -> do
                        _ <-
                            subscription
                                |> set #userId currentUserId
                                |> set #p256dh p256dh
                                |> set #auth auth
                                |> set #userAgent requestUserAgent
                                |> updateRecord
                        pure ()
                    Nothing -> do
                        _ <-
                            newRecord @PushSubscription
                                |> set #userId currentUserId
                                |> set #endpoint endpoint
                                |> set #p256dh p256dh
                                |> set #auth auth
                                |> set #userAgent requestUserAgent
                                |> createRecord
                        pure ()
                renderJson (Aeson.object ["status" .= ("ok" :: Text)])
    action UnsubscribePushAction = do
        body <- getRequestBody
        case parseSubscription body of
            Nothing -> renderJsonWithStatusCode status400 (Aeson.object ["error" .= ("invalid subscription" :: Text)])
            Just (endpoint, _, _) -> do
                existing <-
                    query @PushSubscription
                        |> filterWhere (#endpoint, endpoint)
                        |> filterWhere (#userId, currentUserId)
                        |> fetchOneOrNothing
                forM_ existing deleteRecord
                renderJson (Aeson.object ["status" .= ("ok" :: Text)])

requestUserAgent :: (?request :: Request) => Text
requestUserAgent = maybe "" cs (lookup "User-Agent" (Wai.requestHeaders ?request))

parseSubscription :: LByteString -> Maybe (Text, Text, Text)
parseSubscription body = do
    value <- Aeson.decode body
    flip parseMaybe value $ Aeson.withObject "subscription" \o -> do
        endpoint <- o Aeson..: "endpoint"
        keys <- o Aeson..: "keys"
        p256dh <- keys Aeson..: "p256dh"
        auth <- keys Aeson..: "auth"
        pure (endpoint, p256dh, auth)
