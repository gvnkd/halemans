module Web.Controller.Api where

import Application.Service.Api.Alerts (AlertFilters (..), alertDetail, defaultFilters, listAlertsPage)
import Application.Service.Api.Auth (apiError, withApiToken)
import Application.Service.Api.Cursor (decodeCursor)
import Application.Service.Api.Encode (encodeAlertDetail, encodeAlertSummary, encodeEnvCard)
import Application.Service.Api.RateLimit (LimitClass (..))
import Data.Aeson (object, (.=))
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Network.HTTP.Types (status400, status404)
import Text.Read (readMaybe)
import Web.Controller.Prelude
import Web.View.Dashboard.Index (computeEnvCards)

-- Read-only JSON API (design_docs/milestone_6.md §3). Keyset pagination on
-- (last_seen_at desc, id desc); the cursor is an opaque base64 of the last
-- row's key, stable under concurrent inserts.
instance Controller ApiController where
    action ApiAlertsAction = withApiToken LimitApi "alerts:read" \_ _ -> do
        parsed <- parseListParams
        case parsed of
            Left badParam -> apiError status400 "bad_request" ("invalid " <> badParam <> " parameter")
            Right filters -> do
                (alerts, nextCursor) <- listAlertsPage filters
                renderJson
                    ( object
                        [ "alerts" .= map encodeAlertSummary alerts
                        , "next_cursor" .= nextCursor
                        ]
                    )
    action ApiAlertAction{alertId} = withApiToken LimitApi "alerts:read" \_ _ -> do
        detail <- alertDetail alertId
        case detail of
            Nothing -> apiError status404 "not_found" "unknown alert id"
            Just found -> renderJson (encodeAlertDetail found)
    action ApiEnvironmentsAction = withApiToken LimitApi "alerts:read" \_ _ -> do
        (cards, unassigned) <- computeEnvCards
        renderJson (object ["environments" .= map encodeEnvCard (cards ++ maybeToList unassigned)])

maxLimit :: Int
maxLimit = 500

defaultLimit :: Int
defaultLimit = 100

-- Left names the offending parameter (design §3: 400 with the param named).
parseListParams :: (?request :: Request, ?respond :: Respond) => IO (Either Text AlertFilters)
parseListParams = do
    let textParam :: Text -> Text
        textParam name = fromMaybe "" (paramOrNothing @Text (cs name))
    limit <- case paramOrNothing @Text "limit" of
        Nothing -> pure (Right defaultLimit)
        Just raw -> pure case readMaybe (cs raw) of
            Just parsed | parsed > 0 -> Right (min maxLimit parsed)
            _ -> Left "limit"
    since <- parseTimeParam "since" defaultFilters.afSince
    until <- parseTimeParam "until" defaultFilters.afUntil
    let cursor = case paramOrNothing @Text "cursor" of
            Nothing -> Right Nothing
            Just raw -> maybe (Left "cursor") (Right . Just) (decodeCursor raw)
    pure do
        afLimit <- limit
        afSince <- since
        afUntil <- until
        afCursor <- cursor
        pure
            AlertFilters
                { afEnvironment = textParam "environment"
                , afStatus = textParam "status"
                , afSeverity = textParam "severity"
                , afFingerprint = textParam "fingerprint"
                , afHost = textParam "host"
                , afService = textParam "service"
                , afSince
                , afUntil
                , afCursor
                , afLimit
                }

parseTimeParam :: (?request :: Request, ?respond :: Respond) => Text -> UTCTime -> IO (Either Text UTCTime)
parseTimeParam name fallback = case paramOrNothing @Text (cs name) of
    Nothing -> pure (Right fallback)
    Just raw -> pure $ maybe (Left name) Right (iso8601ParseM (cs raw))
