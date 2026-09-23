module Application.Service.Api.InternalAuth (
    withInternalToken,
) where

import Application.Service.Api.Auth (apiError, apiErrorWith)
import Application.Service.Api.RateLimit (LimitClass (..), checkLimit, limitPerMinute)
import qualified Data.Text as Text
import Generated.Types
import IHP.Fetch (fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, query)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Network.HTTP.Types (status401, status403, status404, status429, status503)
import Network.Wai (Request, ResponseReceived, rawPathInfo, requestHeaders, requestMethod)
import System.Environment (lookupEnv)
import Wai.Request.Params.Middleware (Respond)

-- Internal API auth (internal API milestone). The internal surface is for the
-- local agent and tests only: it is unversioned, unstable, and disabled
-- entirely unless HALEMANS_INTERNAL_TOKEN is set. Callers must send the
-- marker header (so accidental external reliance is visible in every request
-- log) and act as an existing user via X-Act-As; every granted call is audit
-- logged with the acting user.

internalTokenEnv :: String
internalTokenEnv = "HALEMANS_INTERNAL_TOKEN"

withInternalToken ::
    (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext) =>
    (User -> IO ResponseReceived) ->
    IO ResponseReceived
withInternalToken handler = do
    expected <- lookupEnv internalTokenEnv
    case expected of
        Nothing -> apiError status503 "internal_disabled" "internal API is disabled (HALEMANS_INTERNAL_TOKEN is not set)"
        Just expected -> do
            let headers = requestHeaders ?request
                marker = lookup "X-Halemans-Internal" headers
                authorization = cs <$> lookup "Authorization" headers
                actAsEmail = cs <$> lookup "X-Act-As" headers
            if marker /= Just "1"
                then apiError status404 "not_found" "unknown endpoint"
                else case authorization of
                    Just value
                        | Just token <- Text.stripPrefix "Bearer " value
                        , token == cs expected ->
                            resolveActAs actAsEmail handler
                    _ -> apiError status401 "unauthorized" "invalid internal token"

resolveActAs ::
    (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext) =>
    Maybe Text ->
    (User -> IO ResponseReceived) ->
    IO ResponseReceived
resolveActAs Nothing _ = apiError status403 "forbidden" "missing X-Act-As header"
resolveActAs (Just email) handler = do
    user <- query @User |> filterWhere (#email, email) |> fetchOneOrNothing
    case user of
        Nothing -> apiError status403 "forbidden" "unknown act-as user"
        Just user -> do
            audit user
            perMinute <- limitPerMinute LimitApi
            overLimit <- checkLimit ("internal:" <> tshow (get #id user)) perMinute
            case overLimit of
                Just retryAfter ->
                    apiErrorWith status429 [("Retry-After", cs (show retryAfter))] "rate_limited" "rate limit exceeded"
                Nothing -> handler user
  where
    audit user = do
        let method = cs (requestMethod ?request) :: Text
            path = cs (rawPathInfo ?request) :: Text
            userId = get #id user
        _ <-
            sqlExecTyped
                [typedSql|
                INSERT INTO internal_api_audit (user_id, method, path)
                VALUES (${userId}, ${method}, ${path})
            |]
        pure ()
