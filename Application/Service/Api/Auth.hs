module Application.Service.Api.Auth
    ( withApiToken
    , apiError
    , apiErrorWith
    , AuthDecision (..)
    , authorizeToken
    ) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.Fetch (fetch)
import Generated.Types
import Network.Wai (Request, Response, ResponseReceived, requestHeaders, responseLBS)
import Network.HTTP.Types (Status, ResponseHeaders, status401, status403, status429)
import IHP.Controller.Response (respondAndExit)
import Wai.Request.Params.Middleware (Respond)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Application.Service.Api.Token (resolveToken)
import Application.Service.Api.RateLimit (LimitClass (..), limitPerMinute, checkLimit)
import Application.Helper.Controller (userPrivileges)

-- JSON error envelope (design_docs/milestone_6.md §3): no HTML error pages
-- under /api/v1 or /metrics.
apiError :: (?request :: Request, ?respond :: Respond) => Status -> Text -> Text -> IO ResponseReceived
apiError status = apiErrorWith status []

apiErrorWith :: (?request :: Request, ?respond :: Respond) => Status -> ResponseHeaders -> Text -> Text -> IO ResponseReceived
apiErrorWith status headers code message =
    respondAndExit $ responseLBS status
        (("Content-Type", "application/json; charset=utf-8") : headers)
        (Aeson.encode (object ["error" .= code, "message" .= message]))

-- Auth decision, separated from the HTTP plumbing for testing: bearer
-- resolution, scope check, and the owner's current privileges (re-checked
-- per request, so demoting a user constrains their tokens).
data AuthDecision
    = Allow ApiToken User
    | Deny Status Text Text

authorizeToken :: (?modelContext :: ModelContext) => Maybe Text -> Text -> IO AuthDecision
authorizeToken authorization requiredScope = do
    token <- case authorization of
        Just value
            | Just plaintext <- Text.stripPrefix "Bearer " value
            , not (Text.null plaintext) -> resolveToken plaintext
        _ -> pure Nothing
    case token of
        Nothing -> pure (Deny status401 "unauthorized" "missing, revoked or expired bearer token")
        Just apiToken
            | requiredScope `notElem` apiToken.scopes ->
                pure (Deny status403 "insufficient_scope" ("token lacks the " <> requiredScope <> " scope"))
            | otherwise -> do
                user <- fetch apiToken.userId
                privileges <- userPrivileges apiToken.userId
                if "view" `elem` privileges
                    then pure (Allow apiToken user)
                    else pure (Deny status403 "forbidden" "token owner's account lacks the view privilege")

-- Bearer-token auth for /api/v1/* and /metrics (design §3/§5), with the
-- per-token rate limit applied after authentication.
withApiToken
    :: (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext)
    => LimitClass -> Text -> (ApiToken -> User -> IO ResponseReceived) -> IO ResponseReceived
withApiToken limitClass requiredScope handler = do
    let authorization = cs <$> lookup "Authorization" (requestHeaders ?request)
    decision <- authorizeToken authorization requiredScope
    case decision of
        Deny status code message -> apiError status code message
        Allow apiToken user -> do
            perMinute <- limitPerMinute limitClass
            overLimit <- checkLimit (bucketKey limitClass apiToken) perMinute
            case overLimit of
                Just retryAfter ->
                    apiErrorWith status429 [("Retry-After", cs (show retryAfter))] "rate_limited" "rate limit exceeded"
                Nothing -> handler apiToken user

bucketKey :: LimitClass -> ApiToken -> Text
bucketKey limitClass apiToken = prefix <> tshow (get #id apiToken)
    where prefix = case limitClass of
            LimitApi -> "api:"
            LimitMetrics -> "metrics:"
