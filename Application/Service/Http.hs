module Application.Service.Http (
    HttpStatusError (..),
    isDeterministicClientError,
    statusCodeOfError,
    getFollowing,
    getFollowingStream,
    postFollowingStream,
    postFollowing,
    putFollowing,
    deleteFollowing,
) where

import Control.Exception (Exception)
import Control.Lens (view, (&), (.~), (^.), (^?))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as Text
import IHP.Prelude
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (Header, statusCode)
import Network.URI (nonStrictRelativeTo, parseURI, parseURIReference, uriToString)
import qualified Network.Wreq as Wreq
import Text.Read (readMaybe)

-- | Non-2xx final response, mirroring wreq's default checkResponse semantics
-- (which these wrappers must disable per-hop to follow redirects manually).
data HttpStatusError = HttpStatusError String Int deriving stock (Show)

instance Exception HttpStatusError

maxRedirectHops :: Int
maxRedirectHops = 5

-- | GET, following 301/302/303/307/308 manually. wreq/http-client only
-- auto-follow 301/302 for GET/HEAD and strip the Authorization header when
-- the redirect target is a different host; these wrappers re-issue the full
-- request (headers, params, body) on every hop, so cross-host scheme/host
-- redirects behind reverse proxies keep working for all methods.
getFollowing :: Wreq.Options -> String -> IO (Wreq.Response L.ByteString)
getFollowing = follow Wreq.getWith

postFollowing :: Wreq.Options -> String -> Aeson.Value -> IO (Wreq.Response L.ByteString)
postFollowing opts url body = follow (\o u -> Wreq.postWith o u body) opts url

putFollowing :: Wreq.Options -> String -> Aeson.Value -> IO (Wreq.Response L.ByteString)
putFollowing opts url body = follow (\o u -> Wreq.putWith o u body) opts url

deleteFollowing :: Wreq.Options -> String -> IO (Wreq.Response L.ByteString)
deleteFollowing = follow Wreq.deleteWith

-- | GET with a streaming body consumer: same manual redirect following as
-- getFollowing (re-sending the same headers on every hop), but the response
-- body is consumed chunk-by-chunk from the socket and never buffered. This
-- is the tool for endpoints whose listings can be arbitrarily large (the
-- grafana alertmanager alerts API) — decoding the full response as one
-- aeson value OOMed the worker on big sources. Non-2xx final responses
-- still throw HttpStatusError, like getFollowing without a custom
-- checkResponse.
getFollowingStream :: HTTP.Manager -> String -> [Header] -> (HTTP.Response HTTP.BodyReader -> IO a) -> IO a
getFollowingStream manager url headers consume =
    streamWith manager (addHeaders headers) url consume
  where
    addHeaders headers request = request{HTTP.requestHeaders = headers}

-- | POST with a JSON body and a streaming response consumer: the
-- getFollowingStream contract for endpoints that take a request body
-- (grafana's /api/ds/query). The body is encoded once and re-sent on every
-- redirect hop, mirroring postFollowing.
postFollowingStream :: HTTP.Manager -> String -> [Header] -> Aeson.Value -> (HTTP.Response HTTP.BodyReader -> IO a) -> IO a
postFollowingStream manager url headers payload consume =
    streamWith manager addBody url consume
  where
    addBody request =
        request
            { HTTP.requestHeaders = ("Content-Type", "application/json") : headers
            , HTTP.method = "POST"
            , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode payload)
            }

streamWith :: HTTP.Manager -> (HTTP.Request -> HTTP.Request) -> String -> (HTTP.Response HTTP.BodyReader -> IO a) -> IO a
streamWith manager customize url consume = go url maxRedirectHops
  where
    go current hopsLeft = do
        request0 <- HTTP.parseRequest current
        let request = (customize request0){HTTP.redirectCount = 0}
        HTTP.withResponse request manager \response -> do
            let code = statusCode (HTTP.responseStatus response)
            case lookup "Location" (HTTP.responseHeaders response) of
                Just location
                    | isRedirect code
                    , hopsLeft > 0
                    , Just target <- resolveRedirect current (cs (Text.strip (cs location))) -> do
                        -- Tiny redirect bodies: drain so the connection can
                        -- be reused, then follow manually.
                        drain (HTTP.responseBody response)
                        go target (hopsLeft - 1)
                _
                    | code >= 200 && code < 300 -> consume response
                    | otherwise -> throwIO (HttpStatusError current code)
    drain bodyReader = do
        chunk <- HTTP.brRead bodyReader
        unless (BS.null chunk) (drain bodyReader)

follow :: (Wreq.Options -> String -> IO (Wreq.Response L.ByteString)) -> Wreq.Options -> String -> IO (Wreq.Response L.ByteString)
follow issue opts url = go url maxRedirectHops
  where
    quietOpts = opts & Wreq.redirects .~ 0 & Wreq.checkResponse .~ Just (\_ _ -> pure ())
    go current hopsLeft = do
        response <- issue quietOpts current
        let code = statusCode (response ^. Wreq.responseStatus)
        case response ^? Wreq.responseHeader "Location" of
            Just location
                | isRedirect code && hopsLeft > 0
                , Just target <- resolveRedirect current (cs (Text.strip (cs location))) ->
                    go target (hopsLeft - 1)
            _ -> do
                -- Callers that set a custom checkResponse (e.g. LLM reads
                -- 429/500 bodies) get the raw final response; everyone
                -- else keeps wreq's default throw-on-non-2xx semantics.
                when (isNothing (view Wreq.checkResponse opts) && (code < 200 || code >= 300)) do
                    throwIO (HttpStatusError current code)
                pure response

isRedirect :: Int -> Bool
isRedirect code = code `elem` [301, 302, 303, 307, 308]

-- | 4xx responses (except 408/429) fail identically on every retry, so
-- callers use this on rendered HttpStatusError texts to stop re-enqueue
-- loops on deterministic client errors; 5xx/timeouts stay retryable.
isDeterministicClientError :: Text -> Bool
isDeterministicClientError err = case statusCodeOfError err of
    Just code -> code >= 400 && code < 500 && code `notElem` [408, 429]
    Nothing -> False

-- | The status code out of a rendered HttpStatusError text (Nothing for
-- non-HTTP failures like connection errors) — callers classify individual
-- codes (e.g. 404 = resource gone). The rendered text can carry a
-- HasCallStack backtrace after the code, so take the FIRST parseable
-- number (the quoted URL word never parses: it starts with a quote).
statusCodeOfError :: Text -> Maybe Int
statusCodeOfError text = do
    rest <- Text.stripPrefix "HttpStatusError " text
    listToMaybe [code | word <- Text.words rest, Just code <- [readMaybe (Text.unpack word)]]

resolveRedirect :: String -> String -> Maybe String
resolveRedirect current location = do
    base <- parseURI current
    target <- parseURIReference location
    pure (uriToString id (nonStrictRelativeTo target base) "")
