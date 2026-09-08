module Application.Service.Assets
( AssetsClient (..)
, AssetsAuth (..)
, clientFromConfig
, apiUrl
, listSchemas
, listTypesFlat
, searchObjects
, searchAll
, objectDetail
, objectHistory
, connectedTickets
, statusType
, icon
, fetchBinary
, connectionOk
, AssetsError (..)
, describeError
) where

import IHP.Prelude
import Generated.Types (AssetsConfig, AssetsConfig' (..))
import Data.Aeson (Value)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq as Wreq
import Network.Wreq.Lens (checkResponse)
import Control.Lens ((&), (^.), (.~))
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types.Status (statusCode)
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)
import System.Environment (lookupEnv)
import Data.ByteArray.Encoding (convertToBase, Base (Base64))
import Application.Service.Assets.Types
import Application.Service.Assets.Aql (Aql (..))
import Application.Service.Assets.Errors (AssetsError (..), describeError, classifyResponse)

-- Read-only Jira Assets (Insight) client (design_docs/milestone_8.md §3,
-- assets-api.md §9/§10). Unlike the other connectors this client does NOT
-- route through Application.Service.Http: the Assets API 302s unknown/unauth
-- paths to an HTML login page (§8.6) and following that hides auth failures.
-- wreq is used directly with redirects disabled and checkResponse off.

data AssetsAuth
    = BearerAuth Text
    | BasicAuth Text Text -- email, token
    deriving (Eq, Show)

data AssetsClient = AssetsClient
    { clientBaseUrl :: Text
    , clientAuth :: AssetsAuth
    } deriving (Eq, Show)

-- Resolves the DB row + env token (token_env holds the env var NAME, never
-- the secret). basic auth additionally needs jira_email_env.
clientFromConfig :: AssetsConfig -> IO (Either Text AssetsClient)
clientFromConfig config = do
    token <- lookupEnv (cs config.tokenEnv)
    case token of
        Nothing -> pure (Left ("assets token env var not set: " <> config.tokenEnv))
        Just tokenValue
            | config.authMode == "basic" -> do
                case config.jiraEmailEnv of
                    Nothing -> pure (Left ("auth_mode basic needs jira_email_env on " <> config.name))
                    Just emailEnv -> do
                        email <- lookupEnv (cs emailEnv)
                        pure case email of
                            Nothing -> Left ("assets jira email env var not set: " <> emailEnv)
                            Just emailValue -> Right AssetsClient
                                { clientBaseUrl = config.baseUrl
                                , clientAuth = BasicAuth (cs emailValue) (cs tokenValue)
                                }
            | otherwise -> pure (Right AssetsClient
                { clientBaseUrl = config.baseUrl
                , clientAuth = BearerAuth (cs tokenValue)
                })

apiUrl :: AssetsClient -> Text -> Text
apiUrl client path = Text.dropWhileEnd (== '/') client.clientBaseUrl <> path

opts :: AssetsClient -> Wreq.Options
opts client = Wreq.defaults
    & Wreq.manager .~ Left (HTTP.tlsManagerSettings
        { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro (30 * 1000000) })
    & checkResponse .~ Just (\_ _ -> pure ())
    & Wreq.redirects .~ 0
    & Wreq.header "Accept" .~ ["application/json"]
    & Wreq.header "Authorization" .~ [authHeader client.clientAuth]

authHeader :: AssetsAuth -> ByteString
authHeader = \case
    BearerAuth token -> "Bearer " <> cs token
    BasicAuth email token -> "Basic " <> cs (convertToBase Base64 (cs (email <> ":" <> token) :: ByteString) :: ByteString)

-- GET with classification + bounded retry on 5xx/transport failure (max 2
-- retries, ~0.5s apart — same retry shape as the LLM client).
getJson :: AssetsClient -> Maybe Int64 -> Text -> [(Text, Text)] -> IO (Either AssetsError Value)
getJson client requestObjectId path params = go 0
    where
        go :: Int -> IO (Either AssetsError Value)
        go attempt = do
            let requestOpts = foldl (\o (k, v) -> o & Wreq.param k .~ [v]) (opts client) params
            result <- try @SomeException (Wreq.getWith requestOpts (cs (apiUrl client path)))
            case result of
                Left err
                    | attempt < 2 -> retry attempt
                    | otherwise -> pure (Left (Upstream 0 (tshow err)))
                Right response ->
                    let code = statusCode (response ^. Wreq.responseStatus)
                        body = response ^. Wreq.responseBody
                    in if
                        | code >= 500 && attempt < 2 -> retry attempt
                        | code >= 200 && code < 300 -> pure (decodeBody code body)
                        | otherwise -> pure (Left (classifyResponse requestObjectId code body))
        retry attempt = do
            threadDelay 500000
            go (attempt + 1)

decodeBody :: Int -> BL.ByteString -> Either AssetsError Value
decodeBody code body
    | looksHtml = Left (InvalidResponse code (cs (BL.take 200 body)))
    | otherwise = case Aeson.eitherDecode body of
        Left err -> Left (InvalidResponse code (cs err))
        Right value -> Right value
    where
        looksHtml = "<" `BL.isPrefixOf` BL.dropWhile (== 32) (BL.dropWhile (`elem` whitespace) body)
        whitespace = [32, 9, 10, 13]

listSchemas :: AssetsClient -> IO (Either AssetsError [ObjectSchema])
listSchemas client = decodeList ["objectschemas", "values", "objects"]
    <$> getJson client Nothing "/objectschema/list" []

listTypesFlat :: AssetsClient -> Int64 -> IO (Either AssetsError [ObjectType])
listTypesFlat client schemaId = decodeList ["objecttypes", "values", "objects"]
    <$> getJson client Nothing ("/objectschema/" <> tshow schemaId <> "/objecttypes/flat")
        [("includeObjectCounts", "true")]

-- GET /aql/objects with a 1-based page (assets-api.md §4.4). qlQuery is
-- URL-encoded by wreq's param handling.
searchObjects :: AssetsClient -> Aql -> Int -> Int -> IO (Either AssetsError ObjectListResult)
searchObjects client aql page resultPerPage = decodeOne
    <$> getJson client Nothing "/aql/objects"
        [ ("qlQuery", aql.aqlText)
        , ("page", tshow page)
        , ("resultPerPage", tshow resultPerPage)
        , ("includeAttributes", "true")
        , ("includeTypeAttributes", "false")
        ]

-- Pagination helper (§9): iterate while toIndex < totalFilterCount.
searchAll :: AssetsClient -> Aql -> Int -> IO (Either AssetsError [AssetObject])
searchAll client aql resultPerPage = go 1 []
    where
        go page acc = do
            result <- searchObjects client aql page resultPerPage
            case result of
                Left err -> pure (Left err)
                Right pageResult -> do
                    let acc' = acc ++ pageResult.listEntries
                    if hasMorePages pageResult && page < 20
                        then go (page + 1) acc'
                        else pure (Right acc')

objectDetail :: AssetsClient -> Int64 -> IO (Either AssetsError AssetObject)
objectDetail client objectId = decodeOne
    <$> getJson client (Just objectId) ("/object/" <> tshow objectId) []

objectHistory :: AssetsClient -> Int64 -> IO (Either AssetsError [ObjectHistory])
objectHistory client objectId = decodeList ["objectHistoryEntries", "values", "history"]
    <$> getJson client (Just objectId) ("/object/" <> tshow objectId <> "/history") [("asc", "true")]

connectedTickets :: AssetsClient -> Int64 -> IO (Either AssetsError [Ticket])
connectedTickets client objectId = decodeList ["tickets", "values"]
    <$> getJson client (Just objectId) ("/objectconnectedtickets/" <> tshow objectId <> "/tickets") []

statusType :: AssetsClient -> Int64 -> IO (Either AssetsError StatusType)
statusType client statusId = decodeOne
    <$> getJson client (Just statusId) ("/config/statustype/" <> tshow statusId) []

icon :: AssetsClient -> Int64 -> IO (Either AssetsError Icon)
icon client iconId = decodeOne
    <$> getJson client (Just iconId) ("/icon/" <> tshow iconId) []

-- Raw GET of an absolute URL (icon/avatar images live on the Jira origin,
-- outside the /rest/assets/latest API base). Same auth + no-redirect policy
-- as the JSON endpoints; returns the upstream content type and the body.
fetchBinary :: AssetsClient -> Text -> IO (Either AssetsError (Text, BL.ByteString))
fetchBinary client url = do
    result <- try @SomeException (Wreq.getWith (opts client) (cs url))
    pure case result of
        Left err -> Left (Upstream 0 (tshow err))
        Right response ->
            let code = statusCode (response ^. Wreq.responseStatus)
                body = response ^. Wreq.responseBody
            in if code >= 200 && code < 300
                then Right (cs (response ^. Wreq.responseHeader "Content-Type"), body)
                else Left (classifyResponse Nothing code body)

-- Admin "connection test" (milestone_8.md §8): listSchemas reachability.
connectionOk :: AssetsClient -> IO (Either Text ())
connectionOk client = do
    result <- listSchemas client
    pure case result of
        Right _ -> Right ()
        Left err -> Left (describeError err)

decodeOne :: Aeson.FromJSON a => Either AssetsError Value -> Either AssetsError a
decodeOne (Left err) = Left err
decodeOne (Right value) = case Aeson.fromJSON value of
    Aeson.Success parsed -> Right parsed
    Aeson.Error err -> Left (InvalidResponse 200 (cs err))

decodeList :: Aeson.FromJSON a => [Text] -> Either AssetsError Value -> Either AssetsError [a]
decodeList keys (Left err) = Left err
decodeList keys (Right value) = case parseMaybe (envelopeParser keys) value of
    Just parsed -> Right parsed
    Nothing -> Left (InvalidResponse 200 "undecodable list envelope")
