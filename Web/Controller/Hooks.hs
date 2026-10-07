module Web.Controller.Hooks (parseBearerAuth) where

import qualified Application.Connector.Alertmanager as Alertmanager
import qualified Application.Connector.Grafana as Grafana
import Application.Helper.Ingest (NormalizedEvent, ingestEvents)
import Application.Service.Mattermost (actionSecret)
import Application.Service.Mattermost.Actions (ackFromMattermost)
import Application.Service.SourceHealth (recordSuccess)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as Text
import Network.HTTP.Types (status400, status403, status422)
import Network.Wai (requestHeaders)
import Web.Controller.Prelude

instance Controller HooksController where
    action HookAlertmanagerAction{token} = handleHook token (const Alertmanager.normalize)
    action HookGenericAction{token} = handleHook token \source ->
        if source.type_ == "grafana" then Grafana.normalizeAlertnameFirst else Grafana.normalize
    action HookMattermostAction{token} = do
        secret <- actionSecret
        if Text.null secret || token /= secret
            then renderJsonWithStatusCode status403 (Aeson.object ["error" .= ("invalid token" :: Text)])
            else do
                body <- getRequestBody
                case parseClick body of
                    Nothing ->
                        renderJsonWithStatusCode status400 (Aeson.object ["error" .= ("invalid action payload" :: Text)])
                    Just (alertId, username) -> do
                        result <- ackFromMattermost alertId username
                        case result of
                            Left err ->
                                renderJsonWithStatusCode status400 (Aeson.object ["error" .= err])
                            Right reply ->
                                renderJson (Aeson.object ["ephemeral_text" .= reply])

-- Mattermost interactive-action body: {context: {alertId}, user_name}.
parseClick :: L.ByteString -> Maybe (Id Alert, Text)
parseClick body = do
    payload <- either (const Nothing) Just (Aeson.eitherDecode body)
    alertRef <- parseMaybe (Aeson.withObject "click" (\o -> o Aeson..: "context")) payload >>= parseMaybe (Aeson.withObject "context" (\o -> o Aeson..: "alertId"))
    username <- parseMaybe (Aeson.withObject "click" (\o -> o Aeson..:? "user_name" Aeson..!= "")) payload
    pure (textToId (alertRef :: Text), username)

-- | Bearer-token fallback for ingestion hooks: when the URL path token
-- matches no webhook_tokens row, an `Authorization: Bearer <token>` header
-- is tried as well (Jenkins and other senders often prefer header auth over
-- embedding a secret in the URL).
hookBearerToken :: (?request :: Request) => Maybe Text
hookBearerToken = do
    value <- lookup "Authorization" (requestHeaders ?request)
    parseBearerAuth (cs value)

parseBearerAuth :: Text -> Maybe Text
parseBearerAuth value = do
    token <- Text.stripPrefix "Bearer " value
    let stripped = Text.strip token
    if Text.null stripped then Nothing else Just stripped

handleHook ::
    (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext) =>
    Text -> (Source -> Aeson.Value -> Either Text [NormalizedEvent]) -> IO ResponseReceived
handleHook token normalizeFor = do
    let candidates = nub ([token | not (Text.null token)] ++ maybeToList hookBearerToken)
    webhookTokens <- catMaybes <$> mapM lookupHookToken candidates
    case webhookTokens of
        [] ->
            renderJsonWithStatusCode status403 (Aeson.object ["error" .= ("invalid token" :: Text)])
        (webhookToken : _) -> do
            source <- fetch webhookToken.sourceId
            if not source.enabled
                then renderJsonWithStatusCode status403 (Aeson.object ["error" .= ("source disabled" :: Text)])
                else do
                    body <- getRequestBody
                    case Aeson.eitherDecode body of
                        Left err ->
                            renderJsonWithStatusCode status400 (Aeson.object ["error" .= (cs err :: Text)])
                        Right payload ->
                            case normalizeFor source payload of
                                Left err ->
                                    renderJsonWithStatusCode status422 (Aeson.object ["error" .= err])
                                Right events -> do
                                    _ <-
                                        newRecord @RawEvent
                                            |> set #sourceId (Just source.id)
                                            |> set #payload payload
                                            |> createRecord
                                    ingestEvents source events
                                    recordSuccess source
                                    renderJson (Aeson.object ["status" .= ("ok" :: Text)])

lookupHookToken :: (?modelContext :: ModelContext) => Text -> IO (Maybe WebhookToken)
lookupHookToken token =
    query @WebhookToken
        |> filterWhere (#token, token)
        |> fetchOneOrNothing
