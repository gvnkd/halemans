module Web.Controller.Hooks where

import qualified Application.Connector.Alertmanager as Alertmanager
import qualified Application.Connector.Grafana as Grafana
import Application.Helper.Ingest (NormalizedEvent, ingestEvents)
import Application.Service.SourceHealth (recordSuccess)
import qualified Data.Aeson as Aeson
import Network.HTTP.Types (status400, status403, status422)
import Web.Controller.Prelude

instance Controller HooksController where
    action HookAlertmanagerAction{token} = handleHook token Alertmanager.normalize
    action HookGenericAction{token} = handleHook token Grafana.normalize

handleHook ::
    (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext) =>
    Text -> (Aeson.Value -> Either Text [NormalizedEvent]) -> IO ResponseReceived
handleHook token normalize = do
    webhookToken <-
        query @WebhookToken
            |> filterWhere (#token, token)
            |> fetchOneOrNothing
    case webhookToken of
        Nothing ->
            renderJsonWithStatusCode status403 (Aeson.object ["error" .= ("invalid token" :: Text)])
        Just webhookToken -> do
            source <- fetch webhookToken.sourceId
            if not source.enabled
                then renderJsonWithStatusCode status403 (Aeson.object ["error" .= ("source disabled" :: Text)])
                else do
                    body <- getRequestBody
                    case Aeson.eitherDecode body of
                        Left err ->
                            renderJsonWithStatusCode status400 (Aeson.object ["error" .= (cs err :: Text)])
                        Right payload ->
                            case normalize payload of
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
