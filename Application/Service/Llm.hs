module Application.Service.Llm (
    LlmProviderConfig (..),
    llmConfigFromEnv,
    LlmError (..),
    Completion (..),
    LlmMessage (..),
    userMessage,
    assistantMessage,
    toolResultMessage,
    ToolCall (..),
    Prompt (..),
    LlmProvider (..),
    OpenAiCompat (..),
    connectionOk,
    apiUrl,
    chatCompletionPayload,
) where

import qualified Application.Service.Http as Http
import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson (Value, object, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import IHP.Prelude
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wreq as Wreq
import Network.Wreq.Lens (checkResponse)
import System.Environment (lookupEnv)

-- LLM provider subsystem (design_docs/milestone_4.md §3, 01_highlevel.md §9).
-- v1: a single OpenAI-compatible chat-completions client that covers local
-- llama.cpp/vLLM endpoints and hosted APIs. Advisory only — nothing in this
-- module may feed back into pipeline actions (milestone_4.md D8).

-- Runtime-resolved provider config (the generated LlmConfig record is the
-- llm_configs table row; milestone_7.md §7). DB-first resolution lives in
-- Application.Service.Llm.DbConfig (separate module: the generated record
-- shares field names with this one).
data LlmProviderConfig = LlmProviderConfig
    { providerName :: Text
    , endpoint :: Text
    , model :: Text
    , apiKey :: Maybe Text
    , toolsEnabled :: Bool
    }
    deriving (Eq, Show)

-- Env fallback (01_highlevel.md §14: secrets never in DB plaintext).
-- LLM_ENDPOINT/LLM_MODEL required; LLM_API_KEY optional (local endpoints);
-- LLM_PROVIDER_NAME defaults to "default" (budget counters key on it);
-- LLM_TOOLS=1 enables read-only tool calling (milestone_4.md D4a).
llmConfigFromEnv :: IO (Maybe LlmProviderConfig)
llmConfigFromEnv = do
    endpoint <- lookupEnv "LLM_ENDPOINT"
    model <- lookupEnv "LLM_MODEL"
    apiKey <- lookupEnv "LLM_API_KEY"
    providerName <- lookupEnv "LLM_PROVIDER_NAME"
    tools <- lookupEnv "LLM_TOOLS"
    pure case (endpoint, model) of
        (Just endpoint, Just model) ->
            Just
                LlmProviderConfig
                    { providerName = maybe "default" cs providerName
                    , endpoint = cs endpoint
                    , model = cs model
                    , apiKey = cs <$> apiKey
                    , toolsEnabled = tools == Just "1"
                    }
        _ -> Nothing

data LlmError = Retriable Text | Terminal Text deriving (Eq, Show)

data LlmMessage = LlmMessage
    { role :: Text
    , content :: Text
    , toolCallId :: Maybe Text
    , msgToolCalls :: [ToolCall]
    }
    deriving (Eq, Show)

userMessage :: Text -> LlmMessage
userMessage content = LlmMessage{role = "user", content, toolCallId = Nothing, msgToolCalls = []}

assistantMessage :: [ToolCall] -> LlmMessage
assistantMessage calls = LlmMessage{role = "assistant", content = "", toolCallId = Nothing, msgToolCalls = calls}

toolResultMessage :: Text -> Text -> LlmMessage
toolResultMessage callId content = LlmMessage{role = "tool", content, toolCallId = Just callId, msgToolCalls = []}

data ToolCall = ToolCall
    { callId :: Text
    , callName :: Text
    , callArguments :: Text
    }
    deriving (Eq, Show)

data Completion = Completion
    { content :: Text
    , tokensIn :: Maybe Int
    , tokensOut :: Maybe Int
    , toolCalls :: [ToolCall]
    }
    deriving (Eq, Show)

data Prompt = Prompt
    { messages :: [LlmMessage]
    , tools :: [Value]
    }
    deriving (Eq, Show)

class LlmProvider p where
    complete :: p -> Prompt -> IO (Either LlmError Completion)

data OpenAiCompat = OpenAiCompat {config :: LlmProviderConfig}

instance LlmProvider OpenAiCompat where
    complete provider prompt = chatCompletion provider.config prompt

apiUrl :: LlmProviderConfig -> Text -> Text
apiUrl config path = Text.dropWhileEnd (== '/') config.endpoint <> path

-- Admin "connection test": GET /v1/models (milestone_4.md §7).
connectionOk :: LlmProviderConfig -> IO (Either Text ())
connectionOk config = do
    result <- try (Http.getFollowing (opts config) (cs (apiUrl config "/v1/models")))
    pure case result of
        Left (err :: SomeException) -> Left (tshow err)
        Right response ->
            let code = statusCode (response ^. Wreq.responseStatus)
             in if code == 200 then Right () else Left ("status " <> tshow code)

chatCompletionPayload :: LlmProviderConfig -> Prompt -> Value
chatCompletionPayload config prompt =
    object $
        catMaybes
            [ Just ("model" .= config.model)
            , Just ("messages" .= map messageJson prompt.messages)
            , if null prompt.tools then Nothing else Just ("tools" .= prompt.tools)
            ]

chatCompletion :: LlmProviderConfig -> Prompt -> IO (Either LlmError Completion)
chatCompletion config prompt = do
    let payload = chatCompletionPayload config prompt
    result <- try (Http.postFollowing (opts config) (cs (apiUrl config "/v1/chat/completions")) payload)
    pure case result of
        Left err -> Left (Retriable (tshow (err :: SomeException)))
        Right response ->
            let code = statusCode (response ^. Wreq.responseStatus)
                bodyText = Text.strip (cs (response ^. Wreq.responseBody))
                suffix = if Text.null bodyText then "" else ": " <> Text.take 800 bodyText
             in if
                    | code >= 200 && code < 300 -> decodeCompletion response
                    | code == 429 || code >= 500 -> Left (Retriable ("http " <> tshow code <> suffix))
                    | otherwise -> Left (Terminal ("http " <> tshow code <> suffix))

opts :: LlmProviderConfig -> Wreq.Options
opts config =
    Wreq.defaults
        & Wreq.manager
            .~ Left
                ( HTTP.tlsManagerSettings
                    { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro (120 * 1000000)
                    }
                )
        & checkResponse .~ Just (\_ _ -> pure ())
        & Wreq.header "Content-Type" .~ ["application/json"]
        & Wreq.header "Authorization" .~ maybe [] (\key -> ["Bearer " <> cs key]) config.apiKey

messageJson :: LlmMessage -> Value
messageJson message =
    object $
        catMaybes
            [ Just ("role" .= message.role)
            , Just ("content" .= message.content)
            , ("tool_call_id" .=) <$> message.toolCallId
            , if null message.msgToolCalls then Nothing else Just ("tool_calls" .= map toolCallJson message.msgToolCalls)
            ]

toolCallJson :: ToolCall -> Value
toolCallJson call =
    object
        [ "id" .= call.callId
        , "type" .= ("function" :: Text)
        , "function" .= object ["name" .= call.callName, "arguments" .= call.callArguments]
        ]

decodeCompletion :: Wreq.Response LByteString -> Either LlmError Completion
decodeCompletion response = case Aeson.eitherDecode (response ^. Wreq.responseBody) of
    Left err -> Left (Terminal ("undecodable response: " <> cs err))
    Right body -> case parseMaybe parseChatResponse body of
        Nothing -> Left (Terminal "response missing choices")
        Just completion -> Right completion

parseChatResponse :: Value -> Parser Completion
parseChatResponse = Aeson.withObject "chat.completion" \o -> do
    choices <- o .: "choices"
    case choices of
        (choice : _) -> do
            message <- choice .: "message"
            content <- message .:? "content" .!= ""
            rawCalls <- message .:? "tool_calls" .!= []
            toolCalls <- mapM parseToolCall rawCalls
            usage <- o .:? "usage"
            (tokensIn, tokensOut) <- case usage of
                Nothing -> pure (Nothing, Nothing)
                Just u -> do
                    tokensIn <- u .:? "prompt_tokens"
                    tokensOut <- u .:? "completion_tokens"
                    pure (tokensIn, tokensOut)
            pure Completion{..}
        [] -> fail "empty choices"

parseToolCall :: Value -> Parser ToolCall
parseToolCall = Aeson.withObject "tool_call" \o -> do
    callId <- o .: "id"
    function <- o .: "function"
    callName <- function .: "name"
    callArguments <- function .: "arguments"
    pure ToolCall{..}
