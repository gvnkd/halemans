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
    testIntegration,
    verifyStreamBody,
    chatCompletionStreaming,
    StreamStatus (..),
    apiUrl,
    chatCompletionPayload,
) where

import qualified Application.Service.Http as Http
import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~), (^.))
import Control.Monad (foldM, when)
import Data.Aeson (Value, object, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Data.Vector as Vector
import IHP.Prelude
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types as HttpTypes
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

-- Admin "connection test": GET /v1/models (milestone_4.md §7).
connectionOk :: LlmProviderConfig -> IO (Either Text ())
connectionOk config = do
    result <- try (Http.getFollowing (opts config) (cs (apiUrl config "/v1/models")))
    pure case result of
        Left (err :: SomeException) -> Left (tshow err)
        Right response ->
            let code = HttpTypes.statusCode (response ^. Wreq.responseStatus)
             in if code == 200 then Right () else Left ("status " <> tshow code)

-- Chat-agent streaming completion (internal API milestone). Consumes the
-- provider's SSE stream chunk by chunk so the UI can render progress; the
-- full Completion is still assembled at the end (content + usage), so the
-- tool loop treats it exactly like the buffered path. Used ONLY for the
-- final answer round — tool rounds stay buffered.
--
-- The callback fires with the running status after every content delta and
-- once at stream end (with the final accumulated content).
data StreamStatus = StreamStatus
    { stContent :: Text
    , stWords :: Int
    , stElapsedMs :: Int
    }
    deriving (Eq, Show)

chatCompletionStreaming :: LlmProviderConfig -> Prompt -> (StreamStatus -> IO ()) -> IO (Either LlmError Completion)
chatCompletionStreaming config prompt onStatus = do
    started <- getCurrentTime
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    request0 <- HTTP.parseRequest (cs (apiUrl config "/v1/chat/completions"))
    let request =
            request0
                { HTTP.method = "POST"
                , HTTP.requestHeaders =
                    [ ("Content-Type", "application/json")
                    , ("Authorization", maybe "Bearer none" (("Bearer " <>) . cs) config.apiKey)
                    , ("Accept", "text/event-stream")
                    ]
                , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode (streamingPayload config prompt))
                , HTTP.responseTimeout = HTTP.responseTimeoutMicro (120 * 1000000)
                }
    result <- try do
        HTTP.withResponse request manager \response -> do
            let code = HttpTypes.statusCode (HTTP.responseStatus response)
            if code >= 200 && code < 300
                then consumeStream started response
                else do
                    body <- cs <$> HTTP.brReadSome (HTTP.responseBody response) 4096
                    pure (Left (Terminal ("http " <> tshow code <> ": " <> Text.take 300 body)))
    case result of
        Left (err :: SomeException) -> pure (Left (Retriable (tshow err)))
        Right outcome -> pure outcome
  where
    consumeStream started response = go mempty [] Nothing
      where
        bodyReader = HTTP.responseBody response
        go accContent accCalls mUsage = do
            chunk <- HTTP.brRead bodyReader
            if BS.null chunk
                then finish accContent accCalls mUsage
                else do
                    -- SSE frames are line-based; split on newlines and keep
                    -- any partial line in the accumulator.
                    let (frames, rest) = splitFrames (cs chunk :: Text)
                    outcome <- foldM (step started) (Right (accContent, accCalls, mUsage, rest)) frames
                    case outcome of
                        Left done -> pure done
                        Right (accContent', accCalls', mUsage', _rest') -> go accContent' accCalls' mUsage'
        finish accContent accCalls mUsage = do
            now <- getCurrentTime
            emitStatus started accContent
            let toolCalls = finalizeCalls accCalls
            pure
                ( Right
                    Completion
                        { content = accContent
                        , tokensIn = mUsage >>= fst
                        , tokensOut = mUsage >>= snd
                        , toolCalls = toolCalls
                        }
                )
        step started' (Right (accContent, accCalls, mUsage, rest)) frame
            | Text.null frame = pure (Right (accContent, accCalls, mUsage, rest))
            | Just payload <- Text.stripPrefix "data:" frame = case Text.strip payload of
                "[DONE]" -> do
                    -- stream end: the final state is already accumulated
                    now <- getCurrentTime
                    emitStatus started' accContent
                    let toolCalls = finalizeCalls accCalls
                    pure
                        ( Left
                            ( Right
                                Completion
                                    { content = accContent
                                    , tokensIn = mUsage >>= fst
                                    , tokensOut = mUsage >>= snd
                                    , toolCalls = toolCalls
                                    }
                            )
                        )
                other -> case Aeson.decode (cs other) of
                    Nothing -> pure (Right (accContent, accCalls, mUsage, rest))
                    Just chunkValue -> do
                        let deltaContent = chunkContent chunkValue
                            accContent' = accContent <> deltaContent
                            accCalls' = mergeCalls accCalls (chunkToolCallParts chunkValue)
                            mUsage' = case chunkUsage chunkValue of
                                Just usage -> Just usage
                                Nothing -> mUsage
                        when (not (Text.null deltaContent)) (emitStatus started' accContent')
                        pure (Right (accContent', accCalls', mUsage', rest))
            | otherwise = pure (Right (accContent, accCalls, mUsage, rest))
        step _ (Left done) _ = pure (Left done)
        emitStatus started' content = do
            now <- getCurrentTime
            onStatus
                StreamStatus
                    { stContent = content
                    , stWords = length (Text.words content)
                    , stElapsedMs = round (diffUTCTime now started' * 1000)
                    }
    finalizeCalls calls =
        [ ToolCall callId callName callArguments
        | (callId, callName, callArguments) <- calls
        , not (Text.null callName)
        ]

streamingPayload :: LlmProviderConfig -> Prompt -> Value
streamingPayload config prompt =
    object $
        catMaybes
            [ Just ("model" .= config.model)
            , Just ("messages" .= map messageJson prompt.messages)
            , if null prompt.tools then Nothing else Just ("tools" .= prompt.tools)
            , Just ("stream" .= True)
            ]

-- SSE frame splitting: input chunk text + carry-over handled by caller
-- keeping 'rest'. Frames arrive as lines; blank lines separate events.
splitFrames :: Text -> ([Text], Text)
splitFrames input =
    let (complete, rest) = case Text.breakOnEnd "\n" input of
            (before, _) | Text.isSuffixOf "\n" input -> (Text.dropEnd 1 before, "")
            _ -> ("", input)
     in (Text.splitOn "\n" complete, rest)

chunkContent :: Value -> Text
chunkContent value = fromMaybe "" do
    choices <- lookupKey "choices" value
    firstChoice <- case choices of
        Aeson.Array items | (item : _) <- Vector.toList items -> Just item
        _ -> Nothing
    message <- lookupKey "message" firstChoice `orElse` Just firstChoice
    delta <- lookupKey "delta" message `orElse` Just message
    content <- lookupKey "content" delta
    case content of
        Aeson.String text -> Just text
        _ -> Nothing
  where
    orElse (Just a) _ = Just a
    orElse Nothing b = b

-- Accumulated tool-call parts across chunks: (id, name, arguments).
chunkToolCallParts :: Value -> [(Text, Text, Text)]
chunkToolCallParts value = fromMaybe [] do
    choices <- lookupKey "choices" value
    firstChoice <- case choices of
        Aeson.Array items | (item : _) <- Vector.toList items -> Just item
        _ -> Nothing
    message <- lookupKey "message" firstChoice `orElse` Just firstChoice
    delta <- lookupKey "delta" message `orElse` Just message
    calls <- lookupKey "tool_calls" delta
    case calls of
        Aeson.Array items -> mapM parseCallPart (Vector.toList items)
        _ -> Nothing
  where
    orElse (Just a) _ = Just a
    orElse Nothing b = b
    parseCallPart item = do
        function <- lookupKey "function" item
        let textField key value = case lookupKey key value of
                Just (Aeson.String text) -> Just text
                _ -> Nothing
        pure
            ( fromMaybe "" (textField "id" item)
            , fromMaybe "" (textField "name" function)
            , fromMaybe "" (textField "arguments" function)
            )

mergeCalls :: [(Text, Text, Text)] -> [(Text, Text, Text)] -> [(Text, Text, Text)]
mergeCalls existing [] = existing
mergeCalls existing ((callId, name, args) : rest) =
    mergeCalls (insertOrAppend existing) rest
  where
    insertOrAppend [] = [(callId, name, args)]
    insertOrAppend ((eid, ename, eargs) : xs)
        | eid == callId && not (Text.null eid) = (eid, if Text.null name then ename else name, eargs <> args) : xs
        | eid == callId && Text.null eid && ename == name = (eid, ename, eargs <> args) : xs
        | otherwise = (eid, ename, eargs) : insertOrAppend xs

chunkUsage :: Value -> Maybe (Maybe Int, Maybe Int)
chunkUsage value = do
    usage <- lookupKey "usage" value
    let tokensIn = case lookupKey "prompt_tokens" usage of
            Just (Aeson.Number n) -> Just (floor n)
            _ -> Nothing
        tokensOut = case lookupKey "completion_tokens" usage of
            Just (Aeson.Number n) -> Just (floor n)
            _ -> Nothing
    pure (tokensIn, tokensOut)

lookupKey :: Text -> Value -> Maybe Value
lookupKey key value = case value of
    Aeson.Object obj -> KeyMap.lookup (Key.fromText key) obj
    _ -> Nothing

-- Admin "test integration": a minimal chat ping in BOTH non-streaming and
-- streaming modes. Catches providers that answer /v1/models but fail chat
-- completions (auth scope, model name, missing stream support). Each phase
-- gets a short timeout so a half-open stream can't hang the admin page.
testIntegration :: LlmProviderConfig -> IO (Either Text ())
testIntegration config = do
    nonStreaming <- pingNonStreaming config
    case nonStreaming of
        Left err -> pure (Left ("non-streaming: " <> err))
        Right () -> do
            streaming <- pingStreaming config
            case streaming of
                Left err -> pure (Left ("streaming: " <> err))
                Right () -> pure (Right ())

pingNonStreaming :: LlmProviderConfig -> IO (Either Text ())
pingNonStreaming config = do
    -- "Reply with exactly one word" keeps reasoning models terse; max_tokens
    -- must outlast a short reasoning preamble (Qwen3-class models think
    -- first, and a token-starved ping finishes with finish_reason=length and
    -- empty content on an otherwise healthy server).
    let payload =
            object
                [ "model" .= config.model
                , "messages" .= [messageJson (userMessage "Reply with exactly one word: pong")]
                , "max_tokens" .= (64 :: Int)
                ]
    result <- try (Http.postFollowing (testOpts config) (cs (apiUrl config "/v1/chat/completions")) payload)
    pure case result of
        Left (err :: SomeException) -> Left (tshow err)
        Right response ->
            let code = HttpTypes.statusCode (response ^. Wreq.responseStatus)
             in if code >= 200 && code < 300
                    then case decodeCompletion response of
                        Right completion
                            | not (Text.null completion.content) -> Right ()
                            | Just tokensOut <- completion.tokensOut
                            , tokensOut > 0 ->
                                Right ()
                            | otherwise -> Left "empty completion (no generated tokens)"
                        Left err -> Left (renderLlmError err)
                    else Left ("http " <> tshow code)

pingStreaming :: LlmProviderConfig -> IO (Either Text ())
pingStreaming config = do
    let payload =
            object
                [ "model" .= config.model
                , "messages" .= [messageJson (userMessage "Reply with exactly one word: pong")]
                , "max_tokens" .= (64 :: Int)
                , "stream" .= True
                ]
    result <- try (Http.postFollowing (testOpts config) (cs (apiUrl config "/v1/chat/completions")) payload)
    pure case result of
        Left (err :: SomeException) -> Left (tshow err)
        Right response ->
            let code = HttpTypes.statusCode (response ^. Wreq.responseStatus)
                body = cs (response ^. Wreq.responseBody) :: Text
             in if code >= 200 && code < 300
                    then case verifyStreamBody body of
                        Nothing -> Right ()
                        Just err -> Left err
                    else Left ("http " <> tshow code)

-- SSE sanity check on a buffered stream body: needs at least one
-- "data: {...}" chunk and the "[DONE]" terminator.
verifyStreamBody :: Text -> Maybe Text
verifyStreamBody body
    | not ("data:" `Text.isInfixOf` body) = Just "no SSE data chunks in the streaming response"
    | not ("[DONE]" `Text.isInfixOf` body) = Just "stream ended without a [DONE] terminator"
    | otherwise = Nothing

renderLlmError :: LlmError -> Text
renderLlmError (Retriable detail) = detail
renderLlmError (Terminal detail) = detail

-- Short-timeout variant of the request options for the admin tests.
testOpts :: LlmProviderConfig -> Wreq.Options
testOpts config =
    opts config
        & Wreq.manager
            .~ Left
                ( HTTP.tlsManagerSettings
                    { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro (15 * 1000000)
                    }
                )

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
            let code = HttpTypes.statusCode (response ^. Wreq.responseStatus)
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
