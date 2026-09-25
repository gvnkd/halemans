module Web.Controller.AgentChat where

import Application.Service.Agent.Core (AgentEvent (..), runAgentTurn, runAgentTurnStreaming)
import Application.Service.Live (broadcastAgentTurn)
import Application.Service.Llm (StreamStatus (..))
import qualified CMark
import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import qualified Control.Exception.Safe as Exception
import Data.Aeson (Value, object, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Builder (string8)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.Vector as Vector
import Generated.Types
import IHP.ModelSupport (withTransaction)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (queryString, responseLBS, responseStream)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import Web.Controller.Prelude

-- Agent chat (internal API milestone). Session-authed JSON endpoints backing
-- the floating chat widget: POST a message, optionally resume an existing
-- session; GET session list/history for the resume dropdown.

instance Controller AgentChatController where
    beforeAction = ensureIsUser

    action ChatAction = do
        body <- getRequestBody
        case Aeson.decode body >>= parseMaybe parseChatRequest of
            Nothing -> renderJsonWithStatusCode HTTP.status400 (object ["error" .= ("invalid request body" :: Text)])
            Just request
                | request.stream -> streamChat request
                | otherwise -> jsonChat request
    action AgentSessionsAction = do
        sessions <-
            query @AgentSession
                |> filterWhere (#userId, currentUserId)
                |> orderByDesc #updatedAt
                |> fetch
        renderJson (object ["sessions" .= [object ["id" .= get #id s, "title" .= s.title] | s <- sessions]])
    action AgentHistoryAction{sessionId} = do
        session <- fetchOr404 sessionId
        rows <-
            query @AgentMessage
                |> filterWhere (#sessionId, get #id session)
                |> orderByAsc #createdAt
                |> fetch
        renderJson (object ["messages" .= map encodeHistoryRow rows])

jsonChat :: (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext, CurrentUserRecord ~ User) => ChatRequest -> IO ResponseReceived
jsonChat request = do
    sessionResult <- resolveSession request
    case sessionResult of
        Left status -> renderJsonWithStatusCode status (object ["error" .= ("session not found" :: Text)])
        Right session -> do
            userMessageRow <- storeUserMessage request session
            turnResult <- runAgentTurn (get #id session)
            case turnResult of
                Left err -> renderJsonWithStatusCode status500Internal (object ["error" .= err])
                Right () -> do
                    replies <- loadTurnReplies session userMessageRow
                    notifyTurnDone session replies
                    renderJson (object ["session_id" .= get #id session, "replies" .= replies])

-- WS backstop: the reply is persisted at this point, so a browser that lost
-- the SSE done frame renders it immediately on this independent channel
-- instead of waiting for the stall timer's history poll. Must never throw:
-- this runs in the thread that emits the SSE done next.
notifyTurnDone :: (?modelContext :: ModelContext) => AgentSession -> [Value] -> IO ()
notifyTurnDone session replies =
    let Id userUuid = session.userId
     in broadcastAgentTurn userUuid (object ["session_id" .= get #id session, "replies" .= replies])
            `Exception.catch` \(_ :: Exception.SomeException) -> pure ()

-- Streaming variant (fetch + ReadableStream reader on the client): emits
-- SSE token/tool events while the turn runs, then a done event carrying the
-- same payload as the JSON endpoint.
streamChat :: (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext, CurrentUserRecord ~ User) => ChatRequest -> IO ResponseReceived
streamChat request = do
    sessionResult <- resolveSession request
    case sessionResult of
        Left status -> renderJsonWithStatusCode status (object ["error" .= ("session not found" :: Text)])
        Right session -> do
            userMessageRow <- storeUserMessage request session
            let sessionId = get #id session
            events <- newChan :: IO (Chan (Either Text AgentEvent))
            _ <- forkIO do
                _ <- runAgentTurnStreaming (\event -> writeChan events (Right event)) sessionId
                -- the reply is persisted now: notify WS subscribers before
                -- unblocking the SSE done frame, so a tab that lost the
                -- stream renders from this channel without waiting.
                replies <- loadTurnReplies session userMessageRow
                notifyTurnDone session replies
                writeChan events (Left "done")
            respondAndExit $
                responseStream
                    HTTP.status200
                    [ ("Content-Type", "text/event-stream; charset=utf-8")
                    , ("Cache-Control", "no-cache")
                    , ("X-Accel-Buffering", "no")
                    ]
                    \writeBuilder flush -> sendEvents writeBuilder flush events session userMessageRow
  where
    sendEvents writeBuilder flush events session userMessageRow =
        Exception.catch
            do
                -- session id first: the client needs it for stall-recovery even when
                -- the done frame (the only other carrier) never arrives.
                emit "session" (object ["session_id" .= get #id session])
                loop
            \err -> logSseException (err :: Exception.SomeException)
      where
        -- Heartbeat: idle links/proxies swallow quiet SSE tails; a comment
        -- frame every 15s keeps the pipe warm and lets the client tell
        -- "alive, working" from "dead pipe".
        loop = do
            item <- timeout (15 * 1000000) (readChan events)
            case item of
                Nothing -> do
                    writeBuilder (string8 ": hb\n\n")
                    flush
                    loop
                Just (Right (AgentToken status)) -> do
                    -- pre-rendered markdown of the accumulated answer: the
                    -- widget swaps it in per event so the final reply streams
                    -- in formatted (same renderer as the analysis card,
                    -- markdownHtml = renderMarkdownText, optSafe-sanitized).
                    emit "token" (object ["words" .= status.stWords, "elapsed_ms" .= status.stElapsedMs, "tool" .= status.stTool, "html" .= markdownText status.stContent])
                    loop
                Just (Right (AgentToolStart toolName)) -> do
                    emit "tool" (object ["name" .= toolName])
                    loop
                Just (Right (AgentRoundStart roundNumber)) -> do
                    emit "round" (object ["round" .= roundNumber])
                    loop
                Just (Left _) -> do
                    replies <- loadTurnReplies session userMessageRow
                    emit "done" (object ["session_id" .= get #id session, "replies" .= replies])
        emit :: Text -> Value -> IO ()
        emit event payload = do
            writeBuilder (string8 ("event: " <> cs event <> "\ndata: " <> cs (Aeson.encode payload) <> "\n\n"))
            flush

    -- The web layer has no FastLogger (Request carries no logger); stderr
    -- reaches the process-compose log. A silently-dying stream thread is
    -- exactly what the "stalled widget with a finished turn in the DB"
    -- symptom looks like, so this must be loud.
    logSseException :: Exception.SomeException -> IO ()
    logSseException err = hPutStrLn stderr (cs ("agent SSE stream died: " <> tshow err) :: String)

storeUserMessage :: (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext, CurrentUserRecord ~ User) => ChatRequest -> AgentSession -> IO AgentMessage
storeUserMessage request session = do
    let pageContext = request.pageContext
    withTransaction do
        when (isNothing session.pageContext && isJust pageContext) do
            _ <- session |> set #pageContext pageContext |> updateRecord
            pure ()
        newRecord @AgentMessage
            |> set #sessionId (get #id session)
            |> set #role_ ("user" :: Text)
            |> set #content request.message
            |> set #pageContext pageContext
            |> createRecord

loadTurnReplies :: (?modelContext :: ModelContext) => AgentSession -> AgentMessage -> IO [Value]
loadTurnReplies session userMessageRow = do
    rows <-
        query @AgentMessage
            |> filterWhere (#sessionId, get #id session)
            |> orderByAsc #createdAt
            |> fetch
    -- only the assistant rows AFTER the user message we just stored
    -- (createdAt ties within one second make position-based dropWhile
    -- the reliable cut)
    let replies = case dropWhile (\row -> get #id row /= get #id userMessageRow) rows of
            (_userRow : rest) -> [encodeReply row | row <- rest, row.role_ == "assistant"]
            [] -> []
    pure replies

status500Internal :: HTTP.Status
status500Internal = HTTP.mkStatus 500 "Internal Server Error"

data ChatRequest = ChatRequest
    { message :: Text
    , sessionId :: Maybe UUID
    , pageContext :: Maybe Value
    , stream :: Bool
    }

parseChatRequest :: Value -> Parser ChatRequest
parseChatRequest = Aeson.withObject "chat" \o -> do
    message <- o .: "message"
    sessionId <- o .:? "session_id"
    pageContext <- o .:? "page_context"
    stream <- o .:? "stream" .!= False
    pure ChatRequest{..}

-- Owned-session resolution: resume when the id is given and belongs to the
-- current user, otherwise start a fresh session titled from the message.
resolveSession :: (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext, CurrentUserRecord ~ User) => ChatRequest -> IO (Either HTTP.Status AgentSession)
resolveSession request = case request.sessionId of
    Nothing -> Right <$> createSession
    Just rawId -> do
        let sessionId = Id rawId :: Id AgentSession
        found <- fetchOneOrNothing sessionId
        case found of
            Just session | session.userId == currentUserId -> pure (Right session)
            _ -> pure (Left HTTP.status404)
  where
    createSession = do
        let title = Text.take 60 request.message
        newRecord @AgentSession
            |> set #userId currentUserId
            |> set #title (Just title)
            |> createRecord

fetchOr404 :: (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext, CurrentUserRecord ~ User) => Id AgentSession -> IO AgentSession
fetchOr404 sessionId = do
    session <- fetch sessionId
    when (session.userId /= currentUserId) do
        respondAndExit (responseLBS HTTP.status404 [("Content-Type", "application/json")] "{}")
    pure session

encodeReply :: AgentMessage -> Value
encodeReply row =
    object
        [ "content" .= row.content
        , "html" .= markdownText row.content
        , "tool_calls" .= row.toolCalls
        , "trace" .= row.trace
        , "needs_confirmation" .= needsConfirmation row
        ]

encodeHistoryRow :: AgentMessage -> Value
encodeHistoryRow row =
    object
        [ "role" .= row.role_
        , "content" .= row.content
        , "html" .= markdownText row.content
        , "tool_calls" .= row.toolCalls
        , "trace" .= row.trace
        , "created_at" .= row.createdAt
        , "needs_confirmation" .= needsConfirmation row
        ]

-- A two-phase mutating tool ran with confirmed=false: its persisted result
-- carries the "confirmation required" convention (Tools.hs). The widget
-- renders Apply/Discard buttons for the LAST such reply.
needsConfirmation :: AgentMessage -> Bool
needsConfirmation row = case row.toolCalls of
    Just (Aeson.Array items) -> any callNeedsConfirmation (Vector.toList items)
    _ -> False
  where
    callNeedsConfirmation value = case parseMaybe (Aeson.withObject "call" (\o -> o Aeson..: "result")) value of
        Just (result :: Text) -> "confirmation required" `Text.isInfixOf` result
        Nothing -> False

-- Same markdown pipeline as the analysis card (Application.Helper.View
-- markdownHtml); kept as Text here so JSON payloads can carry it.
markdownText :: Text -> Text
markdownText = CMark.commonmarkToHtml [CMark.optSafe]
