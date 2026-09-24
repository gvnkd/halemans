module Web.Controller.AgentChat where

import Application.Service.Agent.Core (runAgentTurn)
import Data.Aeson (Value, object, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Text as Text
import Data.UUID (UUID)
import Generated.Types
import IHP.ModelSupport (withTransaction)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (responseLBS)
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
            Just request -> do
                sessionResult <- resolveSession request
                case sessionResult of
                    Left status -> renderJsonWithStatusCode status (object ["error" .= ("session not found" :: Text)])
                    Right session -> do
                        let pageContext = request.pageContext
                        userMessageRow <-
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
                        turnResult <- runAgentTurn (get #id session)
                        case turnResult of
                            Left err -> renderJsonWithStatusCode status500Internal (object ["error" .= err])
                            Right () -> do
                                rows <-
                                    query @AgentMessage
                                        |> filterWhere (#sessionId, get #id session)
                                        |> orderByAsc #createdAt
                                        |> fetch
                                -- only the assistant rows AFTER the user
                                -- message we just stored (createdAt ties
                                -- within one second make id/position-based
                                -- dropWhile the reliable cut)
                                let replies = case dropWhile (\row -> get #id row /= get #id userMessageRow) rows of
                                        (_userRow : rest) -> [encodeReply row | row <- rest, row.role_ == "assistant"]
                                        [] -> []
                                renderJson (object ["session_id" .= get #id session, "replies" .= replies])
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

status500Internal :: HTTP.Status
status500Internal = HTTP.mkStatus 500 "Internal Server Error"

data ChatRequest = ChatRequest
    { message :: Text
    , sessionId :: Maybe UUID
    , pageContext :: Maybe Value
    }

parseChatRequest :: Value -> Parser ChatRequest
parseChatRequest = Aeson.withObject "chat" \o -> do
    message <- o .: "message"
    sessionId <- o .:? "session_id"
    pageContext <- o .:? "page_context"
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
        , "tool_calls" .= row.toolCalls
        ]

encodeHistoryRow :: AgentMessage -> Value
encodeHistoryRow row =
    object
        [ "role" .= row.role_
        , "content" .= row.content
        , "tool_calls" .= row.toolCalls
        , "created_at" .= row.createdAt
        ]
