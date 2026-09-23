module Application.Service.Agent.Core (
    runAgentTurn,
    maxToolRounds,
) where

import Application.Service.Agent.Tools (AgentContext (..), agentToolDefinitions, executeAgentTool)
import Application.Service.I18n (agentLanguageName)
import Application.Service.Llm (Completion (..), LlmError (..), LlmMessage (..), LlmProvider (..), LlmProviderConfig (..), OpenAiCompat (..), Prompt (..), ToolCall (..), toolResultMessage, userMessage)
import qualified Application.Service.Llm.Budget as Budget
import Application.Service.Llm.DbConfig (currentLlmConfig)
import Control.Monad (void)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Int (Int64)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Generated.Types
import IHP.Fetch (fetch)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, orderByAsc, query)
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)

-- Agent turn driver (internal API milestone). Rebuilds the OpenAI message
-- list from agent_messages, runs the tool-call loop against the configured
-- provider (DB-first, admin/llm page), and persists every iteration so the
-- web chat UI can render the full exchange and later turns resume from
-- durable state. Tool-call/result pairing survives the round trip via
-- deterministic call ids ("<message index>-<call index>") regenerated from
-- the persisted tool_calls JSON.

maxToolRounds :: Int
maxToolRounds = 6

runAgentTurn :: (?modelContext :: ModelContext) => Id AgentSession -> IO (Either Text ())
runAgentTurn sessionId = do
    session <- fetch sessionId
    user <- fetch session.userId
    language <- agentLanguageName (userLanguageCode user)
    let context =
            AgentContext
                { acUser = user
                , acLanguage = language
                }
    config <- currentLlmConfig
    case config of
        Nothing -> persistSimple sessionId Nothing "The LLM provider is not configured (admin/llm). Ask an administrator to enable one." >> pure (Right ())
        Just config -> do
            overBudget <- checkBudget config.providerName
            if overBudget
                then persistSimple sessionId Nothing "The daily LLM token budget is exhausted; try again tomorrow or ask an administrator to raise it." >> pure (Right ())
                else do
                    history <- loadHistory sessionId
                    let messages = systemPrompt context session.pageContext : history
                    loop context config sessionId messages 0
  where
    loop context config sessionId messages roundsLeft = do
        result <- complete (OpenAiCompat config) (Prompt messages agentToolDefinitions)
        case result of
            Left err -> do
                let text = case err of
                        Retriable detail -> "The LLM provider request failed (retriable): " <> detail
                        Terminal detail -> "The LLM provider request failed: " <> detail
                persistSimple sessionId Nothing text
                pure (Right ())
            Right completion -> do
                recordUsage config.providerName completion
                case completion.toolCalls of
                    [] -> do
                        persistSimple sessionId (Just completion) completion.content
                        pure (Right ())
                    calls
                        | roundsLeft <= 0 -> do
                            persistToolRound sessionId completion Nothing calls []
                            persistSimple sessionId Nothing "I ran out of tool-call rounds for this turn; please narrow the request."
                            pure (Right ())
                        | otherwise -> do
                            outputs <- forM calls \call -> executeAgentTool context call
                            persistToolRound sessionId completion Nothing calls outputs
                            let results = [toolResultMessage call.callId output | (call, output) <- zip calls outputs]
                                messages' = messages ++ [assistantRound completion calls] ++ results
                            loop context config sessionId messages' (roundsLeft - 1)

-- | System prompt: identity, act-as identity, language, tool policy.
systemPrompt :: AgentContext -> Maybe Value -> LlmMessage
systemPrompt context pageContext =
    LlmMessage
        { role = "system"
        , content =
            Text.intercalate
                "\n"
                [ "You are the Halemans agent, an embedded operations assistant for the Halemans alerting platform."
                , "You act on behalf of the user " <> context.acUser.displayName <> " (" <> context.acUser.email <> "). You can only do what that user's privileges allow; when a tool reports a permission problem, explain it and stop pushing."
                , "Respond in " <> context.acLanguage <> "."
                , "Rules:"
                , "- Use tools to ground every factual claim about alerts, environments and dashboards; never invent ids, names or counts."
                , "- Mutating tools follow a strict two-phase flow: first call the tool with confirmed=false (or validate_*), present the returned plan to the user, and call with confirmed=true only after the user's explicit agreement in the conversation."
                , "- Answer concisely in markdown. Ask a clarifying question instead of guessing ambiguous names."
                , "- The dashboard match operators are =, !=, ~ (glob with * and ?), in and not-in."
                , pageContextLine
                ]
        , toolCallId = Nothing
        , msgToolCalls = []
        }
  where
    pageContextLine = case pageContext of
        Just value -> "The user is currently looking at this page: " <> cs (Aeson.encode value)
        Nothing -> "No page context is available for this conversation."

-- | Language code from users.settings.language (NULL when unset).
userLanguageCode :: User -> Maybe Text
userLanguageCode user =
    fromMaybe
        Nothing
        (parseMaybe (Aeson.withObject "settings" (\o -> o Aeson..:? "language")) user.settings)

-- History rebuild. Assistant rows with tool_calls regenerate the assistant
-- message plus one tool result message per persisted call, with ids matching
-- persistToolRound's numbering.
loadHistory :: (?modelContext :: ModelContext) => Id AgentSession -> IO [LlmMessage]
loadHistory sessionId = do
    rows <-
        query @AgentMessage
            |> filterWhere (#sessionId, sessionId)
            |> orderByAsc #createdAt
            |> fetch
    pure (concatMap rowMessages (zip [0 ..] rows))
  where
    rowMessages (index, row) = case row.role_ of
        "user" -> [userMessage row.content]
        "assistant"
            | Just callsValue <- row.toolCalls
            , Just calls <- parseMaybe parsePersistedCalls callsValue ->
                assistantRoundFrom index calls
                    : [toolResultMessage (callIdFor index callIndex) result | (callIndex, result) <- zip [0 ..] (map persistedResult calls)]
        "assistant" -> [LlmMessage{role = "assistant", content = row.content, toolCallId = Nothing, msgToolCalls = []}]
        _ -> []
    assistantRoundFrom index calls =
        LlmMessage
            { role = "assistant"
            , content = ""
            , toolCallId = Nothing
            , msgToolCalls = [ToolCall (callIdFor index callIndex) call.persistedName call.persistedArguments | (callIndex, call) <- zip [0 ..] calls]
            }
    callIdFor messageIndex callIndex = "m" <> tshow (messageIndex :: Int) <> "-" <> tshow (callIndex :: Int)

data PersistedCall = PersistedCall
    { persistedName :: Text
    , persistedArguments :: Text
    , persistedResult :: Text
    }

parsePersistedCalls :: Value -> Parser [PersistedCall]
parsePersistedCalls = Aeson.withArray "tool_calls" \items ->
    mapM
        ( Aeson.withObject "tool_call" \o ->
            PersistedCall
                <$> o Aeson..: "name"
                <*> o Aeson..: "arguments"
                <*> o Aeson..: "result"
        )
        (Vector.toList items)

assistantRound :: Completion -> [ToolCall] -> LlmMessage
assistantRound completion calls =
    LlmMessage{role = "assistant", content = completion.content, toolCallId = Nothing, msgToolCalls = calls}

-- Persist one assistant iteration. With tool calls: content may be empty, the
-- tool_calls column carries name/arguments/result for replay and display.
persistToolRound :: (?modelContext :: ModelContext) => Id AgentSession -> Completion -> Maybe Value -> [ToolCall] -> [Text] -> IO ()
persistToolRound sessionId completion pageContext calls outputs = do
    let logValue =
            Aeson.toJSON
                [ object
                    [ "name" .= call.callName
                    , "arguments" .= call.callArguments
                    , "result" .= output
                    ]
                | (call, output) <- zip calls outputs
                ]
    void do
        newRecord @AgentMessage
            |> set #sessionId sessionId
            |> set #role_ ("assistant" :: Text)
            |> set #content completion.content
            |> set #toolCalls (Just logValue)
            |> set #pageContext pageContext
            |> set #promptTokens completion.tokensIn
            |> set #completionTokens completion.tokensOut
            |> createRecord

persistSimple :: (?modelContext :: ModelContext) => Id AgentSession -> Maybe Completion -> Text -> IO ()
persistSimple sessionId completion text = do
    void do
        newRecord @AgentMessage
            |> set #sessionId sessionId
            |> set #role_ ("assistant" :: Text)
            |> set #content text
            |> set #promptTokens (completion >>= (.tokensIn))
            |> set #completionTokens (completion >>= (.tokensOut))
            |> createRecord

checkBudget :: (?modelContext :: ModelContext) => Text -> IO Bool
checkBudget provider = do
    cap <- Budget.dailyTokenBudget
    rows <-
        sqlQueryTyped
            [typedSql|
        SELECT tokens_in, tokens_out FROM llm_budget_counters
        WHERE provider = ${provider} AND day = CURRENT_DATE
    |]
    pure case rows of
        [] -> False
        (row : _) -> Budget.budgetExceeded cap (fromIntegral (get #tokens_in row)) (fromIntegral (get #tokens_out row))

recordUsage :: (?modelContext :: ModelContext) => Text -> Completion -> IO ()
recordUsage provider completion = do
    let tokensIn = fromIntegral (fromMaybe 0 completion.tokensIn) :: Int64
        tokensOut = fromIntegral (fromMaybe 0 completion.tokensOut) :: Int64
    void do
        sqlExecTyped
            [typedSql|
            INSERT INTO llm_budget_counters (provider, day, tokens_in, tokens_out, requests)
            VALUES (${provider}, CURRENT_DATE, ${tokensIn}, ${tokensOut}, 1)
            ON CONFLICT (provider, day) DO UPDATE SET
                tokens_in = llm_budget_counters.tokens_in + EXCLUDED.tokens_in,
                tokens_out = llm_budget_counters.tokens_out + EXCLUDED.tokens_out,
                requests = llm_budget_counters.requests + 1
        |]
