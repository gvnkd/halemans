module Application.Service.Agent.Core (
    runAgentTurn,
    runAgentTurnWith,
    runAgentTurnStreaming,
    AgentEvent (..),
    buildSystemMessage,
    agentTurnGate,
    internalAgentTemplateName,
    defaultAgentTemplateBody,
    maxToolRounds,
) where

import Application.Helper.Controller (userPrivileges)
import Application.Service.Agent.Tools (AgentContext (..), agentToolDefinitionsFor, executeAgentTool)
import Application.Service.Api.RateLimit (checkLimit)
import Application.Service.I18n (agentLanguageName)
import Application.Service.Llm (Completion (..), LlmError (..), LlmMessage (..), LlmProvider (..), LlmProviderConfig (..), OpenAiCompat (..), Prompt (..), StreamStatus (..), ToolCall (..), chatCompletionStreaming, toolResultMessage, userMessage)
import Application.Service.Llm.AgentConfig (AgentBudgetConfig (..), agentBudgetConfig)
import qualified Application.Service.Llm.Budget as Budget
import Application.Service.Llm.DbConfig (currentLlmConfig)
import Application.Service.Llm.GlobalConfig (GlobalBudgetConfig (..), globalBudgetConfig)
import Application.Service.Llm.Prompt (renderTemplate)
import Control.Monad (join, void)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, limit, orderByAsc, orderByDesc, query)
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)

-- Agent turn driver (internal API milestone). Rebuilds the OpenAI message
-- list from agent_messages, runs the tool-call loop against the configured
-- provider (DB-first, admin/llm page), and persists every iteration so the
-- web chat UI can render the full exchange and later turns resume from
-- durable state. Tool-call/result pairing survives the round trip via
-- deterministic call ids ("<message index>-<call index>") regenerated from
-- the persisted tool_calls JSON.

maxToolRounds :: Int
maxToolRounds = 10

-- Live progress events for the streaming chat endpoint: token deltas of the
-- final answer (with word count + elapsed) and tool-round starts.
data AgentEvent
    = AgentToken StreamStatus
    | AgentToolStart Text
    deriving (Eq, Show)

runAgentTurn :: (?modelContext :: ModelContext) => Id AgentSession -> IO (Either Text ())
runAgentTurn sessionId = do
    config <- currentLlmConfig
    case config of
        Nothing -> persistSimple sessionId Nothing "The LLM provider is not configured (admin/llm). Ask an administrator to enable one." >> pure (Right ())
        Just config -> do
            agentConfig <- agentBudgetConfig
            globalConfig <- globalBudgetConfig
            session <- fetch sessionId
            gate <- agentTurnGate config.providerName agentConfig globalConfig
            overRate <- checkLimit ("agent:" <> tshow session.userId) agentConfig.abcRatePerMinute
            case (gate, overRate) of
                (Just "agent", _) -> persistSimple sessionId Nothing "The agent's daily LLM token budget is exhausted (admin/LLM → Agent configuration); try again tomorrow or ask an administrator to raise it." >> pure (Right ())
                (Just _, _) -> persistSimple sessionId Nothing "The global daily LLM token budget is exhausted (admin/LLM → Agent configuration → Global limits); try again tomorrow or ask an administrator to raise it." >> pure (Right ())
                (Nothing, Just _) -> persistSimple sessionId Nothing "The agent is rate-limited right now; wait a minute and retry." >> pure (Right ())
                (Nothing, Nothing) -> runAgentTurnInternal config.providerName (complete (OpenAiCompat config)) (Just (\prompt onStatus -> chatCompletionStreaming config prompt onStatus)) (const (pure ())) sessionId

-- Streaming variant for the chat endpoint: same gates, but emits live
-- events (final-answer token deltas, tool starts) via the callback.
runAgentTurnStreaming :: (?modelContext :: ModelContext) => (AgentEvent -> IO ()) -> Id AgentSession -> IO (Either Text ())
runAgentTurnStreaming onEvent sessionId = do
    config <- currentLlmConfig
    case config of
        Nothing -> persistSimple sessionId Nothing "The LLM provider is not configured (admin/llm). Ask an administrator to enable one." >> pure (Right ())
        Just config -> do
            agentConfig <- agentBudgetConfig
            globalConfig <- globalBudgetConfig
            session <- fetch sessionId
            gate <- agentTurnGate config.providerName agentConfig globalConfig
            overRate <- checkLimit ("agent:" <> tshow session.userId) agentConfig.abcRatePerMinute
            case (gate, overRate) of
                (Just "agent", _) -> persistSimple sessionId Nothing "The agent's daily LLM token budget is exhausted (admin/LLM → Agent configuration); try again tomorrow or ask an administrator to raise it." >> pure (Right ())
                (Just _, _) -> persistSimple sessionId Nothing "The global daily LLM token budget is exhausted (admin/LLM → Agent configuration → Global limits); try again tomorrow or ask an administrator to raise it." >> pure (Right ())
                (Nothing, Just _) -> persistSimple sessionId Nothing "The agent is rate-limited right now; wait a minute and retry." >> pure (Right ())
                (Nothing, Nothing) -> runAgentTurnInternal config.providerName (complete (OpenAiCompat config)) (Just (\prompt onStatus -> chatCompletionStreaming config prompt onStatus)) onEvent sessionId

-- Budget gate for one agent turn: the agent's own scope cap first, then the
-- global cap across ALL LLM consumers. Just reason = blocked.
agentTurnGate ::
    (?modelContext :: ModelContext) =>
    Text ->
    AgentBudgetConfig ->
    GlobalBudgetConfig ->
    IO (Maybe Text)
agentTurnGate provider agentConfig globalConfig = do
    agentRows <-
        sqlQueryTyped
            [typedSql|
        SELECT tokens_in, tokens_out FROM llm_budget_counters
        WHERE scope = 'agent' AND provider = ${provider} AND day = CURRENT_DATE
    |]
    let agentSpent = sum [get #tokens_in row + get #tokens_out row | row <- agentRows]
    globalRows <-
        sqlQueryTyped
            [typedSql|
        SELECT tokens_in, tokens_out FROM llm_budget_counters
        WHERE day = CURRENT_DATE
    |]
    let globalSpent = sum [get #tokens_in row + get #tokens_out row | row <- globalRows]
    pure
        ( if Budget.budgetExceeded agentConfig.abcDailyTokenBudget (fromIntegral agentSpent) 0
            then Just "agent"
            else
                if Budget.budgetExceeded globalConfig.gbcDailyTokenBudget (fromIntegral globalSpent) 0
                    then Just "global"
                    else Nothing
        )

-- The turn driver with an injected completion source (tests script a fake
-- provider; runAgentTurn passes the configured OpenAI-compatible client).
runAgentTurnWith ::
    (?modelContext :: ModelContext) =>
    Text ->
    (Prompt -> IO (Either LlmError Completion)) ->
    Id AgentSession ->
    IO (Either Text ())
runAgentTurnWith providerName completionSource sessionId =
    runAgentTurnInternal providerName completionSource Nothing (const (pure ())) sessionId

runAgentTurnInternal ::
    (?modelContext :: ModelContext) =>
    Text ->
    (Prompt -> IO (Either LlmError Completion)) ->
    Maybe (Prompt -> (StreamStatus -> IO ()) -> IO (Either LlmError Completion)) ->
    (AgentEvent -> IO ()) ->
    Id AgentSession ->
    IO (Either Text ())
runAgentTurnInternal providerName completionSource mStreaming onEvent sessionId = do
    session <- fetch sessionId
    user <- fetch session.userId
    language <- agentLanguageName (userLanguageCode user)
    let context =
            AgentContext
                { acUser = user
                , acLanguage = language
                }
    history <- loadHistory sessionId
    -- Page context for the system prompt comes from the LATEST user message
    -- (each message carries what the widget saw when sent), falling back to
    -- the session's creation-time context — a long-lived conversation must
    -- not be pinned to the page where it started.
    latestUserRows <-
        query @AgentMessage
            |> filterWhere (#sessionId, sessionId)
            |> filterWhere (#role_, "user" :: Text)
            |> orderByDesc #createdAt
            |> limit 1
            |> fetch
    let livePageContext = case latestUserRows of
            (row : _) | isJust row.pageContext -> row.pageContext
            _ -> session.pageContext
    sysMsg <- buildSystemMessage context livePageContext
    privileges <- userPrivileges (get #id user)
    let tools = agentToolDefinitionsFor privileges
        messages = sysMsg : history
    loop context sessionId messages tools maxToolRounds Map.empty
  where
    loop context sessionId messages tools roundsLeft seen = do
        -- Streaming is used for every round when a callback is present:
        -- completions that turn out to carry tool calls are accumulated
        -- chunk-wise (chatCompletionStreaming reassembles them) and the
        -- loop proceeds exactly like the buffered path; token deltas give
        -- the UI live progress during the model's (long) thinking.
        let completeThis prompt = case mStreaming of
                Just streamFn -> streamFn prompt (onEvent . AgentToken)
                Nothing -> completionSource prompt
        result <- completeThis (Prompt messages tools)
        case result of
            Left err -> do
                let text = case err of
                        Retriable detail -> "The LLM provider request failed (retriable): " <> detail
                        Terminal detail -> "The LLM provider request failed: " <> detail
                persistSimple sessionId Nothing text
                pure (Right ())
            Right completion -> do
                recordUsage providerName completion
                case completion.toolCalls of
                    [] -> do
                        persistSimple sessionId (Just completion) completion.content
                        pure (Right ())
                    calls
                        | roundsLeft <= 0 -> do
                            -- Budget spent: one last completion WITHOUT tools
                            -- so the model answers from data already
                            -- collected. Only if it still demands tool calls
                            -- do we apologize to the user.
                            persistToolRound sessionId completion Nothing calls []
                            final <- completionSource (Prompt (messages ++ [finalAnswerMessage]) [])
                            case final of
                                Right finalCompletion
                                    | null finalCompletion.toolCalls -> do
                                        recordUsage providerName finalCompletion
                                        persistSimple sessionId (Just finalCompletion) finalCompletion.content
                                    | otherwise -> do
                                        recordUsage providerName finalCompletion
                                        persistSimple sessionId Nothing "I could not complete this request within the tool-call budget; please narrow the request."
                                Left _ -> persistSimple sessionId Nothing "I could not complete this request within the tool-call budget; please narrow the request."
                            pure (Right ())
                        | otherwise -> do
                            forM_ calls \call -> onEvent (AgentToolStart call.callName)
                            (outputs, seen') <- runCalls context seen calls
                            persistToolRound sessionId completion Nothing calls outputs
                            let results = [toolResultMessage call.callId output | (call, output) <- zip calls outputs]
                                messages' = messages ++ [assistantRound completion calls] ++ results
                            loop context sessionId messages' tools (roundsLeft - 1) seen'

    -- Execute one iteration's tool calls with a per-turn repetition guard:
    -- a call whose (name, arguments) already ran this turn gets its cached
    -- result back with an explicit "do not repeat" note, instead of hitting
    -- the service again and feeding the model the same text (the classic
    -- thrash loop that burned all rounds without answering).
    runCalls context seen calls = go calls seen []
      where
        go [] seenAcc outputs = pure (reverse outputs, seenAcc)
        go (call : rest) seenAcc outputs = case Map.lookup (callKey call) seenAcc of
            Just previous -> go rest seenAcc (repeatNote call previous : outputs)
            Nothing -> do
                output <- executeAgentTool context call
                go rest (Map.insert (callKey call) output seenAcc) (output : outputs)
    callKey call = call.callName <> "\0" <> call.callArguments
    repeatNote call previous =
        "NOTE: you already called the tool \""
            <> call.callName
            <> "\" with these exact arguments earlier in this turn. Its result was:\n"
            <> previous
            <> "\nDo not call it again; answer the user using the data you already have."

-- Last-ditch instruction when the tool budget is spent: forces an answer
-- from collected data instead of another tool request.
finalAnswerMessage :: LlmMessage
finalAnswerMessage =
    LlmMessage
        { role = "user"
        , content = "System: the tool-call budget for this turn is exhausted. Answer the user's request NOW using only the data already collected in this conversation. Do not request any more tool calls."
        , toolCallId = Nothing
        , msgToolCalls = []
        }

-- | Template name of the internal chat agent's system prompt in
-- llm_prompt_templates (versioned like alert_enrichment; edited in the
-- template editor, seeded by nix/scripts/seed-halemans.sh).
internalAgentTemplateName :: Text
internalAgentTemplateName = "internal_agent"

-- | System message for one turn: the ACTIVE internal_agent template rendered
-- with the per-turn bindings when seeded, else the built-in default — the
-- agent keeps working on fresh installs before the seed runs.
buildSystemMessage :: (?modelContext :: ModelContext) => AgentContext -> Maybe Value -> IO LlmMessage
buildSystemMessage context pageContext = do
    template <-
        query @LlmPromptTemplate
            |> filterWhere (#name, internalAgentTemplateName)
            |> filterWhere (#active, True)
            |> fetchOneOrNothing
    let content = case template of
            Just template -> renderTemplate template.body bindings
            Nothing -> defaultSystemPrompt context pageContext
    pure
        LlmMessage
            { role = "system"
            , content
            , toolCallId = Nothing
            , msgToolCalls = []
            }
  where
    bindings =
        [ ("user_name", context.acUser.displayName)
        , ("user_email", context.acUser.email)
        , ("language", context.acLanguage)
        , ("page_context", pageContextText pageContext)
        , ("current_page_url", pageUrl pageContext)
        , ("current_page_title", pageTitle pageContext)
        ]

pageUrl :: Maybe Value -> Text
pageUrl pageContext = fromMaybe "" do
    value <- pageContext
    join (parseMaybe parseUrl value)
  where
    -- New widget payloads use "url" (path + query); older sessions have
    -- "path" only — accept both so history keeps working.
    parseUrl = Aeson.withObject "page_context" \o -> do
        url <- o Aeson..:? "url"
        path <- o Aeson..:? "path"
        pure (url <|> path)

pageTitle :: Maybe Value -> Text
pageTitle pageContext = fromMaybe "" do
    value <- pageContext
    join (parseMaybe (Aeson.withObject "page_context" (\o -> o Aeson..:? "title")) value)

pageContextText :: Maybe Value -> Text
pageContextText pageContext = case pageContext of
    Just value -> "The user is currently looking at this page: " <> cs (Aeson.encode value)
    Nothing -> "No page context is available for this conversation."

-- | Seed body for the internal_agent template (admin "seed from default"
-- button and nix/scripts/seed-halemans.sh). Slots: {{user_name}},
-- {{user_email}}, {{language}}, {{current_page_url}}, {{current_page_title}}.
defaultAgentTemplateBody :: Text
defaultAgentTemplateBody =
    Text.intercalate
        "\n"
        [ "You are the Halemans agent, an embedded operations assistant for the Halemans alerting platform."
        , "You act on behalf of the user {{user_name}} ({{user_email}}). You can only do what that user's privileges allow; when a tool reports a permission problem, explain it and stop pushing."
        , "Respond in {{language}}."
        , ""
        , "Rules:"
        , "- Use tools to ground every factual claim about alerts, environments and dashboards; never invent ids, names or counts."
        , "- At most 10 tool-call rounds per turn: prefer ONE well-filtered call over repeated probing, and answer as soon as you have the data. Never repeat a call with identical arguments."
        , "- Every tool enforces the user's real privileges server-side; when a tool reports a permission problem, explain which privilege is missing and stop pushing — do not retry or work around it."
        , "- Mutating tools follow a strict two-phase flow: first call the tool with confirmed=false (or validate_*), present the returned plan to the user, and call with confirmed=true only after the user's explicit agreement in the conversation."
        , "- Answer concisely in markdown. Ask a clarifying question instead of guessing ambiguous names."
        , "- The dashboard match operators are =, !=, ~ (glob with * and ?), in and not-in."
        , ""
        , "The user is currently looking at: {{current_page_title}} ({{current_page_url}})"
        ]

-- | Built-in fallback system prompt (identity, act-as identity, language,
-- tool policy) used when no active internal_agent template exists.
defaultSystemPrompt :: AgentContext -> Maybe Value -> Text
defaultSystemPrompt context pageContext =
    renderTemplate
        defaultAgentTemplateBody
        [ ("user_name", context.acUser.displayName)
        , ("user_email", context.acUser.email)
        , ("language", context.acLanguage)
        , ("page_context", pageContextText pageContext)
        ]

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

recordUsage :: (?modelContext :: ModelContext) => Text -> Completion -> IO ()
recordUsage provider completion = do
    let tokensIn = fromIntegral (fromMaybe 0 completion.tokensIn) :: Int64
        tokensOut = fromIntegral (fromMaybe 0 completion.tokensOut) :: Int64
    void do
        sqlExecTyped
            [typedSql|
            INSERT INTO llm_budget_counters (scope, provider, day, tokens_in, tokens_out, requests)
            VALUES ('agent', ${provider}, CURRENT_DATE, ${tokensIn}, ${tokensOut}, 1)
            ON CONFLICT (scope, provider, day) DO UPDATE SET
                tokens_in = llm_budget_counters.tokens_in + EXCLUDED.tokens_in,
                tokens_out = llm_budget_counters.tokens_out + EXCLUDED.tokens_out,
                requests = llm_budget_counters.requests + 1
        |]
