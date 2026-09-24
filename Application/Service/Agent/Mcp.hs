module Application.Service.Agent.Mcp (
    mcpStdioServer,
    McpConfig (..),
    handleMessage,
    resolveMcpUser,
    defaultMcpEmail,
    defaultMcpRoleName,
) where

import Application.Helper.Controller (allPrivileges, userPrivileges)
import Application.Service.Agent.Tools (AgentContext (..), agentToolDefinitionsFor, executeAgentTool)
import Application.Service.I18n (agentLanguageName)
import qualified Application.Service.Llm as Llm
import Data.Aeson (Value (..), object, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import Generated.Types
import IHP.Fetch (fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, query)
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)

-- Minimal MCP server (protocol 2025-03-26) over newline-delimited JSON-RPC
-- on stdio. Exposes the agent tool registry to a locally spawned LLM agent;
-- the agent is trusted (same trust domain as the web process), so transport
-- auth is the environment. This module is pure protocol plumbing — all
-- semantics live in Application.Service.Agent.Tools.
--
-- Act-as identity (resolveMcpUser):
--   * HALEMANS_MCP_USER names an existing user explicitly.
--   * Otherwise the server acts as the DEFAULT SERVICE USER mcp@localhost,
--     auto-created on first boot together with its dedicated "mcp" role.
--     The role starts with the minimal "view" privilege; an administrator
--     widens it in the roles UI. HALEMANS_MCP_PRIVILEGES (comma-separated)
--     pins the role's privileges from env instead — when set, the env wins
--     on every boot and overrides UI edits.

data McpConfig = McpConfig
    { mcpUser :: User
    , mcpLanguage :: Text
    , mcpPrivileges :: [Text]
    -- ^ Real RBAC surface: tools/list only advertises tools these
    -- privileges allow (the executor re-checks on every call).
    }

defaultMcpEmail :: Text
defaultMcpEmail = "mcp@localhost"

defaultMcpRoleName :: Text
defaultMcpRoleName = "mcp"

defaultMcpPrivileges :: [Text]
defaultMcpPrivileges = ["view"]

mcpStdioServer :: (?modelContext :: ModelContext) => IO ()
mcpStdioServer = do
    user <- resolveMcpUser
    language <- agentLanguageName (userLanguageCode user)
    privileges <- userPrivileges (get #id user)
    serveLoop McpConfig{mcpUser = user, mcpLanguage = language, mcpPrivileges = privileges}

resolveMcpUser :: (?modelContext :: ModelContext) => IO User
resolveMcpUser = do
    maybeEmail <- lookupEnv "HALEMANS_MCP_USER"
    case maybeEmail of
        Just email -> do
            found <- query @User |> filterWhere (#email, cs email) |> fetchOneOrNothing
            maybe (fail ("HALEMANS_MCP_USER not found: " <> email)) pure found
        Nothing -> ensureDefaultMcpUser

-- | The default service user + its dedicated role, created idempotently at
-- first boot. The login is unusable ("!" password hash): the account exists
-- purely as an act-as identity for tools.
ensureDefaultMcpUser :: (?modelContext :: ModelContext) => IO User
ensureDefaultMcpUser = do
    role <- ensureMcpRole
    user <-
        query @User
            |> filterWhere (#email, defaultMcpEmail)
            |> fetchOneOrNothing
            >>= \case
                Just existing -> pure existing
                Nothing ->
                    newRecord @User
                        |> set #email defaultMcpEmail
                        |> set #displayName "MCP Agent"
                        |> set #passwordHash "!"
                        |> createRecord
    binding <-
        query @UserRole
            |> filterWhere (#userId, get #id user)
            |> filterWhere (#roleId, get #id role)
            |> fetchOneOrNothing
    when (isNothing binding) do
        _ <-
            newRecord @UserRole
                |> set #userId (get #id user)
                |> set #roleId (get #id role)
                |> createRecord
        pure ()
    pure user

-- | The dedicated "mcp" role. HALEMANS_MCP_PRIVILEGES set: parsed and synced
-- onto the role on every boot (env is the source of truth). Unset: created
-- with ["view"] when missing, otherwise left untouched so administrator
-- edits in the roles UI persist.
ensureMcpRole :: (?modelContext :: ModelContext) => IO Role
ensureMcpRole = do
    envPrivileges <- lookupEnv "HALEMANS_MCP_PRIVILEGES"
    existing <- query @Role |> filterWhere (#name, defaultMcpRoleName) |> fetchOneOrNothing
    case envPrivileges of
        Just raw -> do
            let privileges = parseMcpPrivileges raw
            case existing of
                Just role | role.privileges == privileges -> pure role
                Just role -> role |> set #privileges privileges |> updateRecord
                Nothing -> newRecord @Role |> set #name defaultMcpRoleName |> set #privileges privileges |> createRecord
        Nothing -> case existing of
            Just role -> pure role
            Nothing -> newRecord @Role |> set #name defaultMcpRoleName |> set #privileges defaultMcpPrivileges |> createRecord

parseMcpPrivileges :: String -> [Text]
parseMcpPrivileges raw =
    let privileges = map (Text.strip . cs) (Text.splitOn "," (cs raw))
        unknown = [privilege | privilege <- privileges, privilege `notElem` allPrivileges]
     in if null privileges || not (null unknown)
            then error ("HALEMANS_MCP_PRIVILEGES: unknown or empty privileges " <> show unknown <> " (valid: " <> show allPrivileges <> ")")
            else privileges

userLanguageCode :: User -> Maybe Text
userLanguageCode user =
    fromMaybe
        Nothing
        (parseMaybe (Aeson.withObject "settings" (\o -> o Aeson..:? "language")) user.settings)

serveLoop :: (?modelContext :: ModelContext) => McpConfig -> IO ()
serveLoop config = do
    line <- ByteString.getLine
    case Aeson.eitherDecode (cs line) of
        Left _ -> respond (rpcError Null (-32700) "parse error")
        Right message -> do
            response <- handleMessage config message
            forM_ response respond
    serveLoop config
  where
    respond value = do
        ByteString.putStrLn (cs (Aeson.encode value))
        hFlush stdout

-- Returns Nothing for notifications (no response is sent).
handleMessage :: (?modelContext :: ModelContext) => McpConfig -> Value -> IO (Maybe Value)
handleMessage config message = case message of
    Object _
        | Just method <- textField "method" message ->
            case (method, lookupKey "id" message) of
                ("initialize", Just requestId) ->
                    pure
                        ( Just
                            ( rpcResult
                                requestId
                                ( object
                                    [ "protocolVersion" .= ("2025-03-26" :: Text)
                                    , "capabilities" .= object ["tools" .= object []]
                                    , "serverInfo" .= object ["name" .= ("halemans" :: Text), "version" .= ("mcp-1" :: Text)]
                                    ]
                                )
                            )
                        )
                ("ping", Just requestId) -> pure (Just (rpcResult requestId (object [])))
                ("tools/list", Just requestId) -> pure (Just (rpcResult requestId (object ["tools" .= map toMcpTool (agentToolDefinitionsFor config.mcpPrivileges)])))
                ("tools/call", Just requestId) -> do
                    let params = lookupKey "params" message
                        name = params >>= lookupKeyAsText "name"
                        arguments = params >>= lookupKey "arguments"
                    result <- case (name, arguments) of
                        (Just name, Just argsValue) -> do
                            let argsText = cs (Aeson.encode argsValue)
                            output <- executeAgentTool (toContext config) (Llm.ToolCall name name argsText)
                            pure
                                ( object
                                    [ "content" .= [object ["type" .= ("text" :: Text), "text" .= output]]
                                    , "isError" .= isToolError output
                                    ]
                                )
                        _ -> pure (rpcError requestId (-32602) "invalid params")
                    pure (Just (rpcResult requestId result))
                -- notifications/initialized and anything else without an id:
                -- acknowledged silently.
                (_, Nothing) -> pure Nothing
                (_, Just requestId) -> pure (Just (rpcError requestId (-32601) "method not found"))
    _ -> pure (Just (rpcError Null (-32600) "invalid request"))
  where
    toContext config' =
        AgentContext{acUser = config'.mcpUser, acLanguage = config'.mcpLanguage}

isToolError :: Text -> Bool
isToolError output = any (`Text.isPrefixOf` output) ["unknown tool", "invalid arguments", "forbidden"]

toMcpTool :: Value -> Value
toMcpTool definition =
    object
        [ "name" .= functionField "name"
        , "description" .= functionField "description"
        , "inputSchema" .= functionField "parameters"
        ]
  where
    functionField key = do
        inner <- lookupKey "function" definition
        lookupKey key inner

lookupKey :: Text -> Value -> Maybe Value
lookupKey key (Object obj) = KeyMap.lookup (Key.fromText key) obj
lookupKey _ _ = Nothing

lookupKeyAsText :: Text -> Value -> Maybe Text
lookupKeyAsText key value = case lookupKey key value of
    Just (String text) -> Just text
    _ -> Nothing

textField :: Text -> Value -> Maybe Text
textField = lookupKeyAsText

rpcResult :: Value -> Value -> Value
rpcResult requestId result =
    object ["jsonrpc" .= ("2.0" :: Text), "id" .= requestId, "result" .= result]

rpcError :: Value -> Int -> Text -> Value
rpcError requestId code messageText =
    object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= requestId
        , "error" .= object ["code" .= code, "message" .= messageText]
        ]
