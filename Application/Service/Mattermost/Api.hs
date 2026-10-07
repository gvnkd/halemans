module Application.Service.Mattermost.Api (
    MattermostConfig (..),
    configForChannel,
    createPost,
    patchPost,
    deletePost,
    resolveChannel,
    getMe,
    channelPosts,
    testConnection,
) where

import qualified Application.Service.Http as Http
import qualified Application.Service.Mattermost.Render as Render
import Control.Exception (SomeException, displayException, try)
import Control.Lens ((&), (.~), (^.))
import Control.Monad (void)
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as L
import Data.Traversable (traverse)
import qualified Data.Vector as Vector
import Generated.Types
import IHP.Prelude
import qualified Network.Wreq as Wreq
import System.Environment (lookupEnv)

-- Mattermost bot-API client (the v4 subset the notification channel needs).
-- A bot account with a personal access token is REQUIRED: incoming webhooks
-- cannot edit posts, and the Ack flow re-renders the root message after the
-- click. The server address and the token come from the channel's
-- notification_channels row (base_url + config.tokenEnv naming the env var
-- that holds the secret — the zabbix/grafana source pattern); a missing
-- baseUrl/token is a soft-skip (Nothing), like push without VAPID keys.

data MattermostConfig = MattermostConfig
    { mmBaseUrl :: Text
    , mmToken :: Text
    , mmColorOverrides :: [(Text, Text)]
    , mmAckAction :: Bool
    , mmDeleteOnClose :: Bool
    }
    deriving (Eq, Show)

-- | Resolve a channel row to a usable config; Nothing when the row is not a
-- usable mattermost channel (wrong type, empty base URL, unset token env).
-- mmColorOverrides carries the config "colors" mapping (severity/status →
-- hex) that templates reference via the {{color}} slot; mmAckAction is the
-- config "ackAction" flag (default True) — False hides the interactive Ack
-- BUTTON (the markdown [Ack] link in the status line stays template-controlled);
-- mmDeleteOnClose is the config "deleteOnClose" flag (default False) — True
-- DELETEs the root post when the alert reaches resolved/closed instead of
-- patching it to a terminal-gray card.
configForChannel :: NotificationChannel -> IO (Maybe MattermostConfig)
configForChannel channel = do
    maybeToken <- traverse (lookupEnv . cs) tokenEnv
    pure case (channel.type_ == "mattermost", channel.baseUrl, maybeToken) of
        (True, baseUrl, Just (Just token))
            | not (null baseUrl) && not (null token) ->
                Just
                    MattermostConfig
                        { mmBaseUrl = baseUrl
                        , mmToken = cs token
                        , mmColorOverrides = Render.colorMapFromJson channel.config
                        , mmAckAction = Render.ackActionEnabledFromJson channel.config
                        , mmDeleteOnClose = Render.deleteOnCloseEnabledFromJson channel.config
                        }
        _ -> Nothing
  where
    tokenEnv :: Maybe Text
    tokenEnv = case channel.config of
        Aeson.Object object_ -> case KeyMap.lookup "tokenEnv" object_ of
            Just (Aeson.String value) -> Just value
            _ -> Nothing
        _ -> Nothing

mmOpts :: MattermostConfig -> Wreq.Options
mmOpts config = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs config.mmToken]

-- | Create a post (a thread reply when rootId is given); returns the new
-- post id.
createPost :: MattermostConfig -> Text -> Text -> Maybe Text -> Aeson.Value -> IO (Either Text Text)
createPost config channelId message rootId props =
    fmap (fmap postId) (postJson config "/api/v4/posts" payload)
  where
    payload =
        Aeson.object
            ( [ "channel_id" .= channelId
              , "message" .= message
              , "props" .= props
              ]
                <> [("root_id" .= root) | Just root <- [rootId]]
            )
    postId body = fromMaybe "" (parseMaybe (Aeson.withObject "post" (\o -> o Aeson..: "id")) body)

-- | Edit a post's message + props (the status-sync path).
patchPost :: MattermostConfig -> Text -> Text -> Aeson.Value -> IO (Either Text ())
patchPost config postId message props =
    -- The /patch sub-resource, NOT PUT /posts/{id}: the full-post update
    -- endpoint is rejected on some servers (verified 2026-10-02 against
    -- MM 11.8.3/mm.officesvc.bz — the sync silently no-op'd there because
    -- the old path errored and the error was swallowed), while /patch is
    -- the standard edit endpoint and works with the same token.
    fmap Control.Monad.void (putJson config ("/api/v4/posts/" <> postId <> "/patch") payload)
  where
    payload = Aeson.object ["message" .= message, "props" .= props]

-- | Resolve a channel name to its id. The channel must already exist (a bot
-- token cannot auto-create); the mock auto-creates for tests.
-- | teamName is matched against BOTH the team name (URL slug, e.g.
-- "knn-gd") and the display name (e.g. "GD") — operators naturally write
-- either. Resolution uses ONLY the bot's own membership listing plus the
-- team-id channel lookup: the `channels/name/{team}/{channel}` shortcut
-- 404s on some servers (verified 2026-10-01 against mm.officesvc.bz while
-- these two endpoints work), and listing the bot's teams also fails fast
-- with a clear error when the bot was never added to the team.
resolveChannel :: MattermostConfig -> Text -> Text -> IO (Either Text Text)
resolveChannel config teamName channelName = do
    teamsResult <- getJson config "/api/v4/users/me/teams"
    case teamsResult of
        Left err -> pure (Left err)
        Right teams -> case teamIdIn teams of
            Nothing ->
                pure
                    ( Left
                        ( "mattermost: bot is not a member of a team named \""
                            <> teamName
                            <> "\" (matched by name or display name; add the bot account to the team in Mattermost)"
                        )
                    )
            Just teamId ->
                fmap (fmap channelId) (getJson config ("/api/v4/teams/" <> teamId <> "/channels/name/" <> channelName))
  where
    teamIdIn value = case value of
        Aeson.Array arr -> listToMaybe (mapMaybe matchTeam (Vector.toList arr))
        _ -> Nothing
    matchTeam candidate = do
        (teamId, name, displayName) <- parseMaybe parseTeam candidate
        if name == teamName || displayName == teamName then Just teamId else Nothing
    parseTeam = Aeson.withObject "team" \o -> do
        teamId <- o .: "id"
        name <- o .: "name"
        displayName <- o .:? "display_name" .!= ""
        pure (teamId, name, displayName)
    channelId body = fromMaybe "" (parseMaybe (Aeson.withObject "channel" (\o -> o .: "id")) body)

-- | Connectivity check for the admin test button: GET /api/v4/users/me,
-- returning the bot username.
testConnection :: MattermostConfig -> IO (Either Text Text)
testConnection config =
    fmap (fmap username) (getJson config "/api/v4/users/me")
  where
    username body = fromMaybe "" (parseMaybe (Aeson.withObject "user" (\o -> o Aeson..: "username")) body)

-- | Delete a post (the deleteOnClose terminal-state path). The bot can only
-- delete its OWN posts, which is exactly what the notification channel
-- creates. A 404 (post already gone) is treated as success — the desired
-- end state already holds and the sync must not retry forever.
deletePost :: MattermostConfig -> Text -> IO (Either Text ())
deletePost config postId = do
    outcome <-
        try (Http.deleteFollowing (mmOpts config) (cs (config.mmBaseUrl <> "/api/v4/posts/" <> postId))) ::
            IO (Either SomeException (Wreq.Response L.ByteString))
    pure case outcome of
        Left exception -> Left (cs (displayException exception))
        Right response -> case response ^. Wreq.responseStatus . Wreq.statusCode of
            200 -> Right ()
            404 -> Right ()
            other -> Left ("mattermost: delete post failed with HTTP " <> tshow other)

-- | The bot's own user id (GET /users/me). The admin purge uses it to
-- restrict deletes to posts the bot authored.
getMe :: MattermostConfig -> IO (Either Text Text)
getMe config = do
    result <- getJson config "/api/v4/users/me"
    pure case result of
        Left err -> Left err
        Right body -> case parseMaybe (Aeson.withObject "user" (\o -> o Aeson..: "id")) body of
            Nothing -> Left "mattermost: users/me response has no id"
            Just userId -> Right userId

-- | ALL posts of a channel, following the MM "before" cursor at the max page
-- size until a short page. The admin purge walks the channel (not the DB) so
-- deletes are only attempted for posts that still exist. A page failure
-- returns Left — the caller counts the target as failed instead of silently
-- purging nothing.
channelPosts :: MattermostConfig -> Text -> IO (Either Text [Aeson.Value])
channelPosts config channelId = go Nothing []
  where
    go beforeMs acc = do
        result <- getJson config (path beforeMs)
        case result of
            Left err -> pure (Left err)
            Right body -> case pageOf body of
                Nothing -> pure (Left "mattermost: channel posts response has no posts map")
                Just postsMap ->
                    let posts = KeyMap.elems postsMap
                     in if length posts < pageSize
                            then pure (Right (acc <> posts))
                            else case oldestCreateAtMs posts of
                                Nothing -> pure (Right (acc <> posts))
                                Just oldest -> go (Just oldest) (acc <> posts)
    path Nothing = "/api/v4/channels/" <> channelId <> "/posts?per_page=" <> tshow pageSize
    path (Just beforeMs) = "/api/v4/channels/" <> channelId <> "/posts?per_page=" <> tshow pageSize <> "&before=" <> tshow beforeMs
    pageSize = 200 :: Int
    pageOf body =
        parseMaybe (Aeson.withObject "channel posts" (\o -> o Aeson..: "posts")) body ::
            Maybe (KeyMap.KeyMap Aeson.Value)
    oldestCreateAtMs posts =
        case [createdAt | Just createdAt <- map createdAtMs posts] of
            [] -> Nothing
            times -> Just (minimum times)
    createdAtMs value =
        parseMaybe (Aeson.withObject "post" (\o -> o Aeson..: "create_at")) value ::
            Maybe Integer

getJson :: MattermostConfig -> Text -> IO (Either Text Aeson.Value)
getJson config path =
    request config =<< try (Http.getFollowing (mmOpts config) (cs (config.mmBaseUrl <> path)))

postJson :: MattermostConfig -> Text -> Aeson.Value -> IO (Either Text Aeson.Value)
postJson config path payload =
    request config =<< try (Http.postFollowing (mmOpts config) (cs (config.mmBaseUrl <> path)) payload)

putJson :: MattermostConfig -> Text -> Aeson.Value -> IO (Either Text Aeson.Value)
putJson config path payload =
    request config =<< try (Http.putFollowing (mmOpts config) (cs (config.mmBaseUrl <> path)) payload)

request :: MattermostConfig -> Either SomeException (Wreq.Response L.ByteString) -> IO (Either Text Aeson.Value)
request _ (Left exception) = pure (Left (cs (displayException exception)))
request _ (Right response) =
    pure case Aeson.decode (response ^. Wreq.responseBody) of
        Nothing -> Left "mattermost: non-JSON response"
        Just body -> Right body
