module Application.Service.Mattermost.Api (
    MattermostConfig (..),
    configForChannel,
    createPost,
    patchPost,
    resolveChannel,
    testConnection,
) where

import Application.Service.Http qualified as Http
import Control.Exception (SomeException, displayException, try)
import Control.Lens ((&), (.~), (^.))
import Control.Monad (void)
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as L
import Data.Traversable (traverse)
import Data.Vector qualified as Vector
import Generated.Types
import IHP.Prelude
import Network.Wreq qualified as Wreq
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
    }
    deriving (Eq, Show)

-- | Resolve a channel row to a usable config; Nothing when the row is not a
-- usable mattermost channel (wrong type, empty base URL, unset token env).
configForChannel :: NotificationChannel -> IO (Maybe MattermostConfig)
configForChannel channel = do
    maybeToken <- traverse (lookupEnv . cs) tokenEnv
    pure case (channel.type_ == "mattermost", channel.baseUrl, maybeToken) of
        (True, baseUrl, Just (Just token))
            | not (null baseUrl) && not (null token) ->
                Just MattermostConfig{mmBaseUrl = baseUrl, mmToken = cs token}
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
    fmap Control.Monad.void (putJson config ("/api/v4/posts/" <> postId) payload)
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
