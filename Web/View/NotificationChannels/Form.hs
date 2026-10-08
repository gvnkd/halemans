module Web.View.NotificationChannels.Form (
    ChannelFormValues (..),
    defaultChannelFormValues,
    channelFormFields,
    valuesFromChannel,
    knownColorKeys,
) where

import Application.Service.Mattermost.Render (ackActionEnabledFromJson, bannerEnabledFromJson, bannerTrendMinutesFromJson, deleteOnCloseEnabledFromJson)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Prelude

-- Shared new/edit fields for notification channels (the source-form
-- pattern): credentials are env-var references (tokenEnv), never raw tokens.
-- Every mattermost config key is form-managed: banner statistics, the trend
-- window, and the severity/status color overrides — hand-set/provisioned
-- UNKNOWN keys still survive a save (controller overlay).
data ChannelFormValues = ChannelFormValues
    { formChannelName :: Text
    , formChannelType :: Text
    , formChannelBaseUrl :: Text
    , formChannelTokenEnv :: Text
    , formChannelAckAction :: Bool
    , formChannelDeleteOnClose :: Bool
    , formChannelBanner :: Bool
    , formChannelBannerTrendMinutes :: Text
    , formChannelColors :: [(Text, Text)]
    , formChannelEnabled :: Bool
    }

defaultChannelFormValues :: ChannelFormValues
defaultChannelFormValues =
    ChannelFormValues
        { formChannelName = ""
        , formChannelType = "mattermost"
        , formChannelBaseUrl = ""
        , formChannelTokenEnv = ""
        , formChannelAckAction = True
        , formChannelDeleteOnClose = False
        , formChannelBanner = False
        , formChannelBannerTrendMinutes = "30"
        , formChannelColors = [(key, "") | key <- knownColorKeys]
        , formChannelEnabled = True
        }

-- The color-map keys the form manages (Application.Service.Mattermost.Render
-- defaultColorMap). Empty input = built-in default; unknown extra keys set by
-- hand/provision survive a save.
knownColorKeys :: [Text]
knownColorKeys = ["critical", "high", "warning", "default", "resolved", "closed", "stalled"]

channelFormFields :: (CurrentUserRecord ~ User, ?request :: Request) => ChannelFormValues -> Html
channelFormFields values =
    [hsx|
    <div class="mb-3">
        <label class="form-label">{tr "Name"}</label>
        <input name="name" type="text" class="form-control" value={values.formChannelName} data-testid="channel-name" required="required"/>
        <div class="form-text">{tr "Notification rules reference the channel by this name."}</div>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Type"}</label>
        <select name="type" class="select" data-testid="channel-type">
            {forEach ["mattermost", "browser_push", "email"] typeOption}
        </select>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Base URL"}</label>
        <input name="baseUrl" type="text" class="form-control" value={values.formChannelBaseUrl} data-testid="channel-base-url" placeholder="https://mattermost.example.com"/>
        <div class="form-text">{tr "Server address (mattermost)."}</div>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Token env var"}</label>
        <input name="tokenEnv" type="text" class="form-control" value={values.formChannelTokenEnv} data-testid="channel-token-env" placeholder="MATTERMOST_TOKEN"/>
        <div class="form-text">{tr "Name of the environment variable holding the bot token (mattermost)."}</div>
    </div>
    <div class="mb-3 form-check">
        <input name="ackAction" type="checkbox" class="form-check-input" checked={values.formChannelAckAction} data-testid="channel-ack-action"/>
        <label class="form-check-label">{tr "Ack action button"}</label>
        <div class="form-text">{tr "Show the interactive Ack button on firing cards (mattermost). The one-time [Ack] link in the message stays either way."}</div>
    </div>
    <div class="mb-3 form-check">
        <input name="deleteOnClose" type="checkbox" class="form-check-input" checked={values.formChannelDeleteOnClose} data-testid="channel-delete-on-close"/>
        <label class="form-check-label">{tr "Delete card when resolved/closed"}</label>
        <div class="form-text">{tr "Delete the Mattermost post when the alert reaches a terminal state instead of leaving a gray card (root post only — thread replies stay)."}</div>
    </div>
    <div class="mb-3 form-check">
        <input name="banner" type="checkbox" class="form-check-input" checked={values.formChannelBanner} data-testid="channel-banner"/>
        <label class="form-check-label">{tr "Channel banner"}</label>
        <div class="form-text">{tr "Show live alert statistics (active counts per severity with trend arrows) as the Mattermost channel banner. Requires MM 10.9+ and channel-management permission for the bot."}</div>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Banner trend window (minutes)"}</label>
        <input name="bannerTrendMinutes" type="number" min="1" class="form-control" value={values.formChannelBannerTrendMinutes} data-testid="channel-banner-trend-minutes"/>
        <div class="form-text">{tr "Trend arrows compare current counts against the snapshot this many minutes back."}</div>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Colors"}</label>
        <div class="row g-2">
            {forEach knownColorKeys colorField}
        </div>
        <div class="form-text">{tr "Hex color overrides for card bars and status (e.g. #E5484D). Empty = built-in default."}</div>
    </div>
    <div class="mb-3 form-check">
        <input name="enabled" type="checkbox" class="form-check-input" checked={values.formChannelEnabled} data-testid="channel-enabled"/>
        <label class="form-check-label">{tr "Enabled"}</label>
    </div>
|]
  where
    typeOption value = [hsx|<option value={value} selected={values.formChannelType == value}>{value}</option>|]
    colorField key =
        [hsx|
        <div class="col-md-4">
            <div class="input-group input-group-sm">
                <span class="input-group-text">{key}</span>
                <input name={"color_" <> key} type="text" class="form-control" value={colorValue key} data-testid={"channel-color-" <> key} placeholder="#RRGGBB"/>
            </div>
        </div>|]
    colorValue key = fromMaybe "" (lookup key values.formChannelColors)

valuesFromChannel :: NotificationChannel -> ChannelFormValues
valuesFromChannel channel =
    ChannelFormValues
        { formChannelName = channel.name
        , formChannelType = channel.type_
        , formChannelBaseUrl = channel.baseUrl
        , formChannelTokenEnv = tokenEnvOf channel
        , formChannelAckAction = ackActionEnabledFromJson (get #config channel)
        , formChannelDeleteOnClose = deleteOnCloseEnabledFromJson (get #config channel)
        , formChannelBanner = bannerEnabledFromJson (get #config channel)
        , formChannelBannerTrendMinutes = tshow (bannerTrendMinutesFromJson (get #config channel))
        , formChannelColors = [(key, colorValue key) | key <- knownColorKeys]
        , formChannelEnabled = channel.enabled
        }
  where
    tokenEnvOf = fromMaybe "" . parseMaybe (Aeson.withObject "config" (\o -> o Aeson..:? "tokenEnv" Aeson..!= "")) . get #config
    colorValue key = fromMaybe "" (lookup key (colorMapOf (get #config channel)))

-- The "colors" object of a channel config ([(key, hex)]; [] when unset).
colorMapOf :: Aeson.Value -> [(Text, Text)]
colorMapOf config = case config of
    Aeson.Object object_ -> case KeyMap.lookup "colors" object_ of
        Just (Aeson.Object colors) ->
            [ (Key.toText key, value)
            | (key, Aeson.String value) <- KeyMap.toList colors
            ]
        _ -> []
    _ -> []
