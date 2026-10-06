module Web.View.NotificationChannels.Form (
    ChannelFormValues (..),
    defaultChannelFormValues,
    channelFormFields,
    valuesFromChannel,
) where

import Application.Service.Mattermost.Render (ackActionEnabledFromJson)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Prelude

-- Shared new/edit fields for notification channels (the source-form
-- pattern): credentials are env-var references (tokenEnv), never raw tokens.
data ChannelFormValues = ChannelFormValues
    { formChannelName :: Text
    , formChannelType :: Text
    , formChannelBaseUrl :: Text
    , formChannelTokenEnv :: Text
    , formChannelAckAction :: Bool
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
        , formChannelEnabled = True
        }

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
        <div class="form-text">{tr "Show the interactive Ack button on firing cards (mattermost). The one-time [Ack] link in the message stays either way; other config keys (e.g. \"colors\") are preserved on save."}</div>
    </div>
    <div class="mb-3 form-check">
        <input name="enabled" type="checkbox" class="form-check-input" checked={values.formChannelEnabled} data-testid="channel-enabled"/>
        <label class="form-check-label">{tr "Enabled"}</label>
    </div>
|]
  where
    typeOption value = [hsx|<option value={value} selected={values.formChannelType == value}>{value}</option>|]

valuesFromChannel :: NotificationChannel -> ChannelFormValues
valuesFromChannel channel =
    ChannelFormValues
        { formChannelName = channel.name
        , formChannelType = channel.type_
        , formChannelBaseUrl = channel.baseUrl
        , formChannelTokenEnv = tokenEnvOf channel
        , formChannelAckAction = ackActionEnabledFromJson (get #config channel)
        , formChannelEnabled = channel.enabled
        }
  where
    tokenEnvOf = fromMaybe "" . parseMaybe (Aeson.withObject "config" (\o -> o Aeson..:? "tokenEnv" Aeson..!= "")) . get #config
