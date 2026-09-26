module Web.View.NotificationRules.Edit where

import Application.Helper.RuleForm (matchFacetsText, matchFieldsText, matchLabelsText)
import qualified Data.Aeson as Aeson

import Web.View.Fragments (pageHeaderHtml)
import Web.View.NotificationRules.New (notificationRuleFormFields)
import Web.View.Prelude

data EditView = EditView
    { rule :: NotificationRule
    , teams :: [Team]
    , users :: [User]
    , policies :: [EscalationPolicy]
    }

instance View EditView where
    beforeRender _ = setPageTitle (tr "Notification rules")
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit notification rule") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={UpdateNotificationRuleAction rule.id} data-testid="notification-rule-edit-form">
            {notificationRuleFormFields teams users policies rule.name rule.position rule.enabled (matchFieldsText rule.match) (matchLabelsText rule.match) (matchFacetsText rule.match) rule.channel (channelConfigText rule.channelConfig) rule.severityThreshold target rule.throttleSeconds rule.escalationPolicyId}
            <button type="submit" class="btn btn-brand" data-testid="notification-rule-submit">{tr "Save"}</button>
        </form>
        </div></div>|]
      where
        target :: Text
        target = case (rule.teamId, rule.userId) of
            (Just teamId, _) -> "team:" <> tshow teamId
            (_, Just userId) -> "user:" <> tshow userId
            _ -> ""
        channelConfigText :: Aeson.Value -> Text
        channelConfigText value = cs (Aeson.encode value)
