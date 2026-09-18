module Web.View.NotificationRules.Edit where

import Application.Helper.RuleForm (matchFieldsText, matchLabelsText)
import Web.View.NotificationRules.New (notificationRuleFormFields)
import Web.View.Prelude

data EditView = EditView
    { rule :: NotificationRule
    , teams :: [Team]
    , users :: [User]
    , policies :: [EscalationPolicy]
    }

instance View EditView where
    html EditView{..} =
        [hsx|
        <h1>{tr "Edit notification rule"}</h1>
        <form method="POST" action={UpdateNotificationRuleAction rule.id} data-testid="notification-rule-edit-form" class="maxw-600">
            {notificationRuleFormFields teams users policies rule.name rule.position rule.enabled (matchFieldsText rule.match) (matchLabelsText rule.match) rule.severityThreshold target rule.throttleSeconds rule.escalationPolicyId}
            <button type="submit" class="btn btn-primary" data-testid="notification-rule-submit">{tr "Save"}</button>
        </form>
    |]
      where
        target :: Text
        target = case (rule.teamId, rule.userId) of
            (Just teamId, _) -> "team:" <> tshow teamId
            (_, Just userId) -> "user:" <> tshow userId
            _ -> ""
