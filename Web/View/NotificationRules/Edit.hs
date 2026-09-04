module Web.View.NotificationRules.Edit where
import Web.View.Prelude
import Web.View.NotificationRules.New (notificationRuleFormFields)
import Application.Helper.RuleForm (matchFieldsText, matchLabelsText)

data EditView = EditView
    { rule :: NotificationRule
    , teams :: [Team]
    , users :: [User]
    , policies :: [EscalationPolicy]
    }

instance View EditView where
    html EditView { .. } = [hsx|
        <h1>Edit notification rule</h1>
        <form method="POST" action={UpdateNotificationRuleAction rule.id} data-testid="notification-rule-edit-form" style="max-width: 600px">
            {notificationRuleFormFields teams users policies rule.name rule.position rule.enabled (matchFieldsText rule.match) (matchLabelsText rule.match) rule.severityThreshold target rule.throttleSeconds rule.escalationPolicyId}
            <button type="submit" class="btn btn-primary" data-testid="notification-rule-submit">Save</button>
        </form>
    |]
        where
            target :: Text
            target = case (rule.teamId, rule.userId) of
                (Just teamId, _) -> "team:" <> tshow teamId
                (_, Just userId) -> "user:" <> tshow userId
                _ -> ""
