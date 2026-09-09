module Web.View.NotificationRules.Index where
import Web.View.Prelude
import Web.View.Fragments (pageHeaderHtml, editDeleteActionsHtml, enabledBadgeHtml)

data IndexView = IndexView { rulesWithTargets :: [(NotificationRule, Text)] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {pageHeaderHtml "Notification rules" newButton}
        <table class="table" data-testid="notification-rules-table">
            <thead>
                <tr>
                    <th>Position</th>
                    <th>Name</th>
                    <th>Enabled</th>
                    <th>Severity ≥</th>
                    <th>Target</th>
                    <th>Throttle</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach rulesWithTargets renderRule}
            </tbody>
        </table>
    |]
        where
            newButton = [hsx|<a href={NewNotificationRuleAction} class="btn btn-sm btn-primary" data-testid="new-notification-rule">New rule</a>|]

renderRule :: (NotificationRule, Text) -> Html
renderRule (rule, target) = [hsx|
    <tr data-testid="notification-rule-row">
        <td>{rule.position}</td>
        <td>{rule.name}</td>
        <td>{enabledBadgeHtml rule.enabled}</td>
        <td>{rule.severityThreshold}</td>
        <td>{target}</td>
        <td>{rule.throttleSeconds}s</td>
        <td>
            {editDeleteActionsHtml (pathTo (EditNotificationRuleAction rule.id)) (pathTo (DeleteNotificationRuleAction rule.id)) "edit-notification-rule"}
        </td>
    </tr>
|]
