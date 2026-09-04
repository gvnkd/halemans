module Web.View.NotificationRules.Index where
import Web.View.Prelude

data IndexView = IndexView { rulesWithTargets :: [(NotificationRule, Text)] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex justify-content-between align-items-center">
            <h1>Notification rules</h1>
            <a href={NewNotificationRuleAction} class="btn btn-sm btn-primary" data-testid="new-notification-rule">New rule</a>
        </div>
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

renderRule :: (NotificationRule, Text) -> Html
renderRule (rule, target) = [hsx|
    <tr data-testid="notification-rule-row">
        <td>{rule.position}</td>
        <td>{rule.name}</td>
        <td>{enabledBadge}</td>
        <td>{rule.severityThreshold}</td>
        <td>{target}</td>
        <td>{rule.throttleSeconds}s</td>
        <td>
            <a href={EditNotificationRuleAction rule.id} class="btn btn-sm btn-outline-secondary" data-testid="edit-notification-rule">Edit</a>
            <form method="POST" action={DeleteNotificationRuleAction rule.id} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-danger">Delete</button>
            </form>
        </td>
    </tr>
|]
    where
        enabledBadge = if rule.enabled
            then [hsx|<span class="badge bg-success">enabled</span>|]
            else [hsx|<span class="badge bg-secondary">disabled</span>|]
