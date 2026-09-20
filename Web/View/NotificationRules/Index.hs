module Web.View.NotificationRules.Index where

import Web.View.Fragments (editDeleteActionsHtml, emptyStateHtml, enabledBadgeHtml, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView {rulesWithTargets :: [(NotificationRule, Text)]}

instance View IndexView where
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml (tr "Notification rules") newButton}
        {tableOrEmpty}
    |]
      where
        tableOrEmpty =
            if null rulesWithTargets
                then emptyStateHtml "notification-rules-empty" (tr "No notification rules yet — alerts notify nobody.")
                else
                    [hsx|
        <table class="table" data-testid="notification-rules-table">
            <thead>
                <tr>
                    <th>{tr "Position"}</th>
                    <th>{tr "Name"}</th>
                    <th>{tr "Enabled"}</th>
                    <th>{tr "Severity ≥"}</th>
                    <th>{tr "Target"}</th>
                    <th>{tr "Throttle"}</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach rulesWithTargets renderRule}
            </tbody>
        </table>
                    |]
        newButton = [hsx|<a href={NewNotificationRuleAction} class="btn btn-brand" data-testid="new-notification-rule">{tr "New rule"}</a>|]

renderRule :: (NotificationRule, Text) -> Html
renderRule (rule, target) =
    [hsx|
    <tr data-testid="notification-rule-row">
        <td>{rule.position}</td>
        <td>{rule.name} {protectedBadgeHtml (get #protected rule)}</td>
        <td>{enabledBadgeHtml rule.enabled}</td>
        <td>{rule.severityThreshold}</td>
        <td>{target}</td>
        <td>{rule.throttleSeconds}s</td>
        <td>
            {editDeleteActionsHtml (pathTo (EditNotificationRuleAction rule.id)) (pathTo (DeleteNotificationRuleAction rule.id)) "edit-notification-rule"}
        </td>
    </tr>
|]
