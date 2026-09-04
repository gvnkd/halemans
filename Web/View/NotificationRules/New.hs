module Web.View.NotificationRules.New where
import Web.View.Prelude

data NewView = NewView
    { teams :: [Team]
    , users :: [User]
    , policies :: [EscalationPolicy]
    }

instance View NewView where
    html NewView { .. } = [hsx|
        <h1>New notification rule</h1>
        <form method="POST" action={CreateNotificationRuleAction} data-testid="notification-rule-form" style="max-width: 600px">
            {notificationRuleFormFields teams users policies "" 0 True "" "" "high" "" 300 Nothing}
            <button type="submit" class="btn btn-primary" data-testid="notification-rule-submit">Create</button>
        </form>
    |]

-- Shared with Edit. `target` is "team:<uuid>" | "user:<uuid>" | "".
notificationRuleFormFields :: [Team] -> [User] -> [EscalationPolicy] -> Text -> Int -> Bool -> Text -> Text -> Text -> Text -> Int -> Maybe (Id EscalationPolicy) -> Html
notificationRuleFormFields teams users policies name position enabled matchFields matchLabels severityThreshold target throttleSeconds policyRef = [hsx|
    <div class="mb-3">
        <label class="form-label">Name</label>
        <input name="name" type="text" class="form-control" value={name} data-testid="rule-name" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Position</label>
        <input name="position" type="number" class="form-control" value={position} data-testid="rule-position"/>
    </div>
    <div class="mb-3 form-check">
        <input name="enabled" type="checkbox" class="form-check-input" checked={enabled} data-testid="rule-enabled"/>
        <label class="form-check-label">Enabled</label>
    </div>
    <div class="mb-3">
        <label class="form-label">Field equals (env, host, service, check, severity, status)</label>
        <input name="matchFields" type="text" class="form-control" value={matchFields} placeholder="env=dev" data-testid="rule-match-fields"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Label globs</label>
        <input name="matchLabels" type="text" class="form-control" value={matchLabels} placeholder="component=db-*" data-testid="rule-match-labels"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Severity threshold (fires when alert severity ≥ this)</label>
        <select name="severityThreshold" class="form-select" data-testid="rule-severity-threshold">
            {forEach ["info", "warning", "high", "critical"] severityOption}
        </select>
    </div>
    <div class="mb-3">
        <label class="form-label">Target</label>
        <select name="target" class="form-select" data-testid="rule-target">
            <option value="" selected={target == ""}>—</option>
            {forEach teams teamOption}
            {forEach users userOption}
        </select>
    </div>
    <div class="mb-3">
        <label class="form-label">Throttle (seconds)</label>
        <input name="throttleSeconds" type="number" class="form-control" value={throttleSeconds} data-testid="rule-throttle"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Escalation policy</label>
        <select name="escalationPolicyId" class="form-select" data-testid="rule-escalation-policy">
            <option value="" selected={isNothing policyRef}>—</option>
            {forEach policies policyOption}
        </select>
    </div>
|]
    where
        severityOption value = [hsx|<option value={value} selected={severityThreshold == value}>{value}</option>|]
        teamOption team = [hsx|
            <option value={"team:" <> tshow (get #id team)} selected={target == "team:" <> tshow (get #id team)}>team: {team.name}</option>
        |]
        userOption user = [hsx|
            <option value={"user:" <> tshow (get #id user)} selected={target == "user:" <> tshow (get #id user)}>user: {user.email}</option>
        |]
        policyOption policy = [hsx|
            <option value={tshow (get #id policy)} selected={policyRef == Just (get #id policy)}>{policy.name}</option>
        |]
