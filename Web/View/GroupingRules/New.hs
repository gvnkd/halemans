module Web.View.GroupingRules.New where

import Web.View.Prelude

data NewView = NewView

instance View NewView where
    html NewView =
        [hsx|
        <h1>New grouping rule</h1>
        <form method="POST" action={CreateGroupingRuleAction} data-testid="grouping-rule-form" class="maxw-600">
            {groupingRuleFormFields "" 0 True "" "" ""}
            <button type="submit" class="btn btn-primary" data-testid="grouping-rule-submit">Create</button>
        </form>
    |]

-- Shared with Edit. Text fields are the match-editor's comma-separated
-- inputs (Application.Helper.RuleForm).
groupingRuleFormFields :: Text -> Int -> Bool -> Text -> Text -> Text -> Html
groupingRuleFormFields name position enabled matchFields matchLabels groupKeyTemplate =
    [hsx|
    <div class="mb-3">
        <label class="form-label">Name</label>
        <input name="name" type="text" class="form-control" value={name} data-testid="rule-name" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Position (lower runs first; first match wins)</label>
        <input name="position" type="number" class="form-control" value={position} data-testid="rule-position"/>
    </div>
    <div class="mb-3 form-check">
        <input name="enabled" type="checkbox" class="form-check-input" checked={enabled} data-testid="rule-enabled"/>
        <label class="form-check-label">Enabled</label>
    </div>
    <div class="mb-3">
        <label class="form-label">Field equals (env, host, service, check, severity, status)</label>
        <input name="matchFields" type="text" class="form-control" value={matchFields} placeholder="env=dev, severity=critical" data-testid="rule-match-fields"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Label globs</label>
        <input name="matchLabels" type="text" class="form-control" value={matchLabels} placeholder="component=db-*" data-testid="rule-match-labels"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Group key template</label>
        <input name="groupKeyTemplate" type="text" class="form-control" value={groupKeyTemplate} placeholder="{env}/{host}" data-testid="rule-template" required="required"/>
    </div>
|]
