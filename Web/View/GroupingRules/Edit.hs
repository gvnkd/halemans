module Web.View.GroupingRules.Edit where

import Application.Helper.RuleForm (matchFacetsText, matchFieldsText, matchLabelsText)
import Web.View.GroupingRules.New (groupingRuleFormFields)
import Web.View.Prelude

data EditView = EditView {rule :: GroupingRule}

instance View EditView where
    html EditView{..} =
        [hsx|
        <h1>Edit grouping rule</h1>
        <p class="text-secondary">Saving bumps the rule version (current: {rule.version}); already-grouped alerts keep their group.</p>
        <form method="POST" action={UpdateGroupingRuleAction rule.id} data-testid="grouping-rule-edit-form" class="maxw-600">
            {groupingRuleFormFields rule.name rule.position rule.enabled (matchFieldsText rule.match) (matchLabelsText rule.match) (matchFacetsText rule.match) rule.groupKeyTemplate}
            <button type="submit" class="btn btn-primary" data-testid="grouping-rule-submit">Save</button>
            <a href={PreviewGroupingRuleAction rule.id} class="btn btn-outline-secondary" data-testid="preview-grouping-rule">Preview</a>
        </form>
    |]
