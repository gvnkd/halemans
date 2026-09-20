module Web.View.GroupingRules.Edit where

import Application.Helper.RuleForm (matchFacetsText, matchFieldsText, matchLabelsText)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.GroupingRules.New (groupingRuleFormFields)
import Web.View.Prelude

data EditView = EditView {rule :: GroupingRule}

instance View EditView where
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit grouping rule") mempty}
        <p class="text-secondary">{trp "Saving bumps the rule version (current: {version}); already-grouped alerts keep their group." [("version", tshow rule.version)]}</p>
        <form method="POST" action={UpdateGroupingRuleAction rule.id} data-testid="grouping-rule-edit-form" class="maxw-600">
            {groupingRuleFormFields rule.name rule.position rule.enabled (matchFieldsText rule.match) (matchLabelsText rule.match) (matchFacetsText rule.match) rule.groupKeyTemplate}
            <button type="submit" class="btn btn-primary" data-testid="grouping-rule-submit">{tr "Save"}</button>
            <a href={PreviewGroupingRuleAction rule.id} class="btn btn-outline-secondary" data-testid="preview-grouping-rule">{tr "Preview"}</a>
        </form>
    |]
