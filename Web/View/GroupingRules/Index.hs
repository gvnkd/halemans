module Web.View.GroupingRules.Index where
import Web.View.Prelude
import Web.View.Fragments (pageHeaderHtml, editDeleteActionsHtml, enabledBadgeHtml)

data IndexView = IndexView { rules :: [GroupingRule] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {pageHeaderHtml "Grouping rules" newButton}
        <table class="table" data-testid="grouping-rules-table">
            <thead>
                <tr>
                    <th>Position</th>
                    <th>Name</th>
                    <th>Enabled</th>
                    <th>Version</th>
                    <th>Group key template</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach rules renderRule}
            </tbody>
        </table>
    |]
        where
            newButton = [hsx|<a href={NewGroupingRuleAction} class="btn btn-sm btn-primary" data-testid="new-grouping-rule">New rule</a>|]

renderRule :: GroupingRule -> Html
renderRule rule = [hsx|
    <tr data-testid="grouping-rule-row">
        <td>{rule.position}</td>
        <td>{rule.name}</td>
        <td>{enabledBadgeHtml rule.enabled}</td>
        <td data-testid="grouping-rule-version">{rule.version}</td>
        <td><code>{rule.groupKeyTemplate}</code></td>
        <td>
            <a href={PreviewGroupingRuleAction rule.id} class="btn btn-sm btn-outline-secondary" data-testid="preview-grouping-rule">Preview</a>
            {editDeleteActionsHtml (pathTo (EditGroupingRuleAction rule.id)) (pathTo (DeleteGroupingRuleAction rule.id)) "edit-grouping-rule"}
        </td>
    </tr>
|]
