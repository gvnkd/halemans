module Web.View.GroupingRules.Index where
import Web.View.Prelude

data IndexView = IndexView { rules :: [GroupingRule] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex justify-content-between align-items-center">
            <h1>Grouping rules</h1>
            <a href={NewGroupingRuleAction} class="btn btn-sm btn-primary" data-testid="new-grouping-rule">New rule</a>
        </div>
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

renderRule :: GroupingRule -> Html
renderRule rule = [hsx|
    <tr data-testid="grouping-rule-row">
        <td>{rule.position}</td>
        <td>{rule.name}</td>
        <td>{enabledBadge}</td>
        <td data-testid="grouping-rule-version">{rule.version}</td>
        <td><code>{rule.groupKeyTemplate}</code></td>
        <td>
            <a href={PreviewGroupingRuleAction rule.id} class="btn btn-sm btn-outline-secondary" data-testid="preview-grouping-rule">Preview</a>
            <a href={EditGroupingRuleAction rule.id} class="btn btn-sm btn-outline-secondary" data-testid="edit-grouping-rule">Edit</a>
            <form method="POST" action={DeleteGroupingRuleAction rule.id} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-danger">Delete</button>
            </form>
        </td>
    </tr>
|]
    where
        enabledBadge = if rule.enabled
            then [hsx|<span class="badge bg-success">enabled</span>|]
            else [hsx|<span class="badge bg-secondary">disabled</span>|]
