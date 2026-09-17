module Web.View.GroupingRules.Index where

import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Fragments (editDeleteActionsHtml, enabledBadgeHtml, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView {rules :: [GroupingRule]}

instance View IndexView where
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml (tr "Grouping rules") newButton}
        <table class="table" data-testid="grouping-rules-table">
            <thead>
                <tr>
                    <th>{tr "Position"}</th>
                    <th>{tr "Name"}</th>
                    <th>{tr "Enabled"}</th>
                    <th>{tr "Version"}</th>
                    <th>{tr "Group key template"}</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach rules renderRule}
            </tbody>
        </table>
    |]
      where
        newButton = [hsx|<a href={NewGroupingRuleAction} class="btn btn-sm btn-primary" data-testid="new-grouping-rule">{tr "New rule"}</a>|]

renderRule :: (CurrentUserRecord ~ User, ?request :: Request) => GroupingRule -> Html
renderRule rule =
    [hsx|
    <tr data-testid="grouping-rule-row">
        <td>{rule.position}</td>
        <td>{rule.name}</td>
        <td>{enabledBadgeHtml rule.enabled}</td>
        <td data-testid="grouping-rule-version">{rule.version}</td>
        <td><code>{rule.groupKeyTemplate}</code></td>
        <td>
            <a href={PreviewGroupingRuleAction rule.id} class="btn btn-sm btn-outline-secondary" data-testid="preview-grouping-rule">{tr "Preview"}</a>
            {editDeleteActionsHtml (pathTo (EditGroupingRuleAction rule.id)) (pathTo (DeleteGroupingRuleAction rule.id)) "edit-grouping-rule"}
        </td>
    </tr>
|]
