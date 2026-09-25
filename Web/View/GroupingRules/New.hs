module Web.View.GroupingRules.New where

import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data NewView = NewView

instance View NewView where
    beforeRender _ = setPageTitle (tr "Grouping rules")
    html NewView =
        [hsx|
        {pageHeaderHtml (tr "New grouping rule") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={CreateGroupingRuleAction} data-testid="grouping-rule-form">
            {groupingRuleFormFields "" 0 True "" "" "" ""}
            <button type="submit" class="btn btn-brand" data-testid="grouping-rule-submit">{tr "Create"}</button>
        </form>
        </div></div>|]

-- Shared with Edit. Text fields are the match-editor's comma-separated
-- inputs (Application.Helper.RuleForm).
groupingRuleFormFields :: (CurrentUserRecord ~ User, ?request :: Request) => Text -> Int -> Bool -> Text -> Text -> Text -> Text -> Html
groupingRuleFormFields name position enabled matchFields matchLabels matchFacets groupKeyTemplate =
    [hsx|
    <div class="mb-3">
        <label class="form-label">{tr "Name"}</label>
        <input name="name" type="text" class="form-control" value={name} data-testid="rule-name" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Position (lower runs first; first match wins)"}</label>
        <input name="position" type="number" class="form-control" value={position} data-testid="rule-position"/>
    </div>
    <div class="mb-3 form-check">
        <input name="enabled" type="checkbox" class="form-check-input" checked={enabled} data-testid="rule-enabled"/>
        <label class="form-check-label">{tr "Enabled"}</label>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Field equals (env, host, service, check, severity, status)"}</label>
        <input name="matchFields" type="text" class="form-control" value={matchFields} placeholder="env=dev, severity=critical" data-testid="rule-match-fields"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Label globs"}</label>
        <input name="matchLabels" type="text" class="form-control" value={matchLabels} placeholder="component=db-*" data-testid="rule-match-labels"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Facet globs"}</label>
        <input name="matchFacets" type="text" class="form-control" value={matchFacets} placeholder="DB Cluster=ib-*" data-testid="rule-match-facets"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Group key template"}</label>
        <input name="groupKeyTemplate" type="text" class="form-control" value={groupKeyTemplate} placeholder="{env}/{host}" data-testid="rule-template" required="required"/>
    </div>
|]
