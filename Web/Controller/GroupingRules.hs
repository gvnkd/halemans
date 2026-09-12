module Web.Controller.GroupingRules where

import Application.Helper.RuleForm (parseMatchForm)
import Application.Pipeline.Grouping (matchAlert, matchExprFromJSON, renderTemplate)
import Web.Controller.Prelude
import Web.View.GroupingRules.Edit
import Web.View.GroupingRules.Index
import Web.View.GroupingRules.New
import Web.View.GroupingRules.Preview

instance Controller GroupingRulesController where
    beforeAction = ensureIsUser

    action GroupingRulesAction = do
        requirePrivilege "manage_rules"
        rules <- query @GroupingRule |> orderByAsc #position |> fetch
        render IndexView{rules}
    action NewGroupingRuleAction = do
        requirePrivilege "manage_rules"
        render NewView
    action CreateGroupingRuleAction = do
        requirePrivilege "manage_rules"
        _ <-
            newRecord @GroupingRule
                |> set #name (param @Text "name")
                |> set #position (param @Int "position")
                |> set #enabled (enabledParam)
                |> set #match (parseMatchForm (param @Text "matchFields") (param @Text "matchLabels"))
                |> set #groupKeyTemplate (param @Text "groupKeyTemplate")
                |> set #createdBy (Just currentUserId)
                |> createRecord
        setSuccessMessage "Grouping rule created"
        redirectTo GroupingRulesAction
    action EditGroupingRuleAction{groupingRuleId} = do
        requirePrivilege "manage_rules"
        rule <- fetch groupingRuleId
        render EditView{rule}

    -- Any edit bumps version (milestone_2.md §4): previously grouped alerts
    -- keep their group and grouped_by_version for a future replay.
    action UpdateGroupingRuleAction{groupingRuleId} = do
        requirePrivilege "manage_rules"
        rule <- fetch groupingRuleId
        _ <-
            rule
                |> set #name (param @Text "name")
                |> set #position (param @Int "position")
                |> set #enabled (enabledParam)
                |> set #match (parseMatchForm (param @Text "matchFields") (param @Text "matchLabels"))
                |> set #groupKeyTemplate (param @Text "groupKeyTemplate")
                |> set #version (rule.version + 1)
                |> updateRecord
        setSuccessMessage "Grouping rule updated (version bumped)"
        redirectTo GroupingRulesAction
    action DeleteGroupingRuleAction{groupingRuleId} = do
        requirePrivilege "manage_rules"
        rule <- fetch groupingRuleId
        deleteRecord rule
        setSuccessMessage "Grouping rule deleted"
        redirectTo GroupingRulesAction

    -- Test-against-recent-alerts preview (§9): shows which of the last 100
    -- alerts the rule would group and the keys it would render.
    action PreviewGroupingRuleAction{groupingRuleId} = do
        requirePrivilege "manage_rules"
        rule <- fetch groupingRuleId
        recent <- query @Alert |> orderByDesc #lastSeenAt |> limit 100 |> fetch
        let expr = matchExprFromJSON rule.match
            matching = filter (matchAlert expr) recent
            preview = map (\alert -> (alert, renderTemplate rule.groupKeyTemplate alert)) matching
        render PreviewView{rule, preview}

enabledParam :: (?request :: Request, ?respond :: Respond) => Bool
enabledParam = paramOrNothing @Text "enabled" == Just "on"
