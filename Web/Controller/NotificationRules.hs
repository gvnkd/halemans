module Web.Controller.NotificationRules where

import Application.Helper.RuleForm (parseMatchFormFacets)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Web.Controller.Prelude
import Web.View.NotificationRules.Edit
import Web.View.NotificationRules.Index
import Web.View.NotificationRules.New

instance Controller NotificationRulesController where
    beforeAction = ensureIsUser

    action NotificationRulesAction = do
        requirePrivilege "manage_rules"
        rules <- query @NotificationRule |> orderByAsc #position |> fetch
        targets <- forM rules targetLabel
        render IndexView{rulesWithTargets = zip rules targets}
    action NewNotificationRuleAction = do
        requirePrivilege "manage_rules"
        (teams, users, policies) <- formChoices
        render NewView{teams, users, policies}
    action CreateNotificationRuleAction = do
        requirePrivilege "manage_rules"
        let (teamRef, userRef) = targetRef
        _ <-
            newRecord @NotificationRule
                |> set #name (param @Text "name")
                |> set #position (param @Int "position")
                |> set #enabled enabledParam
                |> set #match matchFormValue
                |> set #severityThreshold (param @Text "severityThreshold")
                |> set #teamId teamRef
                |> set #userId userRef
                |> set #channel channelParam
                |> set #channelConfig channelConfigValue
                |> set #throttleSeconds (param @Int "throttleSeconds")
                |> set #escalationPolicyId policyRef
                |> createRecord
        setSuccessMessage (tr "Notification rule created")
        redirectTo NotificationRulesAction
    action EditNotificationRuleAction{notificationRuleId} = do
        requirePrivilege "manage_rules"
        rule <- fetch notificationRuleId
        ensureNotProtected rule.name (get #protected rule)
        (teams, users, policies) <- formChoices
        render EditView{rule, teams, users, policies}
    action UpdateNotificationRuleAction{notificationRuleId} = do
        requirePrivilege "manage_rules"
        rule <- fetch notificationRuleId
        ensureNotProtected rule.name (get #protected rule)
        let (teamRef, userRef) = targetRef
        _ <-
            rule
                |> set #name (param @Text "name")
                |> set #position (param @Int "position")
                |> set #enabled enabledParam
                |> set #match matchFormValue
                |> set #severityThreshold (param @Text "severityThreshold")
                |> set #teamId teamRef
                |> set #userId userRef
                |> set #channel channelParam
                |> set #channelConfig channelConfigValue
                |> set #throttleSeconds (param @Int "throttleSeconds")
                |> set #escalationPolicyId policyRef
                |> updateRecord
        setSuccessMessage (tr "Notification rule updated")
        redirectTo NotificationRulesAction
    action DeleteNotificationRuleAction{notificationRuleId} = do
        requirePrivilege "manage_rules"
        rule <- fetch notificationRuleId
        ensureNotProtected rule.name (get #protected rule)
        deleteRecord rule
        setSuccessMessage (tr "Notification rule deleted")
        redirectTo NotificationRulesAction

formChoices :: (?modelContext :: ModelContext) => IO ([Team], [User], [EscalationPolicy])
formChoices = do
    teams <- query @Team |> orderByAsc #name |> fetch
    users <- query @User |> orderByAsc #email |> fetch
    policies <- query @EscalationPolicy |> orderByAsc #name |> fetch
    pure (teams, users, policies)

-- | Target select value: "team:<uuid>" | "user:<uuid>" | "" (§2: team_id XOR
-- user_id).
targetRef :: (?request :: Request, ?respond :: Respond) => (Maybe (Id Team), Maybe (Id User))
targetRef =
    let raw = param @Text "target"
        (kind, rest) = Text.break (== ':') raw
        rawId = Text.drop 1 rest
     in case kind of
            "team" -> (Just (textToId rawId), Nothing)
            "user" -> (Nothing, Just (textToId rawId))
            _ -> (Nothing, Nothing)

policyRef :: (?request :: Request, ?respond :: Respond) => Maybe (Id EscalationPolicy)
policyRef = case paramOrNothing @Text "escalationPolicyId" of
    Just raw | raw /= "" -> Just (textToId raw)
    _ -> Nothing

enabledParam :: (?request :: Request, ?respond :: Respond) => Bool
enabledParam = paramOrNothing @Text "enabled" == Just "on"

-- Match jsonb from the three CSV inputs (fields / labels / facet globs).
matchFormValue :: (?request :: Request, ?respond :: Respond) => Value
matchFormValue =
    parseMatchFormFacets
        (param @Text "matchFields")
        (param @Text "matchLabels")
        (param @Text "matchFacets")

-- Channel select: browser_push | email (the two the notification engine and
-- the agent tool accept); unknown values fall back to browser_push.
channelParam :: (?request :: Request, ?respond :: Respond) => Text
channelParam = case param @Text "channel" of
    "email" -> "email"
    _ -> "browser_push"

-- Channel config textarea: must be a JSON object; empty/invalid falls back to
-- {} (the form text documents the format).
channelConfigValue :: (?request :: Request, ?respond :: Respond) => Value
channelConfigValue = case paramOrNothing @Text "channelConfig" of
    Just raw
        | not (Text.null (Text.strip raw)) ->
            fromMaybe (Aeson.object []) (Aeson.decode (cs raw))
    _ -> Aeson.object []

targetLabel :: (?modelContext :: ModelContext) => NotificationRule -> IO Text
targetLabel rule = case (rule.teamId, rule.userId) of
    (Just teamId, _) -> do
        team <- fetch teamId
        pure ("team: " <> team.name)
    (_, Just userId) -> do
        user <- fetch userId
        pure ("user: " <> user.email)
    _ -> pure "-"
