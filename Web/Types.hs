module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)

data DashboardController
    = DashboardAction
    deriving (Eq, Show, Data)

data EnvironmentsController
    = ShowEnvironmentAction { environmentName :: !Text }
    deriving (Eq, Show, Data)

data AlertsController
    = AlertsAction
    | ShowAlertAction { alertId :: !(Id Alert) }
    | AckAlertAction { alertId :: !(Id Alert) }
    | UnackAlertAction { alertId :: !(Id Alert) }
    | CloseAlertAction { alertId :: !(Id Alert) }
    | CreateCommentAction { alertId :: !(Id Alert) }
    deriving (Eq, Show)

data BlackoutsController
    = BlackoutsAction
    | NewBlackoutAction
    | CreateBlackoutAction
    | EditBlackoutAction { blackoutId :: !(Id Blackout) }
    | UpdateBlackoutAction { blackoutId :: !(Id Blackout) }
    | DeleteBlackoutAction { blackoutId :: !(Id Blackout) }
    deriving (Eq, Show)

data ProfileController
    = ProfileAction
    deriving (Eq, Show)

data PushSubscriptionsController
    = SubscribePushAction
    | UnsubscribePushAction
    deriving (Eq, Show)

data LiveController
    = LiveController
    deriving (Eq, Show, Data)

data GroupsController
    = ShowGroupAction { groupId :: !(Id AlertGroup) }
    | AckGroupAction { groupId :: !(Id AlertGroup) }
    deriving (Eq, Show)

data HooksController
    = HookAlertmanagerAction { token :: !Text }
    | HookGenericAction { token :: !Text }
    deriving (Eq, Show)

data SourcesController
    = SourcesAction
    | NewSourceAction
    | CreateSourceAction
    | EditSourceAction { sourceId :: !(Id Source) }
    | UpdateSourceAction { sourceId :: !(Id Source) }
    | ToggleSourceAction { sourceId :: !(Id Source) }
    deriving (Eq, Show)

data TeamsController
    = TeamsAction
    | NewTeamAction
    | CreateTeamAction
    | EditTeamAction { teamId :: !(Id Team) }
    | UpdateTeamAction { teamId :: !(Id Team) }
    | DeleteTeamAction { teamId :: !(Id Team) }
    deriving (Eq, Show)

data GroupingRulesController
    = GroupingRulesAction
    | NewGroupingRuleAction
    | CreateGroupingRuleAction
    | EditGroupingRuleAction { groupingRuleId :: !(Id GroupingRule) }
    | UpdateGroupingRuleAction { groupingRuleId :: !(Id GroupingRule) }
    | DeleteGroupingRuleAction { groupingRuleId :: !(Id GroupingRule) }
    | PreviewGroupingRuleAction { groupingRuleId :: !(Id GroupingRule) }
    deriving (Eq, Show)

data NotificationRulesController
    = NotificationRulesAction
    | NewNotificationRuleAction
    | CreateNotificationRuleAction
    | EditNotificationRuleAction { notificationRuleId :: !(Id NotificationRule) }
    | UpdateNotificationRuleAction { notificationRuleId :: !(Id NotificationRule) }
    | DeleteNotificationRuleAction { notificationRuleId :: !(Id NotificationRule) }
    deriving (Eq, Show)

data EscalationPoliciesController
    = EscalationPoliciesAction
    | NewEscalationPolicyAction
    | CreateEscalationPolicyAction
    | EditEscalationPolicyAction { escalationPolicyId :: !(Id EscalationPolicy) }
    | UpdateEscalationPolicyAction { escalationPolicyId :: !(Id EscalationPolicy) }
    | DeleteEscalationPolicyAction { escalationPolicyId :: !(Id EscalationPolicy) }
    deriving (Eq, Show)
