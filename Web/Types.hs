module Web.Types where

import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

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
    = ShowEnvironmentAction {environmentName :: !Text}
    deriving (Eq, Show, Data)

data AlertsController
    = AlertsAction
    | ShowAlertAction {alertId :: !(Id Alert)}
    | RenderMetricChartAction {alertId :: !(Id Alert)}
    | AckAlertAction {alertId :: !(Id Alert)}
    | UnackAlertAction {alertId :: !(Id Alert)}
    | CloseAlertAction {alertId :: !(Id Alert)}
    | CreateCommentAction {alertId :: !(Id Alert)}
    | RefreshCmdbAction {alertId :: !(Id Alert)}
    | RefreshAssetsAction {alertId :: !(Id Alert)}
    | CreateJiraTicketAction {alertId :: !(Id Alert)}
    | DeleteJiraLinkAction {alertId :: !(Id Alert), jiraLinkId :: !(Id JiraLink)}
    | ReanalyzeAlertAction {alertId :: !(Id Alert)}
    | LlmFeedbackAction {alertId :: !(Id Alert), analysisId :: !(Id LlmAnalysis)}
    deriving (Eq, Show)

data BlackoutsController
    = BlackoutsAction
    | NewBlackoutAction
    | CreateBlackoutAction
    | EditBlackoutAction {blackoutId :: !(Id Blackout)}
    | UpdateBlackoutAction {blackoutId :: !(Id Blackout)}
    | DeleteBlackoutAction {blackoutId :: !(Id Blackout)}
    deriving (Eq, Show)

data ProfileController
    = ProfileAction
    | UpdateThemeAction
    | UpdateTimezoneAction
    | UpdateLanguageAction
    | CreateApiTokenAction
    | RevokeApiTokenAction {apiTokenId :: !(Id ApiToken)}
    deriving (Eq, Show)

data DashboardsController
    = DashboardsAction
    | NewDashboardAction
    | CreateDashboardAction
    | ShowDashboardAction {dashboardId :: !(Id Dashboard)}
    | ShowDashboardCardAction {dashboardId :: !(Id Dashboard), cardIndex :: !Int}
    | EditDashboardAction {dashboardId :: !(Id Dashboard)}
    | UpdateDashboardAction {dashboardId :: !(Id Dashboard)}
    | DeleteDashboardAction {dashboardId :: !(Id Dashboard)}
    | SetDefaultDashboardAction {dashboardId :: !(Id Dashboard)}
    | MoveDashboardAction {dashboardId :: !(Id Dashboard)}
    deriving (Eq, Show)

data IntegrationsController
    = IntegrationsAction
    | NewJiraConfigAction
    | CreateJiraConfigAction
    | EditJiraConfigAction {jiraConfigId :: !(Id JiraConfig)}
    | UpdateJiraConfigAction {jiraConfigId :: !(Id JiraConfig)}
    | ToggleJiraConfigAction {jiraConfigId :: !(Id JiraConfig)}
    | DeleteJiraConfigAction {jiraConfigId :: !(Id JiraConfig)}
    | TestJiraConfigAction {jiraConfigId :: !(Id JiraConfig)}
    | NewCmdbConfigAction
    | CreateCmdbConfigAction
    | EditCmdbConfigAction {cmdbConfigId :: !(Id CmdbConfig)}
    | UpdateCmdbConfigAction {cmdbConfigId :: !(Id CmdbConfig)}
    | ToggleCmdbConfigAction {cmdbConfigId :: !(Id CmdbConfig)}
    | DeleteCmdbConfigAction {cmdbConfigId :: !(Id CmdbConfig)}
    | TestCmdbConfigAction {cmdbConfigId :: !(Id CmdbConfig)}
    deriving (Eq, Show)

data LlmAdminController
    = LlmAdminAction
    | LlmQueueAction
    | DropLlmAnalysisAction {analysisId :: !(Id LlmAnalysis)}
    | NewLlmTemplateAction
    | CreateLlmTemplateAction
    | EditLlmTemplateAction {templateId :: !(Id LlmPromptTemplate)}
    | UpdateLlmTemplateAction {templateId :: !(Id LlmPromptTemplate)}
    | ActivateLlmTemplateAction {templateId :: !(Id LlmPromptTemplate)}
    | DeleteLlmTemplateAction {templateId :: !(Id LlmPromptTemplate)}
    | TestLlmConnectionAction
    | NewLlmProviderAction
    | CreateLlmProviderAction
    | EditLlmProviderAction {providerId :: !(Id LlmConfig)}
    | UpdateLlmProviderAction {providerId :: !(Id LlmConfig)}
    | EnableLlmProviderAction {providerId :: !(Id LlmConfig)}
    | DisableLlmProviderAction {providerId :: !(Id LlmConfig)}
    | DeleteLlmProviderAction {providerId :: !(Id LlmConfig)}
    | NewLlmRoleAction
    | CreateLlmRoleAction
    | EditLlmRoleAction {roleId :: !(Id LlmAgentRole)}
    | UpdateLlmRoleAction {roleId :: !(Id LlmAgentRole)}
    | ToggleLlmRoleAction {roleId :: !(Id LlmAgentRole)}
    | SetDefaultLlmRoleAction {roleId :: !(Id LlmAgentRole)}
    | DeleteLlmRoleAction {roleId :: !(Id LlmAgentRole)}
    | UpdateAutoAnalyzeAction
    | UpdateToolCacheAction
    deriving (Eq, Show)

data AssetsAdminController
    = AssetsAdminAction
    | NewAssetsConfigAction
    | CreateAssetsConfigAction
    | EditAssetsConfigAction {configId :: !(Id AssetsConfig)}
    | UpdateAssetsConfigAction {configId :: !(Id AssetsConfig)}
    | ToggleAssetsConfigAction {configId :: !(Id AssetsConfig)}
    | DeleteAssetsConfigAction {configId :: !(Id AssetsConfig)}
    | TestAssetsConnectionAction {configId :: !(Id AssetsConfig)}
    deriving (Eq, Show)

data AssetsIconsController
    = ShowAssetIconAction {objectId :: !(Id AssetsObject)}
    deriving (Eq, Show)

data AdminController
    = AdminAction
    | AdminRevokeApiTokenAction {apiTokenId :: !(Id ApiToken)}
    | AdminPurgeAlertsAction
    | AdminDatabaseAction
    | AdminDbAnalyzeAction
    | AdminDbVacuumAction
    | AdminDbAnalyzeTableAction {tableName :: !Text}
    | AdminExportProvisionAction
    deriving (Eq, Show, Data)

data AuditController
    = AuditExportsAction
    | ExportAuditAction
    deriving (Eq, Show, Data)

data FlappingController
    = FlappingAction
    deriving (Eq, Show, Data)

data ReportsController
    = ReportsAction
    deriving (Eq, Show, Data)

data PushSubscriptionsController
    = SubscribePushAction
    | UnsubscribePushAction
    deriving (Eq, Show)

data LiveController
    = LiveController
    deriving (Eq, Show, Data)

data ApiController
    = ApiAlertsAction
    | ApiAlertAction {alertId :: !(Id Alert)}
    | ApiEnvironmentsAction
    deriving (Eq, Show)

data MetricsController
    = MetricsAction
    deriving (Eq, Show)

data InternalApiController
    = InternalEnvironmentsAction
    | InternalDashboardsAction
    | InternalDashboardSchemaAction
    | InternalValidateDashboardAction
    | InternalCreateDashboardAction
    | InternalSearchAlertsAction
    | InternalLlmConfigAction
    deriving (Eq, Show)

data AgentChatController
    = ChatAction
    | AgentSessionsAction
    | AgentHistoryAction {sessionId :: !(Id AgentSession)}
    deriving (Eq, Show)

data GroupsController
    = ShowGroupAction {groupId :: !(Id AlertGroup)}
    | AckGroupAction {groupId :: !(Id AlertGroup)}
    deriving (Eq, Show)

data HooksController
    = HookAlertmanagerAction {token :: !Text}
    | HookGenericAction {token :: !Text}
    deriving (Eq, Show)

data SourcesController
    = SourcesAction
    | NewSourceAction
    | CreateSourceAction
    | EditSourceAction {sourceId :: !(Id Source)}
    | UpdateSourceAction {sourceId :: !(Id Source)}
    | ToggleSourceAction {sourceId :: !(Id Source)}
    | SyncHostGroupsAction {sourceId :: !(Id Source)}
    deriving (Eq, Show)

data TeamsController
    = TeamsAction
    | NewTeamAction
    | CreateTeamAction
    | EditTeamAction {teamId :: !(Id Team)}
    | UpdateTeamAction {teamId :: !(Id Team)}
    | DeleteTeamAction {teamId :: !(Id Team)}
    deriving (Eq, Show)

data FieldMappingsController
    = FieldMappingsAction
    | NewFieldMappingAction
    | CreateFieldMappingAction
    | EditFieldMappingAction {fieldMappingId :: !(Id FieldMapping)}
    | UpdateFieldMappingAction {fieldMappingId :: !(Id FieldMapping)}
    | DeleteFieldMappingAction {fieldMappingId :: !(Id FieldMapping)}
    | RecomputeFacetsAction
    deriving (Eq, Show)

data GroupingRulesController
    = GroupingRulesAction
    | NewGroupingRuleAction
    | CreateGroupingRuleAction
    | EditGroupingRuleAction {groupingRuleId :: !(Id GroupingRule)}
    | UpdateGroupingRuleAction {groupingRuleId :: !(Id GroupingRule)}
    | DeleteGroupingRuleAction {groupingRuleId :: !(Id GroupingRule)}
    | PreviewGroupingRuleAction {groupingRuleId :: !(Id GroupingRule)}
    deriving (Eq, Show)

data NotificationRulesController
    = NotificationRulesAction
    | NewNotificationRuleAction
    | CreateNotificationRuleAction
    | EditNotificationRuleAction {notificationRuleId :: !(Id NotificationRule)}
    | UpdateNotificationRuleAction {notificationRuleId :: !(Id NotificationRule)}
    | DeleteNotificationRuleAction {notificationRuleId :: !(Id NotificationRule)}
    deriving (Eq, Show)

data EscalationPoliciesController
    = EscalationPoliciesAction
    | NewEscalationPolicyAction
    | CreateEscalationPolicyAction
    | EditEscalationPolicyAction {escalationPolicyId :: !(Id EscalationPolicy)}
    | UpdateEscalationPolicyAction {escalationPolicyId :: !(Id EscalationPolicy)}
    | DeleteEscalationPolicyAction {escalationPolicyId :: !(Id EscalationPolicy)}
    deriving (Eq, Show)
