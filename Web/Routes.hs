module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

instance AutoRoute SessionsController

[routes|DashboardController
GET /    DashboardAction
|]

[routes|EnvironmentsController
GET /env/{environmentName}    ShowEnvironmentAction
|]

[routes|AlertsController
GET  /alerts                            AlertsAction
GET  /alerts/{alertId}                  ShowAlertAction
POST /alerts/{alertId}/ack              AckAlertAction
POST /alerts/{alertId}/unack            UnackAlertAction
POST /alerts/{alertId}/close            CloseAlertAction
POST /alerts/{alertId}/comments         CreateCommentAction
POST /alerts/{alertId}/cmdb/refresh     RefreshCmdbAction
POST /alerts/{alertId}/jira             CreateJiraTicketAction
POST /alerts/{alertId}/jira/{jiraLinkId}/delete  DeleteJiraLinkAction
POST /alerts/{alertId}/reanalyze           ReanalyzeAlertAction
POST /alerts/{alertId}/analyses/{analysisId}/feedback  LlmFeedbackAction
|]

[routes|LlmAdminController
GET  /admin/llm                                LlmAdminAction
GET  /admin/llm/templates/{templateId}/edit    EditLlmTemplateAction
POST /admin/llm/templates/{templateId}/update  UpdateLlmTemplateAction
POST /admin/llm/templates/{templateId}/activate  ActivateLlmTemplateAction
POST /admin/llm/test                           TestLlmConnectionAction
|]

[routes|BlackoutsController
GET  /blackouts                      BlackoutsAction
GET  /blackouts/new                  NewBlackoutAction
POST /blackouts                      CreateBlackoutAction
GET  /blackouts/{blackoutId}/edit    EditBlackoutAction
POST /blackouts/{blackoutId}/update  UpdateBlackoutAction
POST /blackouts/{blackoutId}/delete  DeleteBlackoutAction
|]

[routes|ProfileController
GET  /profile         ProfileAction
POST /profile/theme   UpdateThemeAction
|]

[routes|DashboardsController
GET  /dashboards                              DashboardsAction
GET  /dashboards/new                          NewDashboardAction
POST /dashboards                              CreateDashboardAction
GET  /dashboards/{dashboardId}                ShowDashboardAction
GET  /dashboards/{dashboardId}/edit           EditDashboardAction
POST /dashboards/{dashboardId}/update         UpdateDashboardAction
POST /dashboards/{dashboardId}/delete         DeleteDashboardAction
POST /dashboards/{dashboardId}/default        SetDefaultDashboardAction
POST /dashboards/{dashboardId}/move           MoveDashboardAction
|]

[routes|IntegrationsController
GET  /admin/integrations                      IntegrationsAction
POST /admin/integrations/test-confluence      TestConfluenceAction
POST /admin/integrations/test-jira            TestJiraAction
|]

[routes|PushSubscriptionsController
POST   /api/push/subscribe    SubscribePushAction
DELETE /api/push/subscribe    UnsubscribePushAction
|]

[routes|HooksController
POST /hooks/alertmanager/{token}    HookAlertmanagerAction
POST /hooks/generic/{token}         HookGenericAction
|]

[routes|GroupsController
GET  /groups/{groupId}         ShowGroupAction
POST /groups/{groupId}/ack     AckGroupAction
|]

[routes|TeamsController
GET  /admin/teams                     TeamsAction
GET  /admin/teams/new                 NewTeamAction
POST /admin/teams                     CreateTeamAction
GET  /admin/teams/{teamId}/edit       EditTeamAction
POST /admin/teams/{teamId}/update     UpdateTeamAction
POST /admin/teams/{teamId}/delete     DeleteTeamAction
|]

[routes|GroupingRulesController
GET  /admin/grouping-rules                            GroupingRulesAction
GET  /admin/grouping-rules/new                        NewGroupingRuleAction
POST /admin/grouping-rules                            CreateGroupingRuleAction
GET  /admin/grouping-rules/{groupingRuleId}/edit      EditGroupingRuleAction
POST /admin/grouping-rules/{groupingRuleId}/update    UpdateGroupingRuleAction
POST /admin/grouping-rules/{groupingRuleId}/delete    DeleteGroupingRuleAction
GET  /admin/grouping-rules/{groupingRuleId}/preview   PreviewGroupingRuleAction
|]

[routes|NotificationRulesController
GET  /admin/notification-rules                            NotificationRulesAction
GET  /admin/notification-rules/new                        NewNotificationRuleAction
POST /admin/notification-rules                            CreateNotificationRuleAction
GET  /admin/notification-rules/{notificationRuleId}/edit      EditNotificationRuleAction
POST /admin/notification-rules/{notificationRuleId}/update    UpdateNotificationRuleAction
POST /admin/notification-rules/{notificationRuleId}/delete    DeleteNotificationRuleAction
|]

[routes|EscalationPoliciesController
GET  /admin/escalation-policies                            EscalationPoliciesAction
GET  /admin/escalation-policies/new                        NewEscalationPolicyAction
POST /admin/escalation-policies                            CreateEscalationPolicyAction
GET  /admin/escalation-policies/{escalationPolicyId}/edit      EditEscalationPolicyAction
POST /admin/escalation-policies/{escalationPolicyId}/update    UpdateEscalationPolicyAction
POST /admin/escalation-policies/{escalationPolicyId}/delete    DeleteEscalationPolicyAction
|]

[routes|SourcesController
GET  /sources                       SourcesAction
GET  /sources/new                   NewSourceAction
POST /sources                       CreateSourceAction
GET  /sources/{sourceId}/edit       EditSourceAction
POST /sources/{sourceId}/update     UpdateSourceAction
POST /sources/{sourceId}/toggle     ToggleSourceAction
|]

[routes|AdminController
GET  /admin    AdminAction
|]

[routes|AuditController
GET  /admin/audit           AuditExportsAction
GET  /admin/audit/export    ExportAuditAction
|]
