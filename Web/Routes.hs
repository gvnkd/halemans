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
POST /alerts/{alertId}/assets/refresh   RefreshAssetsAction
POST /alerts/{alertId}/jira             CreateJiraTicketAction
POST /alerts/{alertId}/jira/{jiraLinkId}/delete  DeleteJiraLinkAction
POST /alerts/{alertId}/reanalyze           ReanalyzeAlertAction
POST /alerts/{alertId}/analyses/{analysisId}/feedback  LlmFeedbackAction
|]

[routes|LlmAdminController
GET  /admin/llm                                LlmAdminAction
GET  /admin/llm/queue                          LlmQueueAction
POST /admin/llm/queue/{analysisId}/drop        DropLlmAnalysisAction
GET  /admin/llm/templates/new                  NewLlmTemplateAction
POST /admin/llm/templates                      CreateLlmTemplateAction
GET  /admin/llm/templates/{templateId}/edit    EditLlmTemplateAction
POST /admin/llm/templates/{templateId}/update  UpdateLlmTemplateAction
POST /admin/llm/templates/{templateId}/activate  ActivateLlmTemplateAction
POST /admin/llm/templates/{templateId}/delete  DeleteLlmTemplateAction
POST /admin/llm/test                           TestLlmConnectionAction
GET  /admin/llm/providers/new                  NewLlmProviderAction
POST /admin/llm/providers                      CreateLlmProviderAction
GET  /admin/llm/providers/{providerId}/edit    EditLlmProviderAction
POST /admin/llm/providers/{providerId}/update  UpdateLlmProviderAction
POST /admin/llm/providers/{providerId}/enable  EnableLlmProviderAction
POST /admin/llm/providers/{providerId}/disable DisableLlmProviderAction
POST /admin/llm/providers/{providerId}/delete  DeleteLlmProviderAction
GET  /admin/llm/roles/new                    NewLlmRoleAction
POST /admin/llm/roles                        CreateLlmRoleAction
GET  /admin/llm/roles/{roleId}/edit          EditLlmRoleAction
POST /admin/llm/roles/{roleId}/update        UpdateLlmRoleAction
POST /admin/llm/roles/{roleId}/toggle        ToggleLlmRoleAction
POST /admin/llm/roles/{roleId}/default       SetDefaultLlmRoleAction
POST /admin/llm/roles/{roleId}/delete        DeleteLlmRoleAction
POST /admin/llm/auto-analyze                 UpdateAutoAnalyzeAction
POST /admin/llm/tool-cache                   UpdateToolCacheAction
|]

[routes|AssetsAdminController
GET  /admin/assets                            AssetsAdminAction
GET  /admin/assets/new                        NewAssetsConfigAction
POST /admin/assets                            CreateAssetsConfigAction
GET  /admin/assets/{configId}/edit            EditAssetsConfigAction
POST /admin/assets/{configId}/update          UpdateAssetsConfigAction
POST /admin/assets/{configId}/toggle          ToggleAssetsConfigAction
POST /admin/assets/{configId}/delete          DeleteAssetsConfigAction
POST /admin/assets/{configId}/test            TestAssetsConnectionAction
|]

[routes|AssetsIconsController
GET  /assets/objects/{objectId}/icon        ShowAssetIconAction
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
POST /profile/api-tokens                          CreateApiTokenAction
POST /profile/api-tokens/{apiTokenId}/revoke      RevokeApiTokenAction
|]

[routes|ApiController
GET /api/v1/alerts              ApiAlertsAction
GET /api/v1/alerts/{alertId}    ApiAlertAction
GET /api/v1/environments        ApiEnvironmentsAction
|]

[routes|MetricsController
GET /metrics    MetricsAction
|]

[routes|DashboardsController
GET  /dashboards                              DashboardsAction
GET  /dashboards/new                          NewDashboardAction
POST /dashboards                              CreateDashboardAction
GET  /dashboards/{dashboardId}                ShowDashboardAction
GET  /dashboards/{dashboardId}/cards/{cardIndex} ShowDashboardCardAction
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
GET  /admin/integrations/jira/new                        NewJiraConfigAction
POST /admin/integrations/jira                            CreateJiraConfigAction
GET  /admin/integrations/jira/{jiraConfigId}/edit        EditJiraConfigAction
POST /admin/integrations/jira/{jiraConfigId}/update      UpdateJiraConfigAction
POST /admin/integrations/jira/{jiraConfigId}/toggle      ToggleJiraConfigAction
POST /admin/integrations/jira/{jiraConfigId}/delete      DeleteJiraConfigAction
POST /admin/integrations/jira/{jiraConfigId}/test        TestJiraConfigAction
GET  /admin/integrations/cmdb/new                        NewCmdbConfigAction
POST /admin/integrations/cmdb                            CreateCmdbConfigAction
GET  /admin/integrations/cmdb/{cmdbConfigId}/edit        EditCmdbConfigAction
POST /admin/integrations/cmdb/{cmdbConfigId}/update      UpdateCmdbConfigAction
POST /admin/integrations/cmdb/{cmdbConfigId}/toggle      ToggleCmdbConfigAction
POST /admin/integrations/cmdb/{cmdbConfigId}/delete      DeleteCmdbConfigAction
POST /admin/integrations/cmdb/{cmdbConfigId}/test        TestCmdbConfigAction
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

[routes|FieldMappingsController
GET  /admin/field-mappings                                FieldMappingsAction
GET  /admin/field-mappings/new                            NewFieldMappingAction
POST /admin/field-mappings                                CreateFieldMappingAction
GET  /admin/field-mappings/{fieldMappingId}/edit          EditFieldMappingAction
POST /admin/field-mappings/{fieldMappingId}/update        UpdateFieldMappingAction
POST /admin/field-mappings/{fieldMappingId}/delete        DeleteFieldMappingAction
POST /admin/field-mappings/recompute                      RecomputeFacetsAction
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
POST /sources/{sourceId}/sync-host-groups  SyncHostGroupsAction
|]

[routes|AdminController
GET  /admin    AdminAction
POST /admin/api-tokens/{apiTokenId}/revoke    AdminRevokeApiTokenAction
POST /admin/purge-alerts    AdminPurgeAlertsAction
|]

[routes|AuditController
GET  /admin/audit           AuditExportsAction
GET  /admin/audit/export    ExportAuditAction
|]
