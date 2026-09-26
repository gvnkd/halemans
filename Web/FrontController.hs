module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports

import Web.Controller.Admin
import Web.Controller.AgentChat
import Web.Controller.Alerts
import Web.Controller.Api
import Web.Controller.AssetsAdmin
import Web.Controller.AssetsIcons
import Web.Controller.Audit
import Web.Controller.Blackouts
import Web.Controller.Dashboard
import Web.Controller.Dashboards
import Web.Controller.Environments
import Web.Controller.EscalationPolicies
import Web.Controller.FieldMappings
import Web.Controller.Flapping
import Web.Controller.GroupingRules
import Web.Controller.Groups
import Web.Controller.Hooks
import Web.Controller.Integrations
import Web.Controller.InternalApi
import Web.Controller.Live
import Web.Controller.LlmAdmin
import Web.Controller.Metrics
import Web.Controller.NotificationRules
import Web.Controller.Profile
import Web.Controller.PushSubscriptions
import Web.Controller.Reports
import Web.Controller.Roles
import Web.Controller.Sessions
import Web.Controller.Sources
import Web.Controller.Teams
import Web.Controller.Users

instance FrontController WebApplication where
    controllers =
        [ startPage DashboardAction
        , -- Generator Marker
          webSocketAppWithCustomPath @LiveController "ws"
        , parseRoute @SessionsController
        , parseRoute @DashboardController
        , parseRoute @EnvironmentsController
        , parseRoute @AlertsController
        , parseRoute @BlackoutsController
        , parseRoute @ProfileController
        , parseRoute @PushSubscriptionsController
        , parseRoute @HooksController
        , parseRoute @SourcesController
        , parseRoute @GroupsController
        , parseRoute @TeamsController
        , parseRoute @UsersController
        , parseRoute @RolesController
        , parseRoute @GroupingRulesController
        , parseRoute @FieldMappingsController
        , parseRoute @NotificationRulesController
        , parseRoute @EscalationPoliciesController
        , parseRoute @DashboardsController
        , parseRoute @IntegrationsController
        , parseRoute @LlmAdminController
        , parseRoute @AssetsAdminController
        , parseRoute @AssetsIconsController
        , parseRoute @AdminController
        , parseRoute @AuditController
        , parseRoute @FlappingController
        , parseRoute @ReportsController
        , parseRoute @ApiController
        , parseRoute @MetricsController
        , parseRoute @InternalApiController
        , parseRoute @AgentChatController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
