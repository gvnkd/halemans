module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Dashboard
import Web.Controller.Alerts
import Web.Controller.Hooks
import Web.Controller.Sources
import Web.Controller.Sessions
import Web.Controller.Environments
import Web.Controller.Blackouts
import Web.Controller.Profile
import Web.Controller.PushSubscriptions
import Web.Controller.Live
import Web.Controller.Groups
import Web.Controller.Teams
import Web.Controller.GroupingRules
import Web.Controller.FieldMappings
import Web.Controller.NotificationRules
import Web.Controller.EscalationPolicies
import Web.Controller.Dashboards
import Web.Controller.Integrations
import Web.Controller.LlmAdmin
import Web.Controller.AssetsAdmin
import Web.Controller.AssetsIcons
import Web.Controller.Admin
import Web.Controller.Audit
import Web.Controller.Flapping
import Web.Controller.Api
import Web.Controller.Metrics

instance FrontController WebApplication where
    controllers =
        [ startPage DashboardAction
        -- Generator Marker
        , webSocketAppWithCustomPath @LiveController "ws"
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
        , parseRoute @ApiController
        , parseRoute @MetricsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
