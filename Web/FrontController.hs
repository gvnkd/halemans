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
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
