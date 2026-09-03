module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Static
import Web.Controller.Alerts
import Web.Controller.Hooks
import Web.Controller.Sources

instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction
        -- Generator Marker
        , parseRoute @AlertsController
        , parseRoute @HooksController
        , parseRoute @SourcesController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
