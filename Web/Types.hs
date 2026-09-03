module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data AlertsController
    = AlertsAction
    | ShowAlertAction { alertId :: !(Id Alert) }
    deriving (Eq, Show)

data HooksController
    = HookAlertmanagerAction { token :: !Text }
    | HookGenericAction { token :: !Text }
    deriving (Eq, Show)

data SourcesController
    = SourcesAction
    deriving (Eq, Show)
