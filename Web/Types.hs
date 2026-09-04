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

data HooksController
    = HookAlertmanagerAction { token :: !Text }
    | HookGenericAction { token :: !Text }
    deriving (Eq, Show)

data SourcesController
    = SourcesAction
    deriving (Eq, Show)
