module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- The welcome page at '/' is served by the static controller below.
-- Additional [routes|...|] blocks get appended by `new-controller`.
[routes|StaticController
GET /    WelcomeAction
|]

[routes|AlertsController
GET /alerts              AlertsAction
GET /alerts/{alertId}    ShowAlertAction
|]

[routes|HooksController
POST /hooks/alertmanager/{token}    HookAlertmanagerAction
POST /hooks/generic/{token}         HookGenericAction
|]

[routes|SourcesController
GET /sources    SourcesAction
|]
