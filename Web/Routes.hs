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
|]

[routes|BlackoutsController
GET  /blackouts                      BlackoutsAction
GET  /blackouts/new                  NewBlackoutAction
POST /blackouts                      CreateBlackoutAction
POST /blackouts/{blackoutId}/delete  DeleteBlackoutAction
|]

[routes|ProfileController
GET /profile    ProfileAction
|]

[routes|PushSubscriptionsController
POST   /api/push/subscribe    SubscribePushAction
DELETE /api/push/subscribe    UnsubscribePushAction
|]

[routes|HooksController
POST /hooks/alertmanager/{token}    HookAlertmanagerAction
POST /hooks/generic/{token}         HookGenericAction
|]

[routes|SourcesController
GET /sources    SourcesAction
|]
