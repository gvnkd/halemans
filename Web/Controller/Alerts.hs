module Web.Controller.Alerts where

import Web.Controller.Prelude
import Web.View.Alerts.Index
import Web.View.Alerts.Show

instance Controller AlertsController where
    action AlertsAction = do
        alerts <- query @Alert
            |> orderByDesc #createdAt
            |> limit 100
            |> fetch
        render IndexView { .. }

    action ShowAlertAction { alertId } = do
        alert <- fetch alertId
        render ShowView { .. }
