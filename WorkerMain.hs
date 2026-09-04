module WorkerMain () where

import IHP.Prelude
import IHP.FrameworkConfig (RootApplication (..))
import IHP.Job.Types (Worker (..))
import IHP.Job.Runner (worker)
import Generated.Types
import Application.Job.PollZabbix ()
import Application.Job.AutoClose ()
import Application.Job.PushNotification ()
import Application.Job.PollGrafana ()
import Application.Job.Escalation ()

instance Worker RootApplication where
    workers _ =
        [ worker @PollZabbixJob
        , worker @AutoCloseJob
        , worker @PushNotificationJob
        , worker @PollGrafanaJob
        , worker @EscalationJob
        -- Generator Marker
        ]
