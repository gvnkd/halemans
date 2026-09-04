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
import Application.Job.EnrichAlert ()
import Application.Job.WriteBack ()
import Application.Job.JiraSync ()

instance Worker RootApplication where
    workers _ =
        [ worker @PollZabbixJob
        , worker @AutoCloseJob
        , worker @PushNotificationJob
        , worker @PollGrafanaJob
        , worker @EscalationJob
        , worker @EnrichAlertJob
        , worker @WriteBackJob
        , worker @JiraSyncJob
        -- Generator Marker
        ]
