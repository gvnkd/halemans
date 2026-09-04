module WorkerMain () where

import IHP.Prelude
import IHP.FrameworkConfig (RootApplication (..))
import IHP.Job.Types (Worker (..))
import IHP.Job.Runner (worker)
import Generated.Types
import Application.Job.PollZabbix ()
import Application.Job.AutoClose ()
import Application.Job.PushNotification ()

instance Worker RootApplication where
    workers _ =
        [ worker @PollZabbixJob
        , worker @AutoCloseJob
        , worker @PushNotificationJob
        -- Generator Marker
        ]
