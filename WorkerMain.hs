module WorkerMain () where

import IHP.Prelude
import IHP.FrameworkConfig (RootApplication (..))
import IHP.Job.Types (Worker (..))
import IHP.Job.Runner (worker)
import Generated.Types
import Application.Job.PollZabbix ()

instance Worker RootApplication where
    workers _ =
        [ worker @PollZabbixJob
        -- Generator Marker
        ]
