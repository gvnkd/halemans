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
import Application.Job.LlmAnalysis ()
import Application.Job.Retention ()
import Application.Job.SourceHealth ()
import Application.Job.FacetBackfill ()

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
        , worker @LlmAnalysisJob
        , worker @RetentionJob
        , worker @SourceHealthJob
        , worker @FacetBackfillJob
        -- Generator Marker
        ]
