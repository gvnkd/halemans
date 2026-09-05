module Web.Controller.Admin where

import Web.Controller.Prelude
import Web.View.Admin.Index
import Application.Service.JobMetrics (jobTypeMetrics, recentFailedJobs)

instance Controller AdminController where
    beforeAction = ensureIsUser

    action AdminAction = do
        requirePrivilege "admin"
        metrics <- jobTypeMetrics
        failures <- recentFailedJobs
        render IndexView { .. }
