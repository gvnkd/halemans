module Web.Controller.Admin where

import Web.Controller.Prelude
import Web.View.Admin.Index
import Application.Service.JobMetrics (jobTypeMetrics, recentFailedJobs)
import Data.Time.Clock (getCurrentTime)
import Control.Monad (void)

instance Controller AdminController where
    beforeAction = ensureIsUser

    action AdminAction = do
        requirePrivilege "admin"
        metrics <- jobTypeMetrics
        failures <- recentFailedJobs
        tokens <- query @ApiToken
            |> orderByDesc #createdAt
            |> fetch
        apiTokens <- forM tokens \token -> do
            owner <- fetch token.userId
            pure (token, owner.email)
        render IndexView { .. }

    -- Admins can revoke any user's token (design_docs/milestone_6.md §4).
    action AdminRevokeApiTokenAction { apiTokenId } = do
        requirePrivilege "admin"
        token <- fetch apiTokenId
        now <- getCurrentTime
        when (isNothing token.revokedAt) do
            void (token |> set #revokedAt (Just now) |> updateRecord)
        redirectTo AdminAction
