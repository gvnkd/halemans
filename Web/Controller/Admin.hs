module Web.Controller.Admin where

import Application.Service.DatabaseStats (analyzeDatabase, analyzeTable, fetchDatabaseStats, vacuumAnalyzeDatabase)
import Application.Service.JobMetrics (jobTypeMetrics, recentFailedJobs)
import Application.Service.ProvisionExport (buildProvisionExport)
import Control.Monad (void)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (getCurrentTime)
import qualified Data.Yaml as Yaml
import IHP.ControllerSupport (respondAndExit)
import IHP.ModelSupport (withTransaction)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import Web.Controller.Prelude
import Web.View.Admin.Database
import Web.View.Admin.Index

instance Controller AdminController where
    beforeAction = ensureIsUser

    action AdminAction = do
        requirePrivilege "admin"
        metrics <- jobTypeMetrics
        failures <- recentFailedJobs
        tokens <-
            query @ApiToken
                |> orderByDesc #createdAt
                |> fetch
        apiTokens <- forM tokens \token -> do
            owner <- fetch token.userId
            pure (token, owner.email)
        render IndexView{..}

    -- Admins can revoke any user's token (design_docs/milestone_6.md §4).
    action AdminRevokeApiTokenAction{apiTokenId} = do
        requirePrivilege "admin"
        token <- fetch apiTokenId
        now <- getCurrentTime
        when (isNothing token.revokedAt) do
            void (token |> set #revokedAt (Just now) |> updateRecord)
        redirectTo AdminAction

    -- Danger zone: wipes every alert with all dependent rows (events,
    -- comments, analyses, jobs) plus now-empty groups. FK order matters.
    action AdminPurgeAlertsAction = do
        requirePrivilege "admin"
        withTransaction do
            void $ sqlExecTyped [typedSql| DELETE FROM llm_feedback |]
            void $ sqlExecTyped [typedSql| DELETE FROM llm_analysis_jobs |]
            void $ sqlExecTyped [typedSql| DELETE FROM llm_analyses |]
            void $ sqlExecTyped [typedSql| DELETE FROM alert_events |]
            void $ sqlExecTyped [typedSql| DELETE FROM comments |]
            void $ sqlExecTyped [typedSql| DELETE FROM push_notification_jobs |]
            void $ sqlExecTyped [typedSql| DELETE FROM escalation_trackers |]
            void $ sqlExecTyped [typedSql| DELETE FROM jira_links |]
            void $ sqlExecTyped [typedSql| DELETE FROM write_back_jobs |]
            void $ sqlExecTyped [typedSql| DELETE FROM write_back_attempts |]
            void $ sqlExecTyped [typedSql| DELETE FROM enrich_alert_jobs |]
            void $ sqlExecTyped [typedSql| DELETE FROM alerts |]
            void $ sqlExecTyped [typedSql| DELETE FROM alert_groups |]
        setSuccessMessage (tr "All alerts purged")
        redirectTo AdminAction
    action AdminDatabaseAction = do
        requirePrivilege "admin"
        stats <- fetchDatabaseStats
        render DatabaseView{..}
    -- Provision config snapshot (Application.Service.ProvisionExport) in the
    -- map-keyed provision format; ?format=json selects JSON, default YAML.
    action AdminExportProvisionAction = do
        requirePrivilege "admin"
        config <- buildProvisionExport
        case paramOrNothing @Text "format" of
            Just "json" ->
                respondAndExit $
                    responseLBS
                        status200
                        [ ("Content-Type", "application/json; charset=utf-8")
                        , ("Content-Disposition", "attachment; filename=\"provision.json\"")
                        ]
                        (Aeson.encodePretty config)
            _ ->
                respondAndExit $
                    responseLBS
                        status200
                        [ ("Content-Type", "application/yaml; charset=utf-8")
                        , ("Content-Disposition", "attachment; filename=\"provision.yaml\"")
                        ]
                        (LBS.fromStrict (Yaml.encode config))
    action AdminDbAnalyzeAction = do
        requirePrivilege "admin"
        analyzeDatabase
        setSuccessMessage (tr "ANALYZE completed")
        redirectTo AdminDatabaseAction
    action AdminDbVacuumAction = do
        requirePrivilege "admin"
        vacuumAnalyzeDatabase
        setSuccessMessage (tr "VACUUM ANALYZE completed")
        redirectTo AdminDatabaseAction
    action AdminDbAnalyzeTableAction{tableName} = do
        requirePrivilege "admin"
        ok <- analyzeTable tableName
        if ok
            then setSuccessMessage (trp "ANALYZE {table} completed" [("table", tableName)])
            else setErrorMessage (trp "Unknown table: {table}" [("table", tableName)])
        redirectTo AdminDatabaseAction
