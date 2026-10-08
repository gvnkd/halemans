module Web.Controller.Admin where

import Application.Service.DatabaseStats (analyzeDatabase, analyzeTable, fetchDatabaseStats, vacuumAnalyzeDatabase)
import Application.Service.JobMetrics (jobTypeMetrics, recentFailedJobs)
import Application.Service.Mattermost (PurgeMattermostSummary (..), purgeResolvedMattermostPosts)
import Application.Service.ProvisionExport (buildProvisionExport, renderProvisionJson, renderProvisionYaml)
import Application.Service.PurgeAlerts (purgeAllAlerts)
import Control.Monad (void)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import IHP.ControllerSupport (respondAndExit)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import System.IO (hFlush, stdout)
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
    -- comments, analyses, jobs, asset links) plus now-empty groups.
    action AdminPurgeAlertsAction = do
        requirePrivilege "admin"
        purgeAllAlerts
        setSuccessMessage (tr "All alerts purged")
        redirectTo AdminAction
    -- Danger zone: CHANNEL-FIRST cleanup of the bot's root posts of
    -- resolved/closed alerts — every mattermost rule's target channel is
    -- listed and only existing bot root posts mapped to a terminal alert are
    -- deleted (active alerts' posts are kept). Retroactive, independent of
    -- the per-channel "deleteOnClose" flag.
    action AdminPurgeResolvedMattermostAction = do
        requirePrivilege "admin"
        summary <- purgeResolvedMattermostPosts
        -- The first reasons go straight into the flash — operators must not
        -- need log access to see WHY a target was skipped.
        let reasons = Text.intercalate " | " (take 2 summary.pmsErrors)
            detail = if Text.null reasons then "" else " — " <> reasons
        setSuccessMessage
            ( ( trp
                    "Mattermost purge: {purged} deleted, {failed} failed, {untracked} untracked left, {kept} active kept, {targets} targets failed"
                    [ ("purged", tshow summary.pmsPurged)
                    , ("failed", tshow summary.pmsFailed)
                    , ("untracked", tshow summary.pmsUntracked)
                    , ("kept", tshow summary.pmsKeptActive)
                    , ("targets", tshow summary.pmsTargetsFailed)
                    ]
              )
                <> detail
            )
        -- Full skip reasons also go to the app log (stdout is block-buffered
        -- under docker, hence the explicit flush).
        mapM_ (\err -> putStrLn ("mattermost purge: " <> err)) (take 10 summary.pmsErrors)
        hFlush stdout
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
                        (renderProvisionJson config)
            _ ->
                respondAndExit $
                    responseLBS
                        status200
                        [ ("Content-Type", "application/yaml; charset=utf-8")
                        , ("Content-Disposition", "attachment; filename=\"provision.yaml\"")
                        ]
                        (LBS.fromStrict (renderProvisionYaml config))
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
