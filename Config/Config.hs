module Config (config) where

import Application.Helper.Controller ()
import Application.Service.Log (LogLevel (..))
import Application.Service.Provision (applyProvisionConfig)
import Application.Service.SecurityHeaders (securityHeaders)
import Control.Monad.IO.Class (liftIO)
import Generated.Types (User)
import IHP.EnvVar (envOrDefault)
import IHP.Environment
import IHP.FrameworkConfig
import IHP.LoginSupport.Middleware
import IHP.ModelSupport (noopLogger, withModelContext)
import IHP.Prelude
import System.Environment (lookupEnv)

config :: ConfigBuilder
config = do
    -- See https://ihp.digitallyinduced.com/Guide/config.html
    -- for what you can do here
    option $ AuthMiddleware (authMiddleware @User)
    option $ CustomMiddleware securityHeaders
    configIO provisionAtBoot

    -- App log verbosity (Application.Service.Log): HALEMANS_LOG_LEVEL is
    -- debug|info|warn|error (default info). Validated here so a bad value
    -- aborts startup instead of silently degrading to info.
    _ <- envOrDefault "HALEMANS_LOG_LEVEL" LogInfo
    -- HALEMANS_ACCESS_LOG=0 disables the wai request logger. option is
    -- first-wins against ihpDefaultConfig, so installing identity here
    -- suppresses access logging entirely.
    accessLog <- envOrDefault "HALEMANS_ACCESS_LOG" True
    unless accessLog $ option $ RequestLoggerMiddleware id

-- Milestone 7 (D2, design_docs/milestone_7.md §3): the ConfigBuilder runs in
-- IO and is evaluated by RunProdServer, RunJobs and the dev server alike, so
-- this one hook provisions every process. Unset/empty path = no-op. The
-- builder runs before ihpDefaultConfig fills in DatabaseUrl, so the hook
-- resolves defaultDatabaseUrl itself and opens a short-lived pool.
provisionAtBoot :: IO ()
provisionAtBoot = do
    path <- lookupEnv "HALEMANS_PROVISION_CONFIG"
    case path of
        Just configPath | not (null configPath) -> do
            databaseUrl <- defaultDatabaseUrl
            withModelContext databaseUrl noopLogger \modelContext -> do
                let ?modelContext = modelContext
                applyProvisionConfig configPath
        _ -> pure ()
