module Config (config) where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.LoginSupport.Middleware
import IHP.ModelSupport (withModelContext, noopLogger)
import Control.Monad.IO.Class (liftIO)
import System.Environment (lookupEnv)
import Generated.Types (User)
import Application.Helper.Controller ()
import Application.Service.Provision (applyProvisionConfig)

config :: ConfigBuilder
config = do
    -- See https://ihp.digitallyinduced.com/Guide/config.html
    -- for what you can do here
    option $ AuthMiddleware (authMiddleware @User)
    configIO provisionAtBoot

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
