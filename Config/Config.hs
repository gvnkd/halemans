module Config (config) where

import Application.Helper.Controller ()
import Application.Service.Defaults (ensureDefaults, ensureWebhookTokens)
import Application.Service.Log (LogLevel (..))
import Application.Service.Provision (applyProvisionConfig)
import Application.Service.SecurityHeaders (securityHeaders)
import Application.Service.Turbolinks (turbolinksRedirectLocation)
import Control.Monad.IO.Class (liftIO)
import Generated.Types (User)
import IHP.EnvVar (envOrDefault)
import IHP.Environment
import IHP.FrameworkConfig
import IHP.LoginSupport.Middleware
import IHP.ModelSupport (noopLogger, withModelContext)
import IHP.Prelude
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

config :: ConfigBuilder
config = do
    -- See https://ihp.digitallyinduced.com/Guide/config.html
    -- for what you can do here
    option $ AuthMiddleware (authMiddleware @User)
    option $ CustomMiddleware turbolinksRedirectLocation
    option $ CustomMiddleware securityHeaders
    configIO bootProvisioning

    -- App log verbosity (Application.Service.Log): HALEMANS_LOG_LEVEL is
    -- debug|info|warn|error (default info). Validated here so a bad value
    -- aborts startup instead of silently degrading to info.
    _ <- envOrDefault "HALEMANS_LOG_LEVEL" LogInfo
    -- HALEMANS_ACCESS_LOG=0 disables the wai request logger. option is
    -- first-wins against ihpDefaultConfig, so installing identity here
    -- suppresses access logging entirely.
    accessLog <- envOrDefault "HALEMANS_ACCESS_LOG" True
    unless accessLog $ option $ RequestLoggerMiddleware id

-- Boot provisioning, two steps: (1) unconditional INSERT-if-missing of the
-- built-in default rows (Application.Service.Defaults) so long-lived DBs get
-- the rows fresh DBs only got via Fixtures.sql / seed scripts; (2) the
-- env-gated provision-from-file. The ConfigBuilder runs in IO and is
-- evaluated by RunProdServer, RunJobs and the dev server alike (milestone 7,
-- design_docs/milestone_7.md §3), so this one hook covers every process.
-- The builder runs before ihpDefaultConfig fills in DatabaseUrl, so the hook
-- resolves defaultDatabaseUrl itself and opens a short-lived pool.
bootProvisioning :: IO ()
bootProvisioning = do
    ensureDefaultsAtBoot
    provisionAtBoot

-- A defaults failure must not abort startup (unlike a provision-file error,
-- which is deliberate): every consumer degrades gracefully without the rows
-- (Mattermost/internal-agent render built-in bodies; LLM enrichment fails
-- per-analysis, not at boot).
ensureDefaultsAtBoot :: IO ()
ensureDefaultsAtBoot =
    withDefaultsPool `catch` \(e :: SomeException) ->
        hPutStrLn stderr (cs ("warning: ensure-defaults failed (skipped): " <> tshow e) :: String)

withDefaultsPool :: IO ()
withDefaultsPool = do
    databaseUrl <- defaultDatabaseUrl
    withModelContext databaseUrl noopLogger \modelContext -> do
        let ?modelContext = modelContext
        ensureDefaults
        ensureWebhookTokens

-- Unset/empty path = no-op.
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
