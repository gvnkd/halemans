module Application.Service.Log (
    LogLevel (..),
    parseLogLevel,
    logDebug,
    logInfo,
    logWarn,
    logError,
) where

import qualified Data.Text as Text
import IHP.EnvVar (EnvVarReader (..))
import IHP.Prelude
import System.Environment (lookupEnv)
import System.Log.FastLogger (FastLogger, toLogStr)

-- App log levels for HALEMANS_LOG_LEVEL (debug|info|warn|error, default
-- info). The level is read per call (like the API rate limits) so web,
-- worker and scripts all pick it up without plumbing; Config.hs validates
-- the value once at boot so a typo fails fast instead of silently logging
-- at info.
data LogLevel = LogDebug | LogInfo | LogWarn | LogError
    deriving (Eq, Ord, Show)

instance EnvVarReader LogLevel where
    envStringToValue value =
        maybe (Left "Expected one of: debug, info, warn, error") Right (parseLogLevel (cs value))

parseLogLevel :: Text -> Maybe LogLevel
parseLogLevel value = case Text.toLower (Text.strip value) of
    "debug" -> Just LogDebug
    "info" -> Just LogInfo
    "warn" -> Just LogWarn
    "error" -> Just LogError
    _ -> Nothing

logDebug :: (?context :: context, HasField "logger" context FastLogger) => Text -> IO ()
logDebug = logAt LogDebug

logInfo :: (?context :: context, HasField "logger" context FastLogger) => Text -> IO ()
logInfo = logAt LogInfo

logWarn :: (?context :: context, HasField "logger" context FastLogger) => Text -> IO ()
logWarn = logAt LogWarn

logError :: (?context :: context, HasField "logger" context FastLogger) => Text -> IO ()
logError = logAt LogError

logAt :: (?context :: context, HasField "logger" context FastLogger) => LogLevel -> Text -> IO ()
logAt level message = do
    minLevel <- currentMinLogLevel
    when (level >= minLevel) do
        ?context.logger (toLogStr ("[" <> levelName level <> "] " <> message))

levelName :: LogLevel -> Text
levelName = \case
    LogDebug -> "debug"
    LogInfo -> "info"
    LogWarn -> "warn"
    LogError -> "error"

currentMinLogLevel :: IO LogLevel
currentMinLogLevel = do
    raw <- lookupEnv "HALEMANS_LOG_LEVEL"
    pure case raw of
        Just value | Just level <- parseLogLevel (cs value) -> level
        _ -> LogInfo
