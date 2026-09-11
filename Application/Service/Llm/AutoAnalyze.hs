module Application.Service.Llm.AutoAnalyze
( AutoAnalyzeRules (..)
, defaultRules
, rulesFromRow
, currentRules
, autoAnalyzeAllowed
, allowedByRules
, allStatuses
, allSeverities
) where

import IHP.Prelude
import IHP.ModelSupport (ModelContext)
import IHP.QueryBuilder (query)
import IHP.Fetch (fetch)
import Generated.Types (Alert, Alert' (..), LlmAutoAnalyzeConfig, LlmAutoAnalyzeConfig' (..))
import Data.Aeson (Value)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson

-- Auto-analysis gate (milestone 10 §5): which alert statuses/severities get
-- an LLM analysis enqueued automatically (ingest of new alerts + the
-- enrichment retrigger). Manual re-analyze from the card is never gated.
-- No llm_auto_analyze_configs row = defaultRules, so pre-existing installs
-- keep analyzing every new alert.

data AutoAnalyzeRules = AutoAnalyzeRules
    { aaEnabled :: Bool
    , aaStatuses :: [Text]
    , aaSeverities :: [Text]
    } deriving (Eq, Show)

defaultRules :: AutoAnalyzeRules
defaultRules = AutoAnalyzeRules
    { aaEnabled = True
    , aaStatuses = ["firing", "ack"]
    , aaSeverities = allSeverities
    }

allStatuses :: [Text]
allStatuses = ["firing", "ack", "stalled", "resolved"]

allSeverities :: [Text]
allSeverities = ["critical", "high", "warning", "info"]

rulesFromRow :: LlmAutoAnalyzeConfig -> AutoAnalyzeRules
rulesFromRow row = AutoAnalyzeRules
    { aaEnabled = row.enabled
    , aaStatuses = stringList row.statuses
    , aaSeverities = stringList row.severities
    }

stringList :: Value -> [Text]
stringList value = fromMaybe [] (parseMaybe Aeson.parseJSON value)

currentRules :: (?modelContext :: ModelContext) => IO AutoAnalyzeRules
currentRules = do
    rows <- query @LlmAutoAnalyzeConfig |> fetch
    pure (maybe defaultRules rulesFromRow (head rows))

autoAnalyzeAllowed :: (?modelContext :: ModelContext) => Alert -> IO Bool
autoAnalyzeAllowed alert = do
    rules <- currentRules
    pure (allowedByRules rules alert.status alert.severity)

allowedByRules :: AutoAnalyzeRules -> Text -> Text -> Bool
allowedByRules rules status severity =
    rules.aaEnabled
        && status `elem` rules.aaStatuses
        && severity `elem` rules.aaSeverities
