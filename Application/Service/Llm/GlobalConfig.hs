module Application.Service.Llm.GlobalConfig (
    GlobalBudgetConfig (..),
    globalBudgetConfig,
    saveGlobalBudgetConfig,
) where

import qualified Application.Service.Llm.Budget as Budget
import Control.Monad (void)
import Generated.Types
import IHP.Fetch (fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (query)

-- Global LLM budget (agent configuration milestone). Caps ALL consumers
-- (analysis pipeline + chat agent), each of which additionally enforces its
-- own per-consumer cap. Singleton-by-convention: no row = env fallbacks
-- (LLM_DAILY_TOKEN_BUDGET / LLM_RATE_PER_MINUTE), so existing deployments
-- behave exactly as before until an administrator saves a row.

data GlobalBudgetConfig = GlobalBudgetConfig
    { gbcDailyTokenBudget :: Int
    , gbcRatePerMinute :: Int
    }
    deriving (Eq, Show)

globalBudgetConfig :: (?modelContext :: ModelContext) => IO GlobalBudgetConfig
globalBudgetConfig = do
    envFallback <- Budget.dailyTokenBudget
    envRate <- Budget.rateLimitPerMinute
    row <- query @LlmGlobalConfig |> fetchOneOrNothing
    pure case row of
        Nothing -> GlobalBudgetConfig{gbcDailyTokenBudget = envFallback, gbcRatePerMinute = envRate}
        Just row ->
            GlobalBudgetConfig
                { gbcDailyTokenBudget = row.dailyTokenBudget
                , gbcRatePerMinute = row.ratePerMinute
                }

saveGlobalBudgetConfig :: (?modelContext :: ModelContext) => GlobalBudgetConfig -> IO ()
saveGlobalBudgetConfig config = do
    row <- query @LlmGlobalConfig |> fetchOneOrNothing
    case row of
        Nothing -> void do
            newRecord @LlmGlobalConfig
                |> set #dailyTokenBudget config.gbcDailyTokenBudget
                |> set #ratePerMinute config.gbcRatePerMinute
                |> createRecord
        Just row -> void do
            row
                |> set #dailyTokenBudget config.gbcDailyTokenBudget
                |> set #ratePerMinute config.gbcRatePerMinute
                |> updateRecord
