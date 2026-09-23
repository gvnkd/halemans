module Application.Service.Llm.AgentConfig (
    AgentBudgetConfig (..),
    defaultAgentBudgetConfig,
    agentBudgetConfig,
    saveAgentBudgetConfig,
) where

import Control.Monad (void)
import Generated.Types
import IHP.Fetch (fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (query)

-- Agent chat budget (internal API milestone). Singleton-by-convention like
-- llm_tool_cache_configs: no row = defaultAgentBudgetConfig. Configured on
-- the admin/LLM page; consumed by Application.Service.Agent.Core (daily
-- token cap + per-minute request rate, both scoped to the agent only).

data AgentBudgetConfig = AgentBudgetConfig
    { abcDailyTokenBudget :: Int
    , abcRatePerMinute :: Int
    }
    deriving (Eq, Show)

defaultAgentBudgetConfig :: AgentBudgetConfig
defaultAgentBudgetConfig = AgentBudgetConfig{abcDailyTokenBudget = 200000, abcRatePerMinute = 12}

agentBudgetConfig :: (?modelContext :: ModelContext) => IO AgentBudgetConfig
agentBudgetConfig = do
    row <- query @LlmAgentConfig |> fetchOneOrNothing
    pure case row of
        Nothing -> defaultAgentBudgetConfig
        Just row ->
            AgentBudgetConfig
                { abcDailyTokenBudget = row.dailyTokenBudget
                , abcRatePerMinute = row.ratePerMinute
                }

-- Singleton upsert (single row, id fixed by first insert).
saveAgentBudgetConfig :: (?modelContext :: ModelContext) => AgentBudgetConfig -> IO ()
saveAgentBudgetConfig config = do
    row <- query @LlmAgentConfig |> fetchOneOrNothing
    case row of
        Nothing -> void do
            newRecord @LlmAgentConfig
                |> set #dailyTokenBudget config.abcDailyTokenBudget
                |> set #ratePerMinute config.abcRatePerMinute
                |> createRecord
        Just row -> void do
            row
                |> set #dailyTokenBudget config.abcDailyTokenBudget
                |> set #ratePerMinute config.abcRatePerMinute
                |> updateRecord
