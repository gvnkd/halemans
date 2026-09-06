module Application.Service.Llm.DbConfig
( llmConfigFromDb
, currentLlmConfig
) where

import IHP.Prelude
import IHP.ModelSupport (ModelContext)
import IHP.QueryBuilder (query, filterWhere)
import IHP.Fetch (fetchOneOrNothing)
import Generated.Types (LlmConfig, LlmConfig' (..))
import Application.Service.Llm (LlmProviderConfig (..), llmConfigFromEnv)
import System.Environment (lookupEnv)

-- DB-first LLM config resolution (design_docs/milestone_7.md §7 D7). Lives in
-- its own module because the generated LlmConfig record shares field names
-- with LlmProviderConfig (selector shadowing makes them unusable together in
-- Application.Service.Llm itself).

-- Enabled llm_configs row, if any. api_key_env holds the env var NAME and is
-- resolved with lookupEnv at read time.
llmConfigFromDb :: (?modelContext :: ModelContext) => IO (Maybe LlmProviderConfig)
llmConfigFromDb = do
    maybeRow <- query @LlmConfig
        |> filterWhere (#enabled, True)
        |> fetchOneOrNothing
    case maybeRow of
        Nothing -> pure Nothing
        Just row -> do
            apiKey <- maybe (pure Nothing) (lookupEnv . cs) (get #apiKeyEnv row)
            pure (Just LlmProviderConfig
                { providerName = get #providerName row
                , endpoint = get #endpoint row
                , model = get #model row
                , apiKey = cs <$> apiKey
                , toolsEnabled = get #toolsEnabled row
                })

-- Single resolution point for all callers: enabled DB row wins, env is the
-- fallback so M4–M6 dev/test paths behave exactly as before.
currentLlmConfig :: (?modelContext :: ModelContext) => IO (Maybe LlmProviderConfig)
currentLlmConfig = llmConfigFromDb >>= maybe llmConfigFromEnv (pure . Just)
