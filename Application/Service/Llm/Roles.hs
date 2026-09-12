module Application.Service.Llm.Roles (
    resolveAgentRole,
    resolveAgentRoleByName,
    roleToolNames,
    templateNameForRole,
    toolsForRole,
) where

import Application.Service.Llm.Tools (toolDefinitions)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Vector as Vector
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

-- Agent roles (design_docs/milestone_8.md §7): a role bundles a prompt
-- template name + tool whitelist. Resolution is DB-first like llm_configs
-- (DbConfig pattern): an explicit role id on the analysis wins, otherwise
-- the is_default role applies, otherwise no role → legacy behaviour
-- (alert_enrichment template, full built-in tool set when tools enabled).

resolveAgentRole :: (?modelContext :: ModelContext) => Maybe (Id LlmAgentRole) -> IO (Maybe LlmAgentRole)
resolveAgentRole (Just roleId) = do
    role <- fetch roleId
    pure (if role.enabled then Just role else Nothing)
resolveAgentRole Nothing =
    query @LlmAgentRole
        |> filterWhere (#isDefault, True)
        |> filterWhere (#enabled, True)
        |> fetchOneOrNothing

-- Named-role lookup for internal pipeline consumers that are not driven by an
-- analysis row (milestone 10: the related-tasks filter uses the
-- "jira-related-filter" role so admins can re-point its prompt template and
-- tool whitelist from the web UI).
resolveAgentRoleByName :: (?modelContext :: ModelContext) => Text -> IO (Maybe LlmAgentRole)
resolveAgentRoleByName name = do
    role <-
        query @LlmAgentRole
            |> filterWhere (#name, name)
            |> fetchOneOrNothing
    pure case role of
        Just found | found.enabled -> Just found
        _ -> Nothing

roleToolNames :: LlmAgentRole -> [Text]
roleToolNames role = fromMaybe [] (parseMaybe parser role.tools)
  where
    parser = Aeson.withArray "tools" \arr -> mapM Aeson.parseJSON (Vector.toList arr)

templateNameForRole :: Maybe LlmAgentRole -> Text
templateNameForRole = maybe "alert_enrichment" (.promptTemplateName)

-- The role's tools array filters the built-in definitions by name; no role
-- means the full built-in set (milestone_8.md §7).
toolsForRole :: Maybe LlmAgentRole -> [Value]
toolsForRole Nothing = toolDefinitions
toolsForRole (Just role) =
    let names = roleToolNames role
     in filter (definitionIncluded names) toolDefinitions
  where
    definitionIncluded names definition = case parseMaybe toolName definition of
        Nothing -> False
        Just name -> name `elem` names
    toolName = Aeson.withObject "tool" \o -> do
        function <- o Aeson..: "function"
        function Aeson..: "name"
