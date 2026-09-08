module Application.Service.Llm.Roles
( resolveAgentRole
, roleToolNames
, templateNameForRole
, toolsForRole
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetchOneOrNothing)
import IHP.Fetch (fetch)
import Generated.Types
import Data.Aeson (Value)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import Application.Service.Llm.Tools (toolDefinitions)

-- Agent roles (design_docs/milestone_8.md §7): a role bundles a prompt
-- template name + tool whitelist. Resolution is DB-first like llm_configs
-- (DbConfig pattern): an explicit role id on the analysis wins, otherwise
-- the is_default role applies, otherwise no role → legacy behaviour
-- (alert_enrichment template, full built-in tool set when tools enabled).

resolveAgentRole :: (?modelContext :: ModelContext) => Maybe (Id LlmAgentRole) -> IO (Maybe LlmAgentRole)
resolveAgentRole (Just roleId) = do
    role <- fetch roleId
    pure (if role.enabled then Just role else Nothing)
resolveAgentRole Nothing = query @LlmAgentRole
    |> filterWhere (#isDefault, True)
    |> filterWhere (#enabled, True)
    |> fetchOneOrNothing

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
