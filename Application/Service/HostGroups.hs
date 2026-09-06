module Application.Service.HostGroups
( HostGroupScope (..)
, hostGroupScope
, teamHostGroups
, teamHostGroupNames
, parseHostGroupsInput
, hostGroupsToJson
, replaceHostGroupCache
) where

import IHP.Prelude
import IHP.ModelSupport (ModelContext, Id' (..), newRecord, createRecord)
import IHP.HaskellSupport (set)
import IHP.TypedSql (sqlExecTyped, typedSql)
import Generated.Types
import Application.Connector.Zabbix (ZabbixGroup (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as Text
import Data.List (nub)
import Control.Monad (void)

-- | Per-source fetch scope (source.config.hostGroupScope): "all" (default)
-- ingests every zabbix trigger event; "teams" restricts event.get to the
-- union of host groups configured on teams.
data HostGroupScope = ScopeAll | ScopeTeams
    deriving (Eq, Show)

hostGroupScope :: Source -> HostGroupScope
hostGroupScope source =
    case scopeText of
        Just "teams" -> ScopeTeams
        _ -> ScopeAll
  where
    scopeText :: Maybe Text
    scopeText = parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: "hostGroupScope")) source.config

-- | Zabbix host group names configured on one team (teams.host_groups jsonb
-- array of strings).
teamHostGroups :: Team -> [Text]
teamHostGroups team = fromMaybe [] (parseMaybe Aeson.parseJSON team.hostGroups)

-- | Union of host group names across teams (duplicates removed).
teamHostGroupNames :: [Team] -> [Text]
teamHostGroupNames = nub . concatMap teamHostGroups

-- | Form input: comma- and/or newline-separated names.
parseHostGroupsInput :: Text -> [Text]
parseHostGroupsInput input =
    [name | part <- Text.split (\c -> c == ',' || c == '\n') input
          , let name = Text.strip part
          , name /= ""]

hostGroupsToJson :: [Text] -> Aeson.Value
hostGroupsToJson = Aeson.toJSON

-- | Replace one source's zabbix_host_groups cache rows with the given
-- listing. Shared by the manual sync action (fresh hostgroup.get result) and
-- provisioning (hostGroupsFile import). Returns the row count.
replaceHostGroupCache :: (?modelContext :: ModelContext) => Id' "sources" -> [ZabbixGroup] -> IO Int
replaceHostGroupCache sourceId groups = do
    void $ sqlExecTyped [typedSql| DELETE FROM zabbix_host_groups WHERE source_id = ${sourceId} |]
    forM_ groups \group -> do
        _ <- newRecord @ZabbixHostGroup
            |> set #sourceId sourceId
            |> set #name group.groupName
            |> set #groupId group.groupId
            |> createRecord
        pure ()
    pure (length groups)
