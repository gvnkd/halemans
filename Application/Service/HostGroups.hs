module Application.Service.HostGroups
( HostGroupScope (..)
, hostGroupScope
, teamHostGroups
, teamHostGroupNames
, parseHostGroupsInput
, hostGroupsToJson
) where

import IHP.Prelude
import Generated.Types
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as Text
import Data.List (nub)

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
