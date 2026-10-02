module Application.Service.AlertScope (
    alertScopeBypassFromSettings,
    scopeNamesFor,
    scopeForUser,
    alertVisibleWith,
    groupsIntersect,
    hostGroupNamesOf,
) where

-- This module is deliberately a LEAF (only Generated.Types + aeson):
-- Notify/Live/Ingest all import it, and anything it imported from the
-- ingest/connector stack would close an import cycle (Notify -> AlertScope
-- -> Ingest -> Notify).

import Application.Service.HostGroups (teamHostGroups)
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.List (nub)
import qualified Data.Text as Text
import Generated.Types
import IHP.Fetch (fetch)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

-- Per-user alert visibility by zabbix host group (Sergey 2026-10-01):
-- a user sees a zabbix-source alert only when its host groups intersect the
-- union of the host groups configured on the user's teams. A user with no
-- team memberships (or empty host groups on all of them) sees NOTHING.
-- Non-zabbix alerts (grafana/alertmanager/generic, sourceless legacy rows)
-- stay visible to everyone; so do Halemans' own internal alerts
-- ("halemans:" fingerprint prefix, e.g. the ungrouped-host warning).

-- | Profile parameter (users.settings.alertScopeBypass): when true the user
-- sees every alert regardless of team host groups.
alertScopeBypassFromSettings :: Aeson.Value -> Bool
alertScopeBypassFromSettings settings =
    fromMaybe False (parseMaybe (Aeson.withObject "settings" (\o -> o Aeson..: "alertScopeBypass")) settings)

-- | Nothing = unrestricted (bypass on); Just names = restricted to alerts
-- whose host groups intersect these names.
scopeNamesFor :: (?modelContext :: ModelContext) => Id User -> IO (Maybe [Text])
scopeNamesFor userId = do
    members <- query @TeamMember |> filterWhere (#userId, userId) |> fetch
    teamRows <- mapM (fetch . (.teamId)) members
    pure (Just (nub (concatMap teamHostGroups teamRows)))

-- | One-call convenience for a concrete user record (agent, API token
-- owner): Nothing = unrestricted (bypass on), Just names = restricted.
scopeForUser :: (?modelContext :: ModelContext) => User -> IO (Maybe [Text])
scopeForUser user
    | alertScopeBypassFromSettings user.settings = pure Nothing
    | otherwise = scopeNamesFor (get #id user)

-- | Pure visibility predicate. isZabbix is the alert's source type (supplied
-- by the caller, which knows how it joined/fetched the source row).
alertVisibleWith :: [Text] -> Bool -> Alert -> Bool
alertVisibleWith names isZabbix alert
    | "halemans:" `Text.isPrefixOf` alert.fingerprint = True
    | not isZabbix = True
    | otherwise = groupsIntersect names (hostGroupNamesOf alert)

groupsIntersect :: [Text] -> [Text] -> Bool
groupsIntersect names groups = not (null (names `intersect` groups))

-- | Zabbix host group names stored on an alert row ([] for legacy rows and
-- non-zabbix alerts). Owned here (not in Helper.Ingest) to keep this module
-- a leaf.
hostGroupNamesOf :: Alert -> [Text]
hostGroupNamesOf alert = fromMaybe [] (parseMaybe Aeson.parseJSON alert.hostGroups)
