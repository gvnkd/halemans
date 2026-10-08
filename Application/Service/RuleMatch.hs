module Application.Service.RuleMatch (
    ruleMatches,
    ruleInScope,
) where

import Application.Pipeline.Grouping (matchAlert, matchExprFromJSON, severityAtLeast)
import Application.Service.AlertScope (alertVisibleWith)
import Application.Service.HostGroups (teamHostGroups)
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

-- The notification-rule predicates shared by the dispatch engine (Notify)
-- and the Mattermost channel banner (Banner). Extracted from Notify so the
-- banner — imported from the job worker — does not pull in Notify itself
-- (Notify -> Job.Mattermost -> Banner would close a cycle). All imports are
-- leaves (Grouping/AlertScope/HostGroups).

-- | The rule's severity threshold AND match expression against the alert —
-- the same predicate dispatchNotification applies.
ruleMatches :: Alert -> NotificationRule -> Bool
ruleMatches alert rule =
    severityAtLeast rule.severityThreshold alert.severity
        && matchAlert (matchExprFromJSON rule.match) alert

-- | Team-targeted rules only fire for alerts the team could SEE: a zabbix
-- alert must intersect the team's host groups (the same AlertScope predicate
-- as the UI), non-zabbix and halemans-internal alerts pass. A team with no
-- host groups therefore gets no zabbix notifications — mirroring its empty
-- alert view. Rules without a team target keep the global behavior.
ruleInScope :: (?modelContext :: ModelContext) => Alert -> NotificationRule -> IO Bool
ruleInScope alert rule = case rule.teamId of
    Nothing -> pure True
    Just teamId -> do
        teamOrNothing <- fetchOneOrNothing teamId
        case teamOrNothing of
            Nothing -> pure True
            Just team -> do
                isZabbix <- case alert.sourceId of
                    Nothing -> pure False
                    Just sourceId -> maybe False (\source -> source.type_ == "zabbix") <$> fetchOneOrNothing sourceId
                pure (alertVisibleWith (teamHostGroups team) isZabbix alert)
