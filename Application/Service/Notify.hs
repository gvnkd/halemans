module Application.Service.Notify (
    dispatchNotification,
    notifyUsers,
    currentOnCall,
    teamMemberIds,
    resolveRuleTargets,
    throttleKeyFor,
) where

import Application.Pipeline.Grouping (matchAlert, matchExprFromJSON, severityAtLeast)
import Application.Service.Escalation (createTracker)
import Control.Monad (filterM, void)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Vector as Vector
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlQueryTyped, typedSql)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- Phase-2 notification rule engine (design_docs/milestone_2.md §5): replaces
-- the milestone-1 simplified dispatch. All matching enabled rules fire (union
-- of targets), each throttled per (rule, fingerprint) for standalone alerts
-- and per (rule, group_key) for grouped ones (group notifications replace
-- per-alert ones, §6).

-- | Dev override for rule throttles (HALEMANS_NOTIFY_MIN_INTERVAL_SECONDS).
-- When unset, each rule's own throttle_seconds applies.
notifyThrottleOverrideSeconds :: IO (Maybe Int)
notifyThrottleOverrideSeconds = do
    override <- lookupEnv "HALEMANS_NOTIFY_MIN_INTERVAL_SECONDS"
    pure (override >>= readMaybe)

-- | Evaluate enabled notification rules against the alert and enqueue push
-- jobs for the ones that fire. Returns True when at least one rule fired.
dispatchNotification :: (?modelContext :: ModelContext) => Alert -> IO Bool
dispatchNotification alert = do
    rules <-
        query @NotificationRule
            |> filterWhere (#enabled, True)
            |> orderByAsc #position
            |> fetch
    let matching = filter (ruleMatches alert) rules
    fired <- filterM (fmap not . throttled alert) matching
    forM_ fired (fireRule alert)
    pure (not (null fired))

ruleMatches :: Alert -> NotificationRule -> Bool
ruleMatches alert rule =
    severityAtLeast rule.severityThreshold alert.severity
        && matchAlert (matchExprFromJSON rule.match) alert

-- | Throttle anchor (§5): grouped alerts key on the group's group_key,
-- standalone alerts on their fingerprint.
throttleKeyFor :: (?modelContext :: ModelContext) => Alert -> IO Text
throttleKeyFor alert = case alert.groupId of
    Just groupId -> do
        group <- fetch groupId
        pure ("group:" <> group.groupKey)
    Nothing -> pure ("alert:" <> alert.fingerprint)

throttled :: (?modelContext :: ModelContext) => Alert -> NotificationRule -> IO Bool
throttled alert rule = do
    override <- notifyThrottleOverrideSeconds
    let throttleSeconds = fromMaybe rule.throttleSeconds override
    if throttleSeconds <= 0
        then pure False
        else do
            key <- throttleKeyFor alert
            let ruleIdText = tshow (get #id rule) :: Text
                secs = fromIntegral throttleSeconds :: Double
            recent <-
                sqlQueryTyped
                    [typedSql|
                SELECT count(*) FROM alert_events
                WHERE kind = 'notified'
                  AND payload->>'ruleId' = ${ruleIdText}
                  AND payload->>'throttleKey' = ${key}
                  AND created_at > now() - make_interval(secs => ${secs})
            |]
            let recentCount = case recent of
                    (n : _) -> n
                    [] -> 0
            pure (recentCount > 0)

fireRule :: (?modelContext :: ModelContext) => Alert -> NotificationRule -> IO ()
fireRule alert rule = do
    targets <- resolveRuleTargets rule
    key <- throttleKeyFor alert
    recordNotifiedEvent alert rule key targets
    enqueuePush alert rule targets
    when (alert.status == "firing") do
        forM_ rule.escalationPolicyId \policyId ->
            void (createTracker alert rule policyId)

recordNotifiedEvent :: (?modelContext :: ModelContext) => Alert -> NotificationRule -> Text -> [Id User] -> IO ()
recordNotifiedEvent alert rule throttleKey targets = do
    let payload :: Value =
            object
                [ "ruleId" .= tshow (get #id rule)
                , "rule" .= rule.name
                , "throttleKey" .= throttleKey
                , "targets" .= map (tshow :: Id User -> Text) targets
                ]
    _ <-
        newRecord @AlertEvent
            |> set #alertId (get #id alert)
            |> set #userId Nothing
            |> set #kind "notified"
            |> set #payload payload
            |> createRecord
    pure ()

enqueuePush :: (?modelContext :: ModelContext) => Alert -> NotificationRule -> [Id User] -> IO ()
enqueuePush alert rule targets = do
    let targetJson = Aeson.toJSON (map (tshow :: Id User -> Text) targets)
    _ <-
        newRecord @PushNotificationJob
            |> set #alertId (get #id alert)
            |> set #targetUserIds (Just targetJson)
            |> set #ruleId (Just (get #id rule))
            |> set #groupId alert.groupId
            |> createRecord
    pure ()

-- | Direct push to a fixed user set, used by EscalationJob (§6): no rule
-- evaluation, no throttle — the schedule itself is the pacing.
notifyUsers :: (?modelContext :: ModelContext) => Alert -> [Id User] -> IO ()
notifyUsers alert targets = do
    _ <-
        newRecord @PushNotificationJob
            |> set #alertId (get #id alert)
            |> set #targetUserIds (Just (Aeson.toJSON (map (tshow :: Id User -> Text) targets)))
            |> set #ruleId Nothing
            |> set #groupId alert.groupId
            |> createRecord
    pure ()

-- | Rule target resolution (§5): a user target resolves to that user; a team
-- target resolves to currentOnCall, falling back to all team members when no
-- schedule row exists (§7).
resolveRuleTargets :: (?modelContext :: ModelContext) => NotificationRule -> IO [Id User]
resolveRuleTargets rule = case (rule.teamId, rule.userId) of
    (_, Just userId) -> pure [userId]
    (Just teamId, Nothing) -> do
        onCall <- currentOnCall teamId
        case onCall of
            Just userId -> pure [userId]
            Nothing -> teamMemberIds teamId
    (Nothing, Nothing) -> pure []

-- | On-call stub (milestone_2.md §7, highlevel §18): returns the first member
-- of the team's schedule; Nothing when the team has no schedule row.
currentOnCall :: (?modelContext :: ModelContext) => Id Team -> IO (Maybe (Id User))
currentOnCall teamId = do
    schedule <-
        query @OnCallSchedule
            |> filterWhere (#teamId, teamId)
            |> fetchOneOrNothing
    pure case schedule of
        Nothing -> Nothing
        Just schedule -> case scheduleMembers schedule of
            (firstMember : _) -> Just firstMember
            [] -> Nothing

scheduleMembers :: OnCallSchedule -> [Id User]
scheduleMembers schedule =
    fromMaybe [] (parseMaybe (Aeson.withArray "members" (pure . mapMaybe parseMember . Vector.toList)) schedule.members)
  where
    parseMember (Aeson.String raw) = Just (textToId raw)
    parseMember _ = Nothing

teamMemberIds :: (?modelContext :: ModelContext) => Id Team -> IO [Id User]
teamMemberIds teamId = do
    members <-
        query @TeamMember
            |> filterWhere (#teamId, teamId)
            |> fetch
    pure (map (.userId) members)
