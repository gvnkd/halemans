module Application.Service.Groups (
    assignGroup,
    regroupAlert,
    applyGroupingRules,
    ApplyGroupingSummary (..),
    recomputeGroupRollup,
    publishGroupUpdate,
) where

import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText, matchAlert, matchExprFromJSON, renderTemplate, ruleReferencesFacets, severityRank)
import Control.Monad (foldM, void)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlQueryTyped, typedSql)

-- AlertGroup membership + rollup maintenance (design_docs/milestone_2.md
-- §3 step 5, §4). Rules are evaluated in position order, first match wins;
-- no match leaves the alert standalone.

-- | A new group's environment follows the alert's EFFECTIVE env (facet
-- override wins), upserting the inventory row when the name is facet-only.
-- Local copy of Ingest.upsertEnvironment — Ingest imports this module, so
-- importing it back would cycle.
effectiveEnvironmentRef :: (?modelContext :: ModelContext) => Alert -> IO (Maybe (Id Environment))
effectiveEnvironmentRef alert = forM (effectiveFieldText FieldEnv alert) \name -> do
    existing <-
        query @Environment
            |> filterWhere (#name, name)
            |> fetchOneOrNothing
    case existing of
        Just environment -> pure (get #id environment)
        Nothing -> do
            created <-
                newRecord @Environment
                    |> set #name name
                    |> createRecord
            pure (get #id created)

-- | Evaluate grouping rules for a freshly created alert. Returns the updated
-- alert (group_id + grouped_by_version set) or the alert unchanged. Alerts
-- without any subject (env/host/service all absent) are never grouped: their
-- rendered key would be all dashes and collapse unrelated alerts together.
assignGroup :: (?modelContext :: ModelContext) => Alert -> IO Alert
assignGroup alert
    | not (groupable alert) = pure alert
assignGroup alert = do
    rules <- enabledGroupingRules
    case find (\rule -> matchAlert (matchExprFromJSON rule.match) alert) rules of
        Nothing -> pure alert
        Just rule -> moveToGroup alert rule

-- | Alerts with no env/host/service subject are never grouped: their
-- rendered key would be all dashes and collapse unrelated alerts together.
groupable :: Alert -> Bool
groupable alert =
    isJust (effectiveFieldText FieldEnv alert)
        || isJust (effectiveFieldText FieldHost alert)
        || isJust (effectiveFieldText FieldService alert)

enabledGroupingRules :: (?modelContext :: ModelContext) => IO [GroupingRule]
enabledGroupingRules =
    query @GroupingRule
        |> filterWhere (#enabled, True)
        |> orderByAsc #position
        |> fetch

-- | Group row for the rendered key, created on demand. Does NOT recompute
-- rollups — batch callers (applyGroupingRules) collect the groups they touch
-- and recompute each ONCE at the end instead of once per member.
ensureGroupForKey :: (?modelContext :: ModelContext) => Alert -> Text -> IO (Id AlertGroup)
ensureGroupForKey alert key = do
    group <-
        query @AlertGroup
            |> filterWhere (#groupKey, key)
            |> fetchOneOrNothing
    case group of
        Just group -> pure (get #id group)
        Nothing -> do
            environmentRef <- effectiveEnvironmentRef alert
            created <-
                newRecord @AlertGroup
                    |> set #groupKey key
                    |> set #title key
                    |> set #environmentId environmentRef
                    |> createRecord
            pure (get #id created)

-- | Move an alert into the group for the key rendered by the rule (created
-- on demand), setting group_id/grouped_by_version and recomputing the old
-- and new groups' rollups. Shared by assignGroup and regroupAlert.
moveToGroup :: (?modelContext :: ModelContext) => Alert -> GroupingRule -> IO Alert
moveToGroup alert rule = do
    groupRef <- ensureGroupForKey alert (renderTemplate rule.groupKeyTemplate alert)
    updated <-
        alert
            |> set #groupId (Just groupRef)
            |> set #groupedByVersion (Just rule.version)
            |> updateRecord
    forM_ alert.groupId \oldGroupId -> void (recomputeGroupRollup oldGroupId)
    void (recomputeGroupRollup groupRef)
    pure updated

-- | Regroup replay after enrichment (milestone_9.md §7): assignGroup runs at
-- ingest when attr facets are still absent. Once EnrichAlertJob materializes
-- facets, alerts matched by facet-referencing rules move into (or between)
-- groups. Conservative: an alert that no longer matches any rule keeps its
-- group (same "rule edits don't ungroup" semantics as version bumps).
regroupAlert :: (?modelContext :: ModelContext) => Alert -> IO Alert
regroupAlert alert = do
    rules <- enabledGroupingRules
    if not (any ruleReferencesFacets rules)
        then pure alert
        else case find (\rule -> matchAlert (matchExprFromJSON rule.match) alert) rules of
            Nothing -> pure alert
            Just rule -> do
                let key = renderTemplate rule.groupKeyTemplate alert
                currentGroup <- mapM fetch alert.groupId
                let unchanged = case currentGroup of
                        Just group -> group.groupKey == key && alert.groupedByVersion == Just rule.version
                        Nothing -> False
                if unchanged
                    then pure alert
                    else moveToGroup alert rule

data ApplyGroupingSummary = ApplyGroupingSummary
    { agsRegrouped :: Int
    -- ^ Alerts moved into a (new or existing) group.
    , agsUngrouped :: Int
    -- ^ Alerts whose group was cleared (no enabled rule matched).
    }

-- | Manual "apply rules" replay behind the admin button: re-evaluate EVERY
-- alert against the enabled rules. Full re-apply semantics — an alert
-- matching no rule is UNGROUPED (unlike ingest/regroupAlert, which never
-- ungroup): the admin explicitly asked for the current rule set to become
-- the membership truth. Grouped-but-unchanged alerts (same key, same rule
-- version) are not touched. Empty groups are left in place (the admin alert
-- purge drops them).
--
-- PERFORMANCE (prod 504 fix): one transaction, and rollups are BATCHED —
-- recomputeGroupRollup scans a group's members, so running it per moved
-- alert is quadratic on busy groups (first apply on a big table blew the
-- nginx proxy timeout; the second was instant because everything was
-- already unchanged). Step only updates alert rows and collects the touched
-- group ids; each group's rollup is recomputed ONCE at the end.
applyGroupingRules :: (?modelContext :: ModelContext) => IO ApplyGroupingSummary
applyGroupingRules = withTransaction do
    rules <- enabledGroupingRules
    alerts <- query @Alert |> fetch
    (summary, touched) <- foldM (step rules) (ApplyGroupingSummary 0 0, Set.empty) alerts
    mapM_ (void . recomputeGroupRollup) touched
    pure summary
  where
    step rules (acc, touched) alert =
        case find (\rule -> matchAlert (matchExprFromJSON rule.match) alert) rules of
            Just rule | groupable alert -> do
                let key = renderTemplate rule.groupKeyTemplate alert
                currentGroup <- mapM fetch alert.groupId
                let unchanged = case currentGroup of
                        Just group -> group.groupKey == key && alert.groupedByVersion == Just rule.version
                        Nothing -> False
                if unchanged
                    then pure (acc, touched)
                    else do
                        groupRef <- ensureGroupForKey alert key
                        _ <-
                            alert
                                |> set #groupId (Just groupRef)
                                |> set #groupedByVersion (Just rule.version)
                                |> updateRecord
                        pure
                            ( acc{agsRegrouped = acc.agsRegrouped + 1}
                            , touched <> Set.fromList (maybe [] pure alert.groupId) <> Set.singleton groupRef
                            )
            _ -> case alert.groupId of
                Nothing -> pure (acc, touched)
                Just oldGroupId -> do
                    _ <-
                        alert
                            |> set #groupId Nothing
                            |> set #groupedByVersion Nothing
                            |> updateRecord
                    pure (acc{agsUngrouped = acc.agsUngrouped + 1}, Set.insert oldGroupId touched)

-- | Recompute worst severity / member count / rollup status from the current
-- members. Group resolves when every member is resolved or closed (§3).
recomputeGroupRollup :: (?modelContext :: ModelContext) => Id AlertGroup -> IO AlertGroup
recomputeGroupRollup groupId = do
    group <- fetch groupId
    members <-
        query @Alert
            |> filterWhere (#groupId, Just groupId)
            |> fetch
    let count = length members
        worst = fromMaybe "warning" (maximumByMay (\a b -> compare (severityRank a) (severityRank b)) (map (.severity) members))
        live = filter (\alert -> alert.status `notElem` ["resolved", "closed"]) members
        rollupStatus
            | any (\alert -> alert.status == "firing") live = "firing"
            | any (\alert -> alert.status == "stalled") live = "stalled"
            | not (null live) = "ack"
            | otherwise = "resolved"
    now <- getCurrentTime
    let wasResolved = group.status == "resolved"
        resolvedAt = if rollupStatus == "resolved" && not wasResolved then Just now else (if rollupStatus == "resolved" then group.resolvedAt else Nothing)
    updated <-
        group
            |> set #worstSeverity worst
            |> set #memberCount count
            |> set #status rollupStatus
            |> set #resolvedAt resolvedAt
            |> updateRecord
    publishGroupUpdate updated
    pure updated

maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumByMay _ [] = Nothing
maximumByMay cmp (x : xs) = Just (foldl' (\best y -> if cmp y best == GT then y else best) x xs)

-- | Websocket fan-out for group mutations (§9): same halemans_events
-- channel, fragment target group-row.
publishGroupUpdate :: (?modelContext :: ModelContext) => AlertGroup -> IO ()
publishGroupUpdate group = do
    envName <- forM group.environmentId \environmentId -> do
        environment <- fetch environmentId
        pure environment.name
    let payload :: Text
        payload =
            cs
                ( Aeson.encode
                    ( object
                        [ "groupId" .= get #id group
                        , "groupKey" .= group.groupKey
                        , "kind" .= ("group" :: Text)
                        , "env" .= envName
                        , "status" .= group.status
                        , "worstSeverity" .= group.worstSeverity
                        , "memberCount" .= group.memberCount
                        ]
                    )
                )
    _ <- sqlQueryTyped [typedSql| SELECT 1 WHERE pg_notify('halemans_events', ${payload}) IS NULL |] :: IO [Int]
    pure ()
