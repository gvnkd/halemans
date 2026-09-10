module Application.Service.Groups
( assignGroup
, regroupAlert
, recomputeGroupRollup
, publishGroupUpdate
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Generated.Types
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Control.Monad (void)
import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText, matchExprFromJSON, matchAlert, renderTemplate, severityRank, ruleReferencesFacets)

-- AlertGroup membership + rollup maintenance (design_docs/milestone_2.md
-- §3 step 5, §4). Rules are evaluated in position order, first match wins;
-- no match leaves the alert standalone.

-- | A new group's environment follows the alert's EFFECTIVE env (facet
-- override wins), upserting the inventory row when the name is facet-only.
-- Local copy of Ingest.upsertEnvironment — Ingest imports this module, so
-- importing it back would cycle.
effectiveEnvironmentRef :: (?modelContext :: ModelContext) => Alert -> IO (Maybe (Id Environment))
effectiveEnvironmentRef alert = forM (effectiveFieldText FieldEnv alert) \name -> do
    existing <- query @Environment
        |> filterWhere (#name, name)
        |> fetchOneOrNothing
    case existing of
        Just environment -> pure (get #id environment)
        Nothing -> do
            created <- newRecord @Environment
                |> set #name name
                |> createRecord
            pure (get #id created)

-- | Evaluate grouping rules for a freshly created alert. Returns the updated
-- alert (group_id + grouped_by_version set) or the alert unchanged. Alerts
-- without any subject (env/host/service all absent) are never grouped: their
-- rendered key would be all dashes and collapse unrelated alerts together.
assignGroup :: (?modelContext :: ModelContext) => Alert -> IO Alert
assignGroup alert
    | isNothing (effectiveFieldText FieldEnv alert)
        && isNothing (effectiveFieldText FieldHost alert)
        && isNothing (effectiveFieldText FieldService alert) = pure alert
assignGroup alert = do
    rules <- query @GroupingRule
        |> filterWhere (#enabled, True)
        |> orderByAsc #position
        |> fetch
    case find (\rule -> matchAlert (matchExprFromJSON rule.match) alert) rules of
        Nothing -> pure alert
        Just rule -> do
            let key = renderTemplate rule.groupKeyTemplate alert
            group <- query @AlertGroup
                |> filterWhere (#groupKey, key)
                |> fetchOneOrNothing
            groupRef <- case group of
                Just group -> pure (get #id group)
                Nothing -> do
                    environmentRef <- effectiveEnvironmentRef alert
                    created <- newRecord @AlertGroup
                        |> set #groupKey key
                        |> set #title key
                        |> set #environmentId environmentRef
                        |> createRecord
                    pure (get #id created)
            updated <- alert
                |> set #groupId (Just groupRef)
                |> set #groupedByVersion (Just rule.version)
                |> updateRecord
            void (recomputeGroupRollup groupRef)
            pure updated

-- | Regroup replay after enrichment (milestone_9.md §7): assignGroup runs at
-- ingest when attr facets are still absent. Once EnrichAlertJob materializes
-- facets, alerts matched by facet-referencing rules move into (or between)
-- groups. Conservative: an alert that no longer matches any rule keeps its
-- group (same "rule edits don't ungroup" semantics as version bumps).
regroupAlert :: (?modelContext :: ModelContext) => Alert -> IO Alert
regroupAlert alert = do
    rules <- query @GroupingRule
        |> filterWhere (#enabled, True)
        |> orderByAsc #position
        |> fetch
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
                    else do
                        group <- query @AlertGroup
                            |> filterWhere (#groupKey, key)
                            |> fetchOneOrNothing
                        groupRef <- case group of
                            Just group -> pure (get #id group)
                            Nothing -> do
                                environmentRef <- effectiveEnvironmentRef alert
                                created <- newRecord @AlertGroup
                                    |> set #groupKey key
                                    |> set #title key
                                    |> set #environmentId environmentRef
                                    |> createRecord
                                pure (get #id created)
                        updated <- alert
                            |> set #groupId (Just groupRef)
                            |> set #groupedByVersion (Just rule.version)
                            |> updateRecord
                        forM_ alert.groupId \oldGroupId -> void (recomputeGroupRollup oldGroupId)
                        void (recomputeGroupRollup groupRef)
                        pure updated

-- | Recompute worst severity / member count / rollup status from the current
-- members. Group resolves when every member is resolved or closed (§3).
recomputeGroupRollup :: (?modelContext :: ModelContext) => Id AlertGroup -> IO AlertGroup
recomputeGroupRollup groupId = do
    group <- fetch groupId
    members <- query @Alert
        |> filterWhere (#groupId, Just groupId)
        |> fetch
    let count = length members
        worst = fromMaybe "warning" (maximumByMay (\a b -> compare (severityRank a) (severityRank b)) (map (.severity) members))
        live = filter (\alert -> alert.status `notElem` ["resolved", "closed"]) members
        rollupStatus
            | any (\alert -> alert.status == "firing") live = "firing"
            | not (null live) = "ack"
            | otherwise = "resolved"
    now <- getCurrentTime
    let wasResolved = group.status == "resolved"
        resolvedAt = if rollupStatus == "resolved" && not wasResolved then Just now else (if rollupStatus == "resolved" then group.resolvedAt else Nothing)
    updated <- group
        |> set #worstSeverity worst
        |> set #memberCount count
        |> set #status rollupStatus
        |> set #resolvedAt resolvedAt
        |> updateRecord
    publishGroupUpdate updated
    pure updated

maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumByMay _ [] = Nothing
maximumByMay cmp (x:xs) = Just (foldl' (\best y -> if cmp y best == GT then y else best) x xs)

-- | Websocket fan-out for group mutations (§9): same halemans_events
-- channel, fragment target group-row.
publishGroupUpdate :: (?modelContext :: ModelContext) => AlertGroup -> IO ()
publishGroupUpdate group = do
    envName <- forM group.environmentId \environmentId -> do
        environment <- fetch environmentId
        pure environment.name
    let payload :: Text
        payload = cs (Aeson.encode (object
            [ "groupId" .= get #id group
            , "groupKey" .= group.groupKey
            , "kind" .= ("group" :: Text)
            , "env" .= envName
            , "status" .= group.status
            , "worstSeverity" .= group.worstSeverity
            , "memberCount" .= group.memberCount
            ]))
    _ <- sqlQueryTyped [typedSql| SELECT 1 WHERE pg_notify('halemans_events', ${payload}) IS NULL |] :: IO [Int]
    pure ()
