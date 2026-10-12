module Test.Integration.GroupsSpec (spec) where

import Application.Service.Groups (ApplyGroupingSummary (..), applyGroupingRulesWith)
import Control.Exception (finally)
import Control.Monad (void)
import Data.Aeson (object, (.=))
import Data.UUID.V4 (nextRandom)
import Generated.Types
import IHP.Fetch (fetch)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import Test.Hspec

-- applyGroupingRules (the admin "Apply rules" replay): full re-apply over
-- EVERY alert. The shared random-order suite DB carries alerts and enabled
-- catch-all rules from other examples, so: the rule's match is pinned to a
-- unique env (only our alerts can match), all other enabled rules are
-- sidelined for the duration (DashboardsSpec pattern, restored in finally),
-- and counts are asserted as >= / == 0 rather than exact (other alerts may
-- be regrouped/ungrouped by our run too).
spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = describe "applyGroupingRules" do
    it "groups pre-existing ungrouped alerts, is idempotent, and ungroups on a later full re-apply" do
        tag <- tshow <$> nextRandom
        let envName = "itest-apply-rules-" <> tag
        otherRules <- query @GroupingRule |> filterWhere (#enabled, True) |> fetch
        forM_ otherRules \other -> void (other |> set #enabled False |> updateRecord)
        rule <-
            newRecord @GroupingRule
                |> set #name ("itest-apply-rules-" <> tag)
                |> set #position 9000
                |> set #enabled True
                |> set #match (object ["fields" .= object ["env" .= envName]])
                |> set #groupKeyTemplate "{title} on {host}"
                |> set #createdBy Nothing
                |> createRecord
        let restore = do
                deleteRecord rule
                forM_ otherRules \other -> void (other |> set #enabled True |> updateRecord)
        flip finally restore do
            a1 <-
                newRecord @Alert
                    |> set #fingerprint ("itest-ar-fp1-" <> tag)
                    |> set #title "Disk full"
                    |> set #status "firing"
                    |> set #env (Just envName)
                    |> set #host (Just "db-01")
                    |> createRecord
            a2 <-
                newRecord @Alert
                    |> set #fingerprint ("itest-ar-fp2-" <> tag)
                    |> set #title "Disk full"
                    |> set #status "firing"
                    |> set #env (Just envName)
                    |> set #host (Just "db-01")
                    |> createRecord
            a3 <-
                newRecord @Alert
                    |> set #fingerprint ("itest-ar-fp3-" <> tag)
                    |> set #title "CPU high"
                    |> set #status "firing"
                    |> set #env (Just envName)
                    |> set #host (Just "db-02")
                    |> createRecord
            -- Page size 2 forces the keyset walk across multiple pages
            -- (3+ alerts in the shared suite DB), covering the cursor
            -- advance and per-page folds, not just the single-page path.
            summary1 <- applyGroupingRulesWith 2
            summary1.agsRegrouped `shouldSatisfy` (\n -> n >= (3 :: Int))
            a1' <- fetch (get #id a1)
            a2' <- fetch (get #id a2)
            a3' <- fetch (get #id a3)
            a1'.groupId `shouldBe` a2'.groupId
            a3'.groupId `shouldNotBe` a1'.groupId
            a1'.groupedByVersion `shouldBe` Just rule.version
            case a1'.groupId of
                Nothing -> expectationFailure "alert not grouped by apply"
                Just groupId -> do
                    group <- fetch groupId
                    group.groupKey `shouldBe` "Disk full on db-01"
                    group.memberCount `shouldBe` 2
            case a3'.groupId of
                Nothing -> expectationFailure "alert not grouped by apply"
                Just groupId -> do
                    group <- fetch groupId
                    group.groupKey `shouldBe` "CPU high on db-02"
            -- unchanged alerts are not touched on a second run
            summary2 <- applyGroupingRulesWith 2
            summary2.agsRegrouped `shouldBe` 0
            -- full re-apply: with no enabled rule matching, groups are cleared
            _ <- rule |> set #enabled False |> updateRecord
            summary3 <- applyGroupingRulesWith 2
            summary3.agsUngrouped `shouldSatisfy` (\n -> n >= (3 :: Int))
            a1'' <- fetch (get #id a1)
            a1''.groupId `shouldBe` Nothing
            a1''.groupedByVersion `shouldBe` Nothing
