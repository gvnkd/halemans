module Main where

import Test.Hspec
import IHP.Prelude

-- Import your test specs here:
import qualified Test.StateMachineSpec
import qualified Test.BlackoutsSpec
import qualified Test.PushSpec
import qualified Test.GroupingSpec
import qualified Test.EscalationSpec
import qualified Test.AlertmanagerSpec

main :: IO ()
main = hspec do
    Test.StateMachineSpec.spec
    Test.BlackoutsSpec.spec
    Test.PushSpec.spec
    Test.GroupingSpec.spec
    Test.EscalationSpec.spec
    Test.AlertmanagerSpec.spec
