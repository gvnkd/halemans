module Main where

import Test.Hspec
import IHP.Prelude

-- Import your test specs here:
import qualified Test.StateMachineSpec
import qualified Test.BlackoutsSpec
import qualified Test.PushSpec

main :: IO ()
main = hspec do
    Test.StateMachineSpec.spec
    Test.BlackoutsSpec.spec
    Test.PushSpec.spec
