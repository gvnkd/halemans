module Main where

import IHP.Prelude

import Config
import IHP.FrameworkConfig
import IHP.RouterSupport
import qualified IHP.Server
import Web.FrontController
import Web.Types

instance FrontController RootApplication where
    controllers =
        [ mountFrontController WebApplication
        ]

main :: IO ()
main = IHP.Server.run config
