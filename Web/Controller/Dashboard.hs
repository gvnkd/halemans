module Web.Controller.Dashboard where

import Web.Controller.Prelude
import Web.View.Dashboard.Index

instance Controller DashboardController where
    beforeAction = ensureIsUser

    action DashboardAction = do
        (cards, unassigned) <- computeEnvCards
        render IndexView { cards, unassigned }
