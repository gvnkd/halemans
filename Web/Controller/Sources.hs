module Web.Controller.Sources where

import Web.Controller.Prelude
import Web.View.Sources.Index

instance Controller SourcesController where
    action SourcesAction = do
        sources <- query @Source
            |> orderByAsc #name
            |> fetch
        render IndexView { .. }
