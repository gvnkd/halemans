module Web.Controller.Live where

import Web.Controller.Prelude
import IHP.WebSocket
import Application.Service.Live (liveBroadcastLoop, ensureBroadcaster)

-- /ws live updates (milestone_1.md §7). Session-cookie authenticated via the
-- auth middleware; unauthenticated connects are rejected with a close frame.
instance WSApp LiveController where
    initialState = LiveController

    run = do
        case currentUserOrNothing of
            Nothing -> sendTextData ("{\"error\":\"not authenticated\"}" :: Text)
            Just _user -> do
                ensureBroadcaster
                liveBroadcastLoop
