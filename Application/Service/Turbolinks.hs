module Application.Service.Turbolinks (turbolinksRedirectLocation) where

import IHP.Prelude
import Network.HTTP.Types (statusCode)
import Network.Wai (Middleware, mapResponseHeaders, modifyResponse, responseHeaders, responseStatus)

-- Turbolinks XHR visits cannot see a 302 target: IHP's redirects send a bare
-- Location header, and turbolinks only learns the final URL from the
-- Turbolinks-Location response header (turbolinks.js
-- requestCompletedWithResponse). Without it, visiting /alerts with stored
-- filter prefs ended with plain /alerts in the address bar/history (the
-- prefs-replay redirect target was invisible), while a full reload followed
-- the redirect natively and showed the params. Mirroring Location onto
-- Turbolinks-Location lets turbolinks follow the redirect client-side.
turbolinksRedirectLocation :: Middleware
turbolinksRedirectLocation = modifyResponse addHeader
  where
    addHeader response
        | statusCode (responseStatus response) `elem` [301, 302, 303, 307, 308] =
            case lookup "Location" hdrs of
                Just location
                    | isNothing (lookup "Turbolinks-Location" hdrs) ->
                        mapResponseHeaders (<> [("Turbolinks-Location", location)]) response
                _ -> response
        | otherwise = response
      where
        hdrs = responseHeaders response
