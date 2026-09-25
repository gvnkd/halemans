module Application.Service.Turbolinks (turbolinksRedirectLocation) where

import qualified Data.ByteString.Char8 as ByteString
import IHP.Prelude
import Network.HTTP.Types (statusCode)
import Network.Wai (Middleware, Response, mapResponseHeaders, modifyResponse, rawPathInfo, rawQueryString, requestMethod, responseHeaders, responseStatus)

-- Turbolinks XHR visits cannot see a 302 target. Turbolinks learns the final
-- URL from the Turbolinks-Location RESPONSE HEADER (turbolinks.js
-- requestLoaded) — but XMLHttpRequest follows redirects transparently and
-- exposes only the FINAL response's headers, so a header set on the 302 is
-- invisible. Without it, visiting /alerts with stored filter prefs ended
-- with plain /alerts in the address bar/history (the prefs-replay redirect
-- target was invisible), while a full reload followed the redirect natively
-- and showed the params.
--
-- Rails solves this by stamping Turbolinks-Location on the rendered 200 page
-- after a redirect; we do the equivalent generically: every 2xx HTML
-- response carries Turbolinks-Location set to its own request URL (a
-- host-absolute path+query, resolved client-side against the document
-- origin, proxy-safe). For non-redirected visits the value equals the URL
-- turbolinks already pushed, so its replaceState is a no-op; for redirected
-- visits it corrects the address bar to the final URL. The mirror onto 3xx
-- Location is kept for completeness (some turbolinks plugins read it).
turbolinksRedirectLocation :: Middleware
turbolinksRedirectLocation app request respond = app request (respond . addHeader)
  where
    addHeader response
        | statusCode (responseStatus response) `elem` [301, 302, 303, 307, 308] =
            case lookup "Location" hdrs of
                Just location
                    | isNothing (lookup "Turbolinks-Location" hdrs) ->
                        mapResponseHeaders (<> [("Turbolinks-Location", location)]) response
                _ -> response
        | isGetMethod && statusCode (responseStatus response) `elem` [200 .. 299] && isHtml response =
            case lookup "Turbolinks-Location" hdrs of
                Nothing ->
                    mapResponseHeaders (<> [("Turbolinks-Location", ownUrl)]) response
                _ -> response
        | otherwise = response
      where
        hdrs = responseHeaders response
        isGetMethod = requestMethod request == "GET"
        ownUrl = rawPathInfo request <> if null (rawQueryString request) then "" else rawQueryString request

isHtml :: Response -> Bool
isHtml response = case lookup "Content-Type" (responseHeaders response) of
    Just contentType -> "text/html" `ByteString.isPrefixOf` contentType
    Nothing -> False
