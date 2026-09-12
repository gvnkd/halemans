module Application.Service.SecurityHeaders (securityHeaders) where

import IHP.Prelude
import Network.HTTP.Types (ResponseHeaders)
import Network.Wai (Middleware, mapResponseHeaders)

-- Security response headers (milestone 12 §8). CSP became feasible once the
-- last inline <script> blocks and on* handlers moved into static/app.js
-- (milestone 12 §3); style-src keeps 'unsafe-inline' because JS libraries
-- (flatpickr, bootstrap) set element.style at runtime.
securityHeaders :: Middleware
securityHeaders app request respond = app request (respond . mapResponseHeaders (<> headers))

headers :: ResponseHeaders
headers =
    [
        ( "Content-Security-Policy"
        , "default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; connect-src 'self' ws: wss:; font-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'self'"
        )
    , ("X-Content-Type-Options", "nosniff")
    , ("Referrer-Policy", "strict-origin-when-cross-origin")
    , ("X-Frame-Options", "SAMEORIGIN")
    ]
