module Application.Helper.View where

import IHP.ViewPrelude
import Data.Time.Format (formatTime, defaultTimeLocale)

-- Here you can add functions which are available in all your views

-- Timestamps render as <time datetime=…> with a microsecond-free UTC
-- fallback; static/app.js rewrites them to the browser's local timezone
-- with an offset label (UTC+4) on load and for WS-injected fragments.
utcTimeHtml :: UTCTime -> Html
utcTimeHtml t = [hsx|<time class="utc-time" datetime={iso}>{fallback}</time>|]
    where
        iso :: Text
        iso = cs (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t)
        fallback :: Text
        fallback = cs (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t) <> " UTC"

maybeUtcTimeHtml :: Maybe UTCTime -> Html
maybeUtcTimeHtml = maybe mempty utcTimeHtml