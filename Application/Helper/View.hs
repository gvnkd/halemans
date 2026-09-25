module Application.Helper.View (
    module Application.Helper.View,
    module Application.Helper.I18n,
) where

import Application.Helper.I18n
import qualified CMark
import Data.Time.Format (defaultTimeLocale, formatTime)
import IHP.ViewPrelude

-- Here you can add functions which are available in all your views

-- Document <title> ("Section · Halemans"): browser tabs and history entries
-- were all "Halemans" because no view set a title. Views opt in via
-- `beforeRender _ = setPageTitle (tr "...")`.
setPageTitle :: (?request :: Request) => Text -> IO ()
setPageTitle title = setTitle (title <> " · Halemans")

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

-- | Maybe timestamp with a plain-text fallback ("never", "on schedule").
utcTimeOrHtml :: Text -> Maybe UTCTime -> Html
utcTimeOrHtml fallback = maybe [hsx|{fallback}|] utcTimeHtml

-- LLM analysis markdown renders server-side via cmark (milestone_4.md §7).
-- Safe mode suppresses raw HTML and dangerous link URLs — the text is model
-- output and must never inject markup into the card.
renderMarkdownText :: Text -> Text
renderMarkdownText = CMark.commonmarkToHtml [CMark.optSafe]

markdownHtml :: Text -> Html
markdownHtml = preEscapedToHtml . renderMarkdownText

-- Badge for provision-managed config items: shown in admin list views next
-- to the item name; the controller side rejects edits via ensureNotProtected.
protectedBadgeHtml :: Bool -> Html
protectedBadgeHtml itemProtected =
    if itemProtected
        then [hsx|<span class="badge bg-secondary" data-testid="protected-badge" title={tr "Managed by provisioning (HALEMANS_PROVISION_CONFIG); edit the provision file to change"}>{tr "protected"}</span>|]
        else mempty
